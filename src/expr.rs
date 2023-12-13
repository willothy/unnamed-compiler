use std::{borrow::Cow, collections::BTreeMap};

use chumsky::{
    primitive::{choice, just, none_of, one_of},
    recursive::recursive,
    text::{ident, keyword},
    IterParser, Parser,
};

use crate::{
    stmt::{BinaryOp, Stmt, UnaryOp},
    ty::{TypePath, TypeSignature},
    util::comma_separated,
    NodeParser,
};

#[derive(Debug, PartialEq, Eq)]
pub enum VariantValue<'a> {
    /// A unit variant with no attached data, e.g. `UnitVariant`
    Unit,
    /// A tuple-style variant, e.g. `TupleVariant(int, int)`
    Tuple(Vec<TypeSignature<'a>>),
    /// A struct-style variant, e.g. `StructVariant { field: int }`
    Struct(BTreeMap<&'a str, TypeSignature<'a>>),
    /// A variant with a single value, e.g. `CStyleVariant = 42`
    Integer(i64),
}

/// Represents a literal value at parse level.
#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    /// A unit literal, e.g. `()`
    Unit,
    /// An integer literal, e.g. `42` or `0x2A`
    Integer(i64),
    /// A float literal, e.g. `3.14` or `0.1e-10`
    Float(f64),
    /// A string literal, e.g. `"Hello, world!"`
    String(Cow<'a, str>),
    /// A utf-8 character literal, e.g. `'a'`
    Char(char),
    /// A boolean literal, e.g. `true` or `false`
    Boolean(bool),
    /// A tuple literal, e.g. `(1, 2, 3)`
    Tuple(Vec<Expr<'a>>),
    /// A struct literal / initializer, e.g. `Struct { field: 42 }`
    Struct(TypePath<'a>, BTreeMap<&'a str, Expr<'a>>),
    /// An array initializer, e.g. `[1, 2, 3]`
    Array(Vec<Expr<'a>>),
}

impl<'a> NodeParser<'a, Self> for Literal<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        let unit = just("()").map(|_| Literal::Unit);

        let integer = just("0x")
            .to(16)
            .then(chumsky::text::int(16))
            .or(just("0b").to(2).then(chumsky::text::int(2)))
            .or(just("0o").to(8).then(chumsky::text::int(8)))
            .or(chumsky::text::int(10).map(|i| (10, i)))
            .map(|(radix, int)| {
                let num =
                    i64::from_str_radix(int, radix).expect("to parse integer literal as integer");

                Literal::Integer(num)
            });

        let scientific = just("e")
            .or(just("E"))
            .then(
                // Special sign case because handling this as a unary expr would be a pain
                just("-")
                    .or_not()
                    .map(|o| if o.is_some() { "-" } else { "" }),
            )
            .then(chumsky::text::int(10))
            .map(|((e, negate), i)| format!("{}{}{}", e, negate, i));

        let float = chumsky::text::int(10)
            .then_ignore(just("."))
            .then(chumsky::text::int(10))
            .then(scientific.or_not())
            .map(|((f, f2), sci)| {
                let combined = if let Some(sci) = sci {
                    format!("{}.{}{}", f, f2, sci)
                } else {
                    format!("{}.{}", f, f2)
                };
                let num = combined.parse::<f64>().expect("to parse float literal");
                Literal::Float(num)
            });

        let escape = just('\\').ignore_then(choice((
            just('/'),
            just('"'),
            just('\\'),
            just('b').to('\x08'),
            just('f').to('\x0C'),
            just('n').to('\n'),
            just('r').to('\r'),
            just('t').to('\t'),
            just("u{")
                .ignore_then(
                    chumsky::text::digits(16)
                        .at_least(2)
                        .at_most(6)
                        .to_slice()
                        .validate(|digits, _e, _emitter| {
                            // TODO: report error
                            char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap()
                        }),
                )
                .then_ignore(just("}")),
        )));

        let string = none_of("\\\"")
            .or(escape)
            .repeated()
            .collect::<Vec<char>>()
            .delimited_by(just('"'), just('"'))
            .map(|s| Literal::String(Cow::Owned(s.into_iter().collect::<String>())));

        let char = none_of("\\'")
            .or(escape)
            .delimited_by(just('\''), just('\''))
            .map(Literal::Char);

        let boolean = just("true")
            .map(|_| Literal::Boolean(true))
            .or(just("false").map(|_| Literal::Boolean(false)));

        choice((
            unit,    //
            float,   //
            integer, //
            string,  //
            char,    //
            boolean, //,
        ))
        .padded()
    }
}

#[derive(Debug, PartialEq)]
pub struct MatchArm<'a> {
    pub pattern: Expr<'a>,
    pub body: Expr<'a>,
}

/// Represents an expression at parse level.
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    /// A literal value, e.g. `42` or `true`
    Literal(Literal<'a>),
    /// A binary expression, e.g. `1 + 2` or `foo && bar`
    Binary(Box<Expr<'a>>, BinaryOp, Box<Expr<'a>>),
    /// A unary expression, e.g. `*foo` or `-42`
    Unary(UnaryOp, Box<Expr<'a>>),
    /// An identifier, e.g. `foo`
    Identifier(&'a str),
    /// A call expression, e.g. `foo(42, 3.14)`
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    /// An array index expression, e.g. `foo[42]`
    Index(Box<Expr<'a>>, Box<Expr<'a>>),
    /// A struct field access, e.g. `foo.bar`
    /// This is used for both struct fields and enum variants.
    StructField(Box<Expr<'a>>, &'a str),
    /// A tuple field access, e.g. `foo.0`
    TupleField(Box<Expr<'a>>, usize),
    /// A block expression, e.g. `{ let x = 42; x }`
    ///
    /// The last expression in the block is used as the return value,
    /// but early returns are allowed with the `return` statement.
    Block {
        body: Vec<Stmt<'a>>,
        terminator: Box<Expr<'a>>,
    },
    If {
        cond: Box<Expr<'a>>,
        body: Box<Expr<'a>>,
        alternate: Option<Box<Expr<'a>>>,
    },
    /// Rust-style loops work as expressions, but *must* yield a value
    /// if used as such.
    Loop(Box<Expr<'a>>),
    /// Used to break out of a loop, e.g. `break;` or `break 42;`
    /// The value is optional, and is only used if the loop is used as an expression.
    Break(Option<Box<Expr<'a>>>),
    Match {
        value: Box<Expr<'a>>,
        arms: Vec<MatchArm<'a>>,
    },
}

impl<'a> NodeParser<'a, Self> for Expr<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        recursive(|expr| {
            let literal = Literal::parser().map(Expr::Literal);

            let r#match = keyword("match")
                .padded()
                .ignore_then(expr.clone().padded())
                .then(
                    expr.clone()
                        .then_ignore(just("=>").padded())
                        .then(expr.clone().padded())
                        .map(|(pattern, body)| MatchArm { pattern, body })
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just("{").padded(), just("}").padded()),
                )
                .padded()
                .map(|(value, arms)| Expr::Match {
                    value: Box::new(value),
                    arms,
                });

            let r#let = keyword("let")
                .ignore_then(expr.clone().padded())
                .then(
                    just(":")
                        .padded()
                        .ignore_then(TypeSignature::parser())
                        .or_not(),
                )
                .then(just("=").padded().ignore_then(expr.clone()).or_not())
                .map(|((lhs, ty), rhs)| Stmt::Let(lhs, ty, rhs));

            let assignment = expr
                .clone()
                .padded()
                .then_ignore(just("=").padded())
                .then(expr.clone())
                .map(|(lhs, rhs)| Stmt::Assignment(lhs, rhs));

            let r#return = keyword("return")
                .ignore_then(expr.clone().padded())
                .map(Stmt::Return);

            let block = recursive(|block| {
                let r#while = keyword("while")
                    .ignore_then(expr.clone())
                    .then(block.clone())
                    .map(|(cond, body)| Stmt::While {
                        condition: cond,
                        body,
                    });

                let r#for = keyword("for")
                    .ignore_then(expr.clone().padded())
                    .then_ignore(keyword("in").padded())
                    .then(expr.clone().padded())
                    .then(block.clone())
                    .map(|((pattern, iter), body)| Stmt::For {
                        pattern,
                        iter,
                        body,
                    });

                let stmt = choice((
                    r#let,      //
                    assignment, //
                    r#return,
                    r#while,
                    r#for,
                    expr.clone().map(Stmt::Expression),
                ))
                .then_ignore(one_of(";\n"))
                .padded();

                stmt.repeated()
                    .collect::<Vec<_>>()
                    .then(expr.clone())
                    .delimited_by(just("{").padded(), just("}").padded())
                    .map(|(x, terminator)| {
                        // let terminator = x.pop().expect("block to have a terminator");

                        Expr::Block {
                            body: x,
                            terminator: Box::new(terminator),
                        }
                    })
            });

            let r#loop = keyword("loop")
                .ignore_then(block.clone())
                .map(Box::new)
                .map(Expr::Loop);

            let r#break = keyword("break")
                .then(expr.clone().or_not())
                .map(|(_, val)| Expr::Break(val.map(Box::new)));

            // let block = stmt
            //     .repeated()
            //     .collect::<Vec<_>>()
            //     .then(expr.clone())
            //     .delimited_by(just("{").padded(), just("}").padded())
            //     .map(|(x, terminator)| {
            //         // let terminator = x.pop().expect("block to have a terminator");
            //
            //         Expr::Block {
            //             body: x,
            //             terminator: Box::new(terminator),
            //         }
            //     });

            let r#if = recursive(|r#if| {
                keyword("if")
                    .padded()
                    .ignore_then(expr.clone())
                    .then(block.clone())
                    .then(
                        keyword("else")
                            .ignore_then(r#if.clone().or(block.clone()))
                            .or_not()
                            .padded(),
                    )
                    .map(|((cond, body), else_body)| {
                        return Expr::If {
                            cond: Box::new(cond),
                            body: Box::new(body),
                            alternate: else_body.map(Box::new),
                        };
                    })
            });

            // (,) is the same as () because... reasons
            let unit = just(",")
                .or_not()
                .delimited_by(just("("), just(")"))
                .map(|_| Literal::Unit)
                .map(Expr::Literal);

            let tuple = expr
                .clone()
                .padded()
                .separated_by(just(","))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just("("), just(")"))
                .map(Literal::Tuple)
                .map(Expr::Literal);

            let array = comma_separated(expr.clone())
                .delimited_by(just("["), just("]"))
                .map(Literal::Array)
                .map(Expr::Literal);

            let struct_field = ident()
                .then_ignore(just(":").padded())
                .then(expr.clone())
                .map(|(name, value)| (name, value));

            let struct_ = TypePath::parser()
                .then_ignore(just("{").padded())
                .then(
                    struct_field
                        .separated_by(just(",").padded())
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .map(BTreeMap::from_iter)
                        .boxed(),
                )
                .then_ignore(just("}").padded())
                .map(|(name, fields)| Literal::Struct(name, BTreeMap::from_iter(fields)))
                .map(Expr::Literal);

            let atom = choice((
                literal,
                r#loop,
                r#if,
                r#break,
                r#match,
                tuple,
                array,
                struct_,
                unit,
                ident().padded().map(Expr::Identifier),
                block,
                expr.clone().padded().delimited_by(just("("), just(")")),
            ))
            .boxed();

            let call = atom
                .foldl(
                    expr.clone()
                        .separated_by(just(','))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just("("), just(")"))
                        .repeated(),
                    |lhs, args| {
                        return Expr::Call(Box::new(lhs), args);
                    },
                )
                .boxed();

            let index_access = call.foldl(
                expr.clone().delimited_by(just("["), just("]")).repeated(),
                |lhs, rhs| Expr::Index(Box::new(lhs), Box::new(rhs)),
            );

            let field_access = index_access
                .foldl(just(".").ignore_then(ident()).repeated(), |lhs, rhs| {
                    Expr::StructField(Box::new(lhs), rhs)
                });

            let tuple_access = field_access.foldl(
                just(".").ignore_then(chumsky::text::int(10)).repeated(),
                |lhs, rhs| {
                    let rhs = str::parse::<usize>(&rhs).unwrap();
                    Expr::TupleField(Box::new(lhs), rhs)
                },
            );

            let unary_op = just("-")
                .map(|_| UnaryOp::Negation)
                .or(just("!").map(|_| UnaryOp::Not))
                .or(just("*").map(|_| UnaryOp::Dereference))
                .or(just("&").map(|_| UnaryOp::Reference));

            let unary = recursive(|unary| {
                unary_op
                    .then(unary)
                    .map(|(op, expr)| Expr::Unary(op, Box::new(expr)))
                    .or(tuple_access)
            })
            .boxed();

            let bin_parsers = [
                just("*")
                    .map(|_| BinaryOp::Multiply)
                    .or(just("/").map(|_| BinaryOp::Divide))
                    .boxed(),
                just("+")
                    .map(|_| BinaryOp::Add)
                    .or(just("/").map(|_| BinaryOp::Subtract))
                    .boxed(),
                just("%").map(|_| BinaryOp::Modulo).boxed(),
                just("<<")
                    .map(|_| BinaryOp::ShiftLeft)
                    .or(just(">>").map(|_| BinaryOp::ShiftRight))
                    .boxed(),
                just("<")
                    .map(|_| BinaryOp::LessThan)
                    .or(just(">").map(|_| BinaryOp::GreaterThan))
                    .or(just("<=").map(|_| BinaryOp::LessThanOrEqual))
                    .or(just(">=").map(|_| BinaryOp::GreaterThanOrEqual))
                    .boxed(),
                just("==")
                    .map(|_| BinaryOp::Equal)
                    .or(just("!=").map(|_| BinaryOp::NotEqual))
                    .boxed(),
                just("&").map(|_| BinaryOp::And).boxed(),
                just("^").map(|_| BinaryOp::Xor).boxed(),
                just("|").map(|_| BinaryOp::Or).boxed(),
                just("&&").map(|_| BinaryOp::LogicalAnd).boxed(),
                just("||").map(|_| BinaryOp::LogicalOr).boxed(),
            ];

            let mut binary = unary.boxed();
            for op in &bin_parsers {
                binary = binary
                    .clone()
                    .foldl(op.clone().then(binary).repeated(), |lhs, (op, rhs)| {
                        Expr::Binary(Box::new(lhs), op, Box::new(rhs))
                    })
                    .boxed();
            }

            binary
        })
    }
}
