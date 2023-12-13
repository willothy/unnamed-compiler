use std::{borrow::Cow, collections::BTreeMap};

use chumsky::{
    primitive::{choice, just, none_of, todo},
    recovery::{self, Strategy},
    recursive::recursive,
    select,
    text::{ident, keyword, whitespace},
    IterParser, Parser,
};

use crate::{
    stmt::{BinaryOp, Stmt, UnaryOp},
    ty::TypeSignature,
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
    Struct(BTreeMap<&'a str, Expr<'a>>),
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

        // TODO: Expr parser

        // let tuple = comma_separated(Expr::parser())
        //     .delimited_by(just("("), just(")"))
        //     .map(Literal::Tuple);

        // let array = comma_separated(Expr::parser())
        //     .delimited_by(just("["), just("]"))
        //     .map(Literal::Array);
        //
        // let struct_field = ident()
        //     .then_ignore(just(":"))
        //     .then(Expr::parser())
        //     .map(|(name, value)| (name, value));
        //
        // let struct_ = ident()
        //     .then_ignore(just("{"))
        //     .then(comma_separated(struct_field))
        //     .then_ignore(just("}"))
        //     .map(|(name, fields)| {
        //         let mut map = BTreeMap::new();
        //         for (name, value) in fields {
        //             map.insert(name, value);
        //         }
        //         Literal::Struct(map)
        //     });

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
    pattern: Expr<'a>,
    body: Expr<'a>,
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
    Block(Vec<Stmt<'a>>, Box<Expr<'a>>),
    If(Box<Expr<'a>>, Box<Expr<'a>>, Option<Box<Expr<'a>>>),
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
        recursive(|this| {
            let literal = Literal::parser().map(Expr::Literal);

            let r#if = keyword("if")
                .ignore_then(this.clone())
                .then(this.clone())
                .then(keyword("else").ignore_then(this.clone()).or_not())
                .map(|((cond, body), else_body)| {
                    return Expr::If(Box::new(cond), Box::new(body), else_body.map(Box::new));
                });

            let r#loop = keyword("loop")
                .ignore_then(this.clone())
                .map(Box::new)
                .map(Expr::Loop);

            let r#break = keyword("break")
                .then(this.clone().or_not())
                .map(|(_, val)| Expr::Break(val.map(Box::new)));

            let index = this
                .clone()
                .then(just("[").ignore_then(this.clone()))
                .then_ignore(just("]"))
                .map(|(lhs, rhs)| Expr::Index(Box::new(lhs), Box::new(rhs)));

            let struct_field = this
                .clone()
                .then(just(".").ignore_then(ident()))
                .map(|(lhs, rhs)| Expr::StructField(Box::new(lhs), rhs));

            let tuple_field = this
                .clone()
                .then(just(".").ignore_then(chumsky::text::int(10)))
                .map(|(lhs, rhs)| {
                    let rhs = str::parse::<usize>(&rhs).unwrap();
                    Expr::TupleField(Box::new(lhs), rhs)
                });

            let ident = ident().padded().map(Expr::Identifier);

            let r#match = keyword("match")
                .ignore_then(this.clone().padded())
                .then(comma_separated(
                    this.clone()
                        .then_ignore(just("=>"))
                        .then(this.clone())
                        .map(|(pattern, body)| MatchArm { pattern, body }),
                ))
                .delimited_by(just("{"), just("}"))
                .map(|(value, arms)| Expr::Match {
                    value: Box::new(value),
                    arms,
                });

            // let block = this
            //     .clone()
            //     .repeated()
            //     .at_least(1)
            //     .collect::<Vec<_>>()
            //     .delimited_by(just("{"), just("}"))
            //     .map(|mut x| {
            //         let terminator = x.pop().expect("block to have a terminator");
            //
            //         Expr::Block(x, Box::new(terminator))
            //     });

            let atom = choice((
                literal, //
                r#loop,  //
                r#if,    //
                r#break, //
                r#match,
                ident, //
                       // block,   //
                       // struct_field,
                       // index,
                       // tuple_field,
                       // call,
                       // this.clone().padded().delimited_by(just("("), just(")")),
            ))
            .boxed();

            let call = atom
                .foldl(
                    this.clone()
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
                this.clone().delimited_by(just("["), just("]")).repeated(),
                |lhs, rhs| Expr::Index(Box::new(lhs), Box::new(rhs)),
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
                    .or(index_access)
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
