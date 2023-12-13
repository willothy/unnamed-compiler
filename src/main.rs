use std::{borrow::Cow, collections::BTreeMap};

use chumsky::{
    error::Rich,
    primitive::{choice, just, none_of},
    recursive::recursive,
    text::{ident, whitespace},
    IterParser, Parser,
};

#[derive(Debug, PartialEq, Eq)]
pub enum TypePathSegment<'a> {
    Package,
    SelfModule,
    SuperModule,
    Ident(&'a str),
}

/// Represents a type path, e.g. `package::path::to::type`.
/// This is used to represent the path to the type in the dependency graph.
///
/// The paths at parse level are not guaranteed to be valid path, and may contain
/// intermediate nodes that are not valid identifiers, such as `package`, `self` or `super`.
/// These will be parsed as [`TypePathSegment::Package`], [`TypePathSegment::SelfModule`] and
/// [`TypePathSegment::SuperModule`] respectively, and must be resolved to a valid path
/// if valid or reported as an error during import resolution.
#[derive(Debug, PartialEq, Eq)]
pub struct TypePath<'a> {
    /// The actual type name.
    name: &'a str,
    /// The path to the type, if any, starting with the package name
    /// or `package`, `self` or `super`.
    path: Option<Vec<TypePathSegment<'a>>>,
}

/// Defines the interface used to construct a parser for a node type.
///
/// Always outputs a `T` / `Self` and takes a `Self::Input` as input.
pub trait NodeParser<'a, T>
where
    Self: 'a,
{
    fn parser() -> impl Parser<'a, &'a str, T> + Clone + 'a;
}

impl<'a> NodeParser<'a, TypePathSegment<'a>> for TypePathSegment<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        ident().map(|s| match s {
            "package" => Self::Package,
            "self" => Self::SelfModule,
            "super" => Self::SuperModule,
            name => Self::Ident(name),
        })
    }
}

impl<'a> NodeParser<'a, TypePath<'a>> for TypePath<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        TypePathSegment::parser()
            .separated_by(just("::"))
            .at_least(2)
            .collect::<Vec<_>>()
            .map(|mut path| {
                let name = path.pop().unwrap();
                let TypePathSegment::Ident(name) = name else {
                    unreachable!()
                };

                TypePath {
                    name,
                    path: Some(path),
                }
            })
            .or(ident().map(|name| TypePath { name, path: None }))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TypeSignature<'a> {
    Unit,
    // Struct, enum or union types without generic parameters
    // Also primitive types - maybe this should be a separate variant?
    Named(TypePath<'a>),
    // Struct, enum or union types with generic parameters
    GenericApplication(TypePath<'a>, Vec<TypeSignature<'a>>),
    Tuple(Vec<TypeSignature<'a>>),
    Array(Box<TypeSignature<'a>>),
    Function(Vec<TypeSignature<'a>>, Box<TypeSignature<'a>>),
    Reference(Box<TypeSignature<'a>>),
}

fn comma_separated<'a, P: 'a + Parser<'a, &'a str, T>, T: 'a>(
    parser: P,
) -> impl Parser<'a, &'a str, Vec<T>> + Clone {
    parser
        .separated_by(just(",").then_ignore(whitespace()))
        .collect::<Vec<_>>()
        .boxed()
}

impl<'a> NodeParser<'a, TypeSignature<'a>> for TypeSignature<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        fn unit<'b>() -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            just("()").map(|_| TypeSignature::Unit)
        }

        fn array<'b>(
            p: impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b,
        ) -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            just("[")
                .ignore_then(p)
                .then_ignore(just("]"))
                .map(|t| TypeSignature::Array(Box::new(t)))
        }

        fn generic_application<'b>(
            p: impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b,
        ) -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            TypePath::parser()
                .then(comma_separated(p).delimited_by(just("<"), just(">")))
                .map(|(name, args)| TypeSignature::GenericApplication(name, args))
        }

        fn tuple<'b>(
            p: impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b,
        ) -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            comma_separated(p)
                .delimited_by(just("("), just(")"))
                .map(|t| TypeSignature::Tuple(t))
        }

        fn reference<'b>(
            p: impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b,
        ) -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            just("&")
                .ignore_then(whitespace().or_not())
                .ignore_then(p)
                .map(|t| TypeSignature::Reference(Box::new(t)))
        }

        fn function<'b>(
            p: impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b,
        ) -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            just("fn")
                .ignore_then(
                    whitespace()
                        .ignore_then(comma_separated(p.clone()))
                        .then_ignore(whitespace())
                        .delimited_by(just("("), just(")")),
                )
                .then_ignore(
                    whitespace()
                        .or_not()
                        .then(just("->"))
                        .then(whitespace().or_not()),
                )
                .then(p)
                .map(|(input, output)| TypeSignature::Function(input, Box::new(output)))
        }

        fn named<'b>() -> impl Parser<'b, &'b str, TypeSignature<'b>> + Clone + 'b {
            TypePath::parser().map(TypeSignature::Named)
        }

        recursive(|this| {
            choice((
                unit(),
                array(this.clone()),
                generic_application(this.clone()),
                tuple(this.clone()),
                reference(this.clone()),
                function(this.clone()),
                named(),
            ))
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
enum VariantValue<'a> {
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
enum Literal<'a> {
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
    }
}

#[test]
fn parse_float_literal() {
    let input = "3.14";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Float(3.14));

    let input = "3.14e10";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Float(3.14e10));

    let input = "3.14e-10";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Float(3.14e-10));
}

#[test]
fn parse_int_literal() {
    let input = "42";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Integer(42));

    let input = "0x2A";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Integer(42));

    let input = "0b101010";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Integer(42));

    let input = "0o52";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Integer(42));
}

#[test]
fn parse_string_literal() {
    let input = "\"Hello, world!\"";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Literal::String(Cow::Borrowed("Hello, world!"))
    );

    let input = "\"Hello, \\\"world!\\\"\"";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Literal::String(Cow::Borrowed("Hello, \"world!\""))
    );

    let input = "\"Hello, \\u{1F600}!\"";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Literal::String(Cow::Borrowed("Hello, 😀!"))
    );
}

#[test]
fn parse_char_literal() {
    let input = "'a'";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Char('a'));
    let input = "'\\n'";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Char('\n'));
    let input = "'\\u{1F600}'";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Char('😀'));
}

#[derive(Debug, PartialEq)]
enum Stmt<'a> {
    /// A let binding, e.g. `let x: int = 42;`
    Let(&'a str, Option<TypeSignature<'a>>, Option<Expr<'a>>),
    /// An assignment, e.g. `x = 42;` or `let Point { x, y } = point;`
    Assignment(Expr<'a>, Expr<'a>),
    /// A return statement
    Return(Expr<'a>),
    /// An expression statement.
    Expression(Expr<'a>),
    /// While loops cannot be used as expressions because their resulting value cannot be
    /// statically guaranteed.
    While { condition: Expr<'a>, body: Expr<'a> },
    /// For loops follow the same rules as while loops
    For {
        pattern: Expr<'a>,
        iter: Expr<'a>,
        body: Expr<'a>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    LogicalAnd,
    LogicalOr,

    And,
    Or,
    Xor,
}

impl<'a> NodeParser<'a, BinaryOp> for BinaryOp {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        choice((
            just("+").map(|_| Self::Add),
            just("-").map(|_| Self::Subtract),
            just("*").map(|_| Self::Multiply),
            just("/").map(|_| Self::Divide),
            just("%").map(|_| Self::Modulo),
            just("&&").map(|_| Self::LogicalAnd),
            just("||").map(|_| Self::LogicalOr),
            just("&").map(|_| Self::And),
            just("|").map(|_| Self::Or),
            just("^").map(|_| Self::Xor),
        ))
    }
}

#[test]
fn binop_parser() {
    let input = "+-*/%&&||&|^";
    let result = BinaryOp::parser()
        .repeated()
        .collect::<Vec<_>>()
        .parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &[
            BinaryOp::Add,
            BinaryOp::Subtract,
            BinaryOp::Multiply,
            BinaryOp::Divide,
            BinaryOp::Modulo,
            BinaryOp::LogicalAnd,
            BinaryOp::LogicalOr,
            BinaryOp::And,
            BinaryOp::Or,
            BinaryOp::Xor,
        ]
    );
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryOp {
    /// Dereference, e.g. `*foo`
    Dereference,
    /// Reference, e.g. `&foo`
    Reference,
    /// Negation, e.g. `-42`
    Negation,
    /// Logical not, e.g. `!foo`
    Not,
    /// Two's complement, e.g. `~foo
    BitwiseNot,
}

impl<'a> NodeParser<'a, UnaryOp> for UnaryOp {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone + 'a {
        choice((
            just("*").to(Self::Dereference),
            just("&").to(Self::Reference),
            just("-").to(Self::Negation),
            just("!").to(Self::Not),
            just("~").to(Self::BitwiseNot),
        ))
    }
}

#[test]
fn unop_parser() {
    let input = "*&-!~";
    let result = UnaryOp::parser()
        .repeated()
        .collect::<Vec<_>>()
        .parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &[
            UnaryOp::Dereference,
            UnaryOp::Reference,
            UnaryOp::Negation,
            UnaryOp::Not,
            UnaryOp::BitwiseNot,
        ]
    );
}

#[derive(Debug, PartialEq)]
pub struct MatchArm<'a> {
    pattern: Expr<'a>,
    body: Expr<'a>,
}

/// Represents an expression at parse level.
#[derive(Debug, PartialEq)]
enum Expr<'a> {
    /// A literal value, e.g. `42` or `true`
    Literal(Literal<'a>),
    /// A binary expression, e.g. `1 + 2` or `foo && bar`
    Binary(Box<Expr<'a>>, &'a str, Box<Expr<'a>>),
    /// A unary expression, e.g. `*foo` or `-42`
    Unary(&'a str, Box<Expr<'a>>),
    /// An identifier, e.g. `foo`
    Identifier(&'a str),
    /// A call expression, e.g. `foo(42, 3.14)`
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    /// An array index expression, e.g. `foo[42]`
    Index(Box<Expr<'a>>, Box<Expr<'a>>),
    /// A field access, e.g. `foo.bar`
    /// This is used for both struct fields and enum variants.
    Field(Box<Expr<'a>>, &'a str),
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

#[derive(Debug, PartialEq)]
enum Declaration<'a> {
    Function {
        name: &'a str,
        ty: TypeSignature<'a>,
        body: Expr<'a>,
    },
    Struct {
        name: &'a str,
        fields: BTreeMap<&'a str, TypeSignature<'a>>,
    },
    Enum {
        name: &'a str,
        variants: Vec<(&'a str, VariantValue<'a>)>,
    },
    Union {
        name: &'a str,
        variants: BTreeMap<&'a str, TypeSignature<'a>>,
    },
    Static {
        name: &'a str,
        ty: TypeSignature<'a>,
        value: Expr<'a>,
    },
    Constant {
        name: &'a str,
        ty: TypeSignature<'a>,
        value: Expr<'a>,
    },
    /// A type alias, e.g. `type Foo<T> = Vec<T>;`
    TypeAlias {
        name: &'a str,
        /// The generic parameters of the type alias, if any.
        ///
        /// The `T` in `type Foo<T> = Vec<T>;` is a generic parameter.
        generic_params: Vec<&'a str>,
        /// The type the alias points to. Unresolved at this stage.
        ty: TypeSignature<'a>,
    },
    /// A top-level import, e.g. `use package::path::to::type;`
    ///
    /// Not sure if this should be in [`Declaration`] or not...
    Use { path: TypePath<'a> },
}

#[test]
fn parse_type_signature() {
    let input = "fn(self::Test, (&int, [int])) -> &package::Vec<(T, U)>";
    let result = TypeSignature::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &TypeSignature::Function(
            vec![
                TypeSignature::Named(TypePath {
                    name: "Test",
                    path: Some(vec![TypePathSegment::SelfModule])
                }),
                TypeSignature::Tuple(vec![
                    TypeSignature::Reference(Box::new(TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    }))),
                    TypeSignature::Array(Box::new(TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    }))),
                ])
            ],
            Box::new(TypeSignature::Reference(Box::new(
                TypeSignature::GenericApplication(
                    TypePath {
                        name: "Vec",
                        path: Some(vec![TypePathSegment::Package])
                    },
                    vec![TypeSignature::Tuple(vec![
                        TypeSignature::Named(TypePath {
                            name: "T",
                            path: None
                        }),
                        TypeSignature::Named(TypePath {
                            name: "U",
                            path: None
                        })
                    ])]
                )
            )))
        ),
        "{:#?}",
        result
    );
}

#[test]
fn parse_type_path() {
    let input = "package::path::to::type";
    let result = TypePath::parser().parse(input);
    println!("{:#?}", result);
    assert_eq!(
        result.output().expect("to output a typepath"),
        &TypePath {
            name: "type",
            path: Some(vec![
                TypePathSegment::Package,
                TypePathSegment::Ident("path"),
                TypePathSegment::Ident("to")
            ])
        }
    );

    let input = "type";
    let result = TypePath::parser().parse(input);
    println!("{:#?}", result);
    assert_eq!(
        result.output().expect("to output a typepath"),
        &TypePath {
            name: "type",
            path: None
        }
    );

    let input = "package::type";
    let result = TypePath::parser().parse(input);
    println!("{:#?}", result);
    assert_eq!(
        result.output().expect("to output a typepath"),
        &TypePath {
            name: "type",
            path: Some(vec![TypePathSegment::Package])
        }
    );

    let input = "self::type";
    let result = TypePath::parser().parse(input);
    println!("{:#?}", result);
    assert_eq!(
        result.output().expect("to output a typepath"),
        &TypePath {
            name: "type",
            path: Some(vec![TypePathSegment::SelfModule])
        }
    );
}

fn main() {
    let input = "package::path::to::type";
    let result = TypePath::parser().parse(input);
    println!("{:#?}", result);
}
