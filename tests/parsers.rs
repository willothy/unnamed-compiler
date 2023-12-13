use std::{borrow::Cow, collections::BTreeMap};

use chumsky::{IterParser, Parser};
use crane::{
    expr::{Expr, Literal},
    stmt::{BinaryOp, UnaryOp},
    ty::{TypePath, TypePathSegment, TypeSignature},
    NodeParser as _,
};

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

#[test]
fn parse_expr() {
    let input = "1 + 2 * 3";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Binary(
            Box::new(Expr::Literal(Literal::Integer(1))),
            BinaryOp::Add,
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Integer(2))),
                BinaryOp::Multiply,
                Box::new(Expr::Literal(Literal::Integer(3)))
            ))
        )
    );

    let input = "1 + 2 * 3 + 4";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Binary(
            Box::new(Expr::Binary(
                Box::new(Expr::Literal(Literal::Integer(1))),
                BinaryOp::Add,
                Box::new(Expr::Binary(
                    Box::new(Expr::Literal(Literal::Integer(2))),
                    BinaryOp::Multiply,
                    Box::new(Expr::Literal(Literal::Integer(3)))
                ))
            )),
            BinaryOp::Add,
            Box::new(Expr::Literal(Literal::Integer(4)))
        )
    );
}

#[test]
fn parse_call_expr() {
    let input = "foo()";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Call(Box::new(Expr::Identifier("foo")), vec![])
    );

    let input = "foo(42)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Call(
            Box::new(Expr::Identifier("foo")),
            vec![Expr::Literal(Literal::Integer(42))]
        )
    );

    let input = "foo(42, 3.14)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Call(
            Box::new(Expr::Identifier("foo")),
            vec![
                Expr::Literal(Literal::Integer(42)),
                Expr::Literal(Literal::Float(3.14))
            ]
        )
    );
}

#[test]
fn test_index_expr() {
    let input = "foo[42]";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Index(
            Box::new(Expr::Identifier("foo")),
            Box::new(Expr::Literal(Literal::Integer(42)))
        )
    );
}

#[test]
fn parse_struct_access_expr() {
    let input = "foo.bar";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::StructField(Box::new(Expr::Identifier("foo")), "bar")
    );

    let input = "foo.bar.baz";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::StructField(
            Box::new(Expr::StructField(Box::new(Expr::Identifier("foo")), "bar")),
            "baz"
        )
    );

    let input = "foo().bar";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::StructField(
            Box::new(Expr::Call(Box::new(Expr::Identifier("foo")), vec![])),
            "bar"
        )
    );
}

#[test]
fn test_tuple_access_expr() {
    let input = "foo.0";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::TupleField(Box::new(Expr::Identifier("foo")), 0)
    );

    let input = "foo.0.1";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::TupleField(
            Box::new(Expr::TupleField(Box::new(Expr::Identifier("foo")), 0)),
            1
        )
    );

    let input = "foo().0";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::TupleField(
            Box::new(Expr::Call(Box::new(Expr::Identifier("foo")), vec![])),
            0
        )
    );
}

#[test]
fn parse_tuple_init_expr() {
    let input = "(1, 2, 3)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Tuple(vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ]))
    );

    let input = "(1, 2, 3,)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Tuple(vec![
            Expr::Literal(Literal::Integer(1)),
            Expr::Literal(Literal::Integer(2)),
            Expr::Literal(Literal::Integer(3)),
        ]))
    );

    let input = "(1,)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Tuple(vec![Expr::Literal(Literal::Integer(1)),]))
    );

    let input = "(,)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Expr::Literal(Literal::Unit));
}

#[test]
fn pare_struct_init() {
    let input = "Foo { bar: 42 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Struct(
            TypePath {
                name: "Foo",
                path: None
            },
            BTreeMap::from_iter([("bar", Expr::Literal(Literal::Integer(42)))])
        ))
    );

    let input = "Foo { bar: 42, }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Struct(
            TypePath {
                name: "Foo",
                path: None
            },
            BTreeMap::from_iter([("bar", Expr::Literal(Literal::Integer(42)))])
        ))
    );

    let input = "Foo { bar: 42, baz: 3.14 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Literal(Literal::Struct(
            TypePath {
                name: "Foo",
                path: None
            },
            BTreeMap::from_iter([
                ("bar", Expr::Literal(Literal::Integer(42))),
                ("baz", Expr::Literal(Literal::Float(3.14)))
            ])
        ))
    );
}

#[test]
fn test_if_expr() {
    let input = "if foo { 42 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::If {
            cond: Box::new(Expr::Identifier("foo")),
            body: Box::new(Expr::Block {
                body: vec![],
                terminator: Box::new(Expr::Literal(Literal::Integer(42)))
            }),
            alternate: None,
        }
    );

    let input = "if foo { 42 } else { 3.14 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::If {
            cond: Box::new(Expr::Identifier("foo")),
            body: Box::new(Expr::Block {
                body: vec![],
                terminator: Box::new(Expr::Literal(Literal::Integer(42)))
            }),
            alternate: Some(Box::new(Expr::Block {
                body: vec![],
                terminator: Box::new(Expr::Literal(Literal::Float(3.14)))
            })),
        }
    );

    let input = "if foo { 42 } else if bar { 3.14 } else { 0 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::If {
            cond: Box::new(Expr::Identifier("foo")),
            body: Box::new(Expr::Block {
                body: vec![],
                terminator: Box::new(Expr::Literal(Literal::Integer(42)))
            }),
            alternate: Some(Box::new(Expr::If {
                cond: Box::new(Expr::Identifier("bar")),
                body: Box::new(Expr::Block {
                    body: vec![],
                    terminator: Box::new(Expr::Literal(Literal::Float(3.14)))
                }),
                alternate: Some(Box::new(Expr::Block {
                    body: vec![],
                    terminator: Box::new(Expr::Literal(Literal::Integer(0)))
                })),
            })),
        }
    );
}

#[test]
fn test_match_expr() {
    let input = "match foo { 42 => 3.14 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Match {
            value: Box::new(Expr::Identifier("foo")),
            arms: vec![crane::expr::MatchArm {
                pattern: Expr::Literal(Literal::Integer(42)),
                body: Expr::Literal(Literal::Float(3.14))
            }]
        }
    );
}
