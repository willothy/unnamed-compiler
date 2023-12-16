use std::{borrow::Cow, collections::BTreeMap};

use chumsky::{IterParser, Parser};
use crane::{
    expr::{Expr, Literal},
    module::{Declaration, Variant},
    stmt::{BinaryOp, Stmt, UnaryOp},
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
fn parse_bool_literal() {
    let input = "true";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Boolean(true));

    let input = "false";
    let result = Literal::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output().unwrap(), &Literal::Boolean(false));
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
fn unary_expr() {
    let input = "!true";

    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Unary(
            UnaryOp::Not,
            Box::new(Expr::Literal(Literal::Boolean(true)))
        )
    );

    let input = "-42";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Unary(
            UnaryOp::Negation,
            Box::new(Expr::Literal(Literal::Integer(42)))
        )
    );

    let input = "&foo";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Unary(UnaryOp::Reference, Box::new(Expr::Identifier("foo")),)
    );

    let input = "*foo";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Unary(UnaryOp::Dereference, Box::new(Expr::Identifier("foo")),)
    );

    let input = "!foo + bar";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Binary(
            Box::new(Expr::Unary(UnaryOp::Not, Box::new(Expr::Identifier("foo")))),
            BinaryOp::Add,
            Box::new(Expr::Identifier("bar")),
        )
    );

    let input = "foo +!bar";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Binary(
            Box::new(Expr::Identifier("foo")),
            BinaryOp::Add,
            Box::new(Expr::Unary(UnaryOp::Not, Box::new(Expr::Identifier("bar")))),
        )
    );

    let input = "*test() + 42";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Binary(
            Box::new(Expr::Unary(
                UnaryOp::Dereference,
                Box::new(Expr::Call(Box::new(Expr::Identifier("test")), vec![]))
            )),
            BinaryOp::Add,
            Box::new(Expr::Literal(Literal::Integer(42))),
        )
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
fn index_expr() {
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
fn tuple_access_expr() {
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
fn parse_struct_init() {
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
fn if_expr() {
    let input = "if foo { 42 }";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::If {
            cond: Box::new(Expr::Identifier("foo")),
            body: Box::new(Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(42))))
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
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(42))))
            }),
            alternate: Some(Box::new(Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Literal(Literal::Float(3.14))))
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
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(42))))
            }),
            alternate: Some(Box::new(Expr::If {
                cond: Box::new(Expr::Identifier("bar")),
                body: Box::new(Expr::Block {
                    body: vec![],
                    terminator: Some(Box::new(Expr::Literal(Literal::Float(3.14))))
                }),
                alternate: Some(Box::new(Expr::Block {
                    body: vec![],
                    terminator: Some(Box::new(Expr::Literal(Literal::Integer(0))))
                })),
            })),
        }
    );
}

#[test]
fn match_expr() {
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

#[test]
fn tuple_access() {
    let input = "foo.0";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::TupleField(Box::new(Expr::Identifier("foo")), 0)
    );

    // chained
    let input = "foo.0.1";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::TupleField(
            Box::new(Expr::TupleField(Box::new(Expr::Identifier("foo")), 0)),
            1
        )
    );
}

#[test]
fn index_and_call_orders() {
    let input = "foo[42](3.14)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::Identifier("foo")),
                Box::new(Expr::Literal(Literal::Integer(42)))
            )),
            vec![Expr::Literal(Literal::Float(3.14))]
        )
    );

    let input = "foo(42)[3.14]";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Index(
            Box::new(Expr::Call(
                Box::new(Expr::Identifier("foo")),
                vec![Expr::Literal(Literal::Integer(42))]
            )),
            Box::new(Expr::Literal(Literal::Float(3.14)))
        )
    );

    let input = "foo(42)(3.14)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Expr::Call(
            Box::new(Expr::Call(
                Box::new(Expr::Identifier("foo")),
                vec![Expr::Literal(Literal::Integer(42))]
            )),
            vec![Expr::Literal(Literal::Float(3.14))]
        )
    );

    // longer chains

    let input = "foo[42][3.14](2.71)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::Index(
                    Box::new(Expr::Identifier("foo")),
                    Box::new(Expr::Literal(Literal::Integer(42)))
                )),
                Box::new(Expr::Literal(Literal::Float(3.14)))
            )),
            vec![Expr::Literal(Literal::Float(2.71))]
        )
    );

    // with struct / tuple access
    let input = "foo.bar[42](3.14)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::StructField(Box::new(Expr::Identifier("foo")), "bar")),
                Box::new(Expr::Literal(Literal::Integer(42)))
            )),
            vec![Expr::Literal(Literal::Float(3.14))]
        )
    );

    let input = "foo.bar(42)[3.14]";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Index(
            Box::new(Expr::Call(
                Box::new(Expr::StructField(Box::new(Expr::Identifier("foo")), "bar")),
                vec![Expr::Literal(Literal::Integer(42))]
            )),
            Box::new(Expr::Literal(Literal::Float(3.14)))
        )
    );

    let input = "foo.0.bar(42)[3.14]";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Index(
            Box::new(Expr::Call(
                Box::new(Expr::StructField(
                    Box::new(Expr::TupleField(Box::new(Expr::Identifier("foo")), 0)),
                    "bar"
                )),
                vec![Expr::Literal(Literal::Integer(42))]
            )),
            Box::new(Expr::Literal(Literal::Float(3.14)))
        )
    );

    let input = "foo.bar[42](3.14)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::StructField(Box::new(Expr::Identifier("foo")), "bar")),
                Box::new(Expr::Literal(Literal::Integer(42)))
            )),
            vec![Expr::Literal(Literal::Float(3.14))]
        )
    );

    // with range
    let input = "foo[42..][3.14](2.71)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::Index(
                    Box::new(Expr::Identifier("foo")),
                    Box::new(Expr::Range(
                        Some(Box::new(Expr::Literal(Literal::Integer(42)))),
                        None
                    ))
                )),
                Box::new(Expr::Literal(Literal::Float(3.14)))
            )),
            vec![Expr::Literal(Literal::Float(2.71))]
        )
    );

    let input = "foo[..42][3.14](2.71)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::Index(
                    Box::new(Expr::Identifier("foo")),
                    Box::new(Expr::Range(
                        None,
                        Some(Box::new(Expr::Literal(Literal::Integer(42)))),
                    ))
                )),
                Box::new(Expr::Literal(Literal::Float(3.14)))
            )),
            vec![Expr::Literal(Literal::Float(2.71))]
        )
    );

    let input = "foo[..][3.14](2.71)";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::Call(
            Box::new(Expr::Index(
                Box::new(Expr::Index(
                    Box::new(Expr::Identifier("foo")),
                    Box::new(Expr::Range(None, None))
                )),
                Box::new(Expr::Literal(Literal::Float(3.14)))
            )),
            vec![Expr::Literal(Literal::Float(2.71))]
        )
    );
}

#[test]
fn small_program() {
    let input = r#"
{
    let foo = 42;
    let bar = 3.14;
    foo = floor(bar);
    console.log(foo + bar);
}
    "#;

    let result = crate::Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &crate::Expr::Block {
            body: vec![
                Stmt::Expression(Expr::Let {
                    pattern: Box::new(Expr::Identifier("foo")),
                    ty: None,
                    value: Some(Box::new(Expr::Literal(Literal::Integer(42)))),
                }),
                Stmt::Expression(Expr::Let {
                    pattern: Box::new(Expr::Identifier("bar")),
                    ty: None,
                    value: Some(Box::new(Expr::Literal(Literal::Float(3.14)))),
                }),
                Stmt::Assignment(
                    Expr::Identifier("foo"),
                    Expr::Call(
                        Box::new(Expr::Identifier("floor")),
                        vec![Expr::Identifier("bar")]
                    )
                ),
                Stmt::Expression(Expr::Call(
                    Box::new(Expr::StructField(
                        Box::new(Expr::Identifier("console")),
                        "log"
                    )),
                    vec![Expr::Binary(
                        Box::new(Expr::Identifier("foo")),
                        BinaryOp::Add,
                        Box::new(Expr::Identifier("bar"))
                    ),]
                )),
            ],
            terminator: None
        },
    );
}

#[test]
fn if_let() {
    let input = r#"
if let foo = 42 {
    foo
} else {
    0
}
"#;
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::If {
            cond: Box::new(Expr::Let {
                pattern: Box::new(Expr::Identifier("foo")),
                ty: None,
                value: Some(Box::new(Expr::Literal(Literal::Integer(42)))),
            }),
            body: Box::new(Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Identifier("foo")))
            }),
            alternate: Some(Box::new(Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(0))))
            })),
        }
    );
}

#[test]
fn static_member_access() {
    let input = "foo::bar";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::StaticAccess(Box::new(Expr::Identifier("foo")), "bar")
    );

    let input = "foo::bar::baz";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Expr::StaticAccess(
            Box::new(Expr::StaticAccess(Box::new(Expr::Identifier("foo")), "bar")),
            "baz"
        )
    );
}

#[test]
fn declarations() {
    let input = "fn foo() {}";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Function {
            name: "foo",
            generic_params: None,
            params: vec![],
            ret: None,
            body: Expr::Block {
                body: vec![],
                terminator: None
            },
        }
    );

    let input = "fn foo() int { 42 }";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Function {
            name: "foo",
            generic_params: None,
            params: vec![],
            ret: Some(TypeSignature::Named(TypePath {
                name: "int",
                path: None
            })),
            body: Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(42))))
            },
        }
    );

    let input = "const foo: int = 42";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Constant {
            name: "foo",
            ty: TypeSignature::Named(TypePath {
                name: "int",
                path: None
            }),
            value: Expr::Literal(Literal::Integer(42)),
        }
    );

    let input = "static foo: int = 42";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Static {
            name: "foo",
            ty: TypeSignature::Named(TypePath {
                name: "int",
                path: None
            }),
            value: Expr::Literal(Literal::Integer(42)),
        }
    );

    let input = "struct Foo {}";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Struct {
            name: "Foo",
            generic_params: None,
            value: Variant::Struct(BTreeMap::new()),
        }
    );
}

#[test]
fn struct_decl() {
    let input = r#"struct Foo {
        bar: int,
        baz: bool,
    }
"#;

    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Struct {
            name: "Foo",
            generic_params: None,
            value: Variant::Struct(BTreeMap::from_iter([
                (
                    "bar",
                    TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    })
                ),
                (
                    "baz",
                    TypeSignature::Named(TypePath {
                        name: "bool",
                        path: None
                    })
                )
            ]))
        }
    );

    let input = r#"struct Foo"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Struct {
            generic_params: None,
            name: "Foo",
            value: Variant::Unit
        }
    );

    let input = r#"struct Foo {}"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Struct {
            generic_params: None,
            name: "Foo",
            value: Variant::Struct(BTreeMap::new())
        }
    );

    let input = "struct Foo(int, bool)";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Struct {
            name: "Foo",
            generic_params: None,
            value: Variant::Tuple(vec![
                TypeSignature::Named(TypePath {
                    name: "int",
                    path: None
                }),
                TypeSignature::Named(TypePath {
                    name: "bool",
                    path: None
                })
            ])
        }
    );
}

#[test]
fn enum_decl() {
    let input = r#"enum Foo {
        Bar,
        Baz,
    }"#;

    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Enum {
            name: "Foo",
            generic_params: None,
            variants: vec![("Bar", Variant::Unit), ("Baz", Variant::Unit),]
        }
    );

    let input = r#"enum Foo {
        Bar(int),
        Baz(int, bool),
    }"#;

    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());

    assert_eq!(
        *result.output().unwrap(),
        Declaration::Enum {
            name: "Foo",
            generic_params: None,
            variants: vec![
                (
                    "Bar",
                    Variant::Tuple(vec![TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    })])
                ),
                (
                    "Baz",
                    Variant::Tuple(vec![
                        TypeSignature::Named(TypePath {
                            name: "int",
                            path: None
                        }),
                        TypeSignature::Named(TypePath {
                            name: "bool",
                            path: None
                        })
                    ])
                ),
            ]
        }
    );

    let input = r#"enum Foo {
        Foo,
        Bar(int),
        Baz { baz: int, qux: bool },
    }"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Enum {
            name: "Foo",
            generic_params: None,
            variants: vec![
                ("Foo", Variant::Unit),
                (
                    "Bar",
                    Variant::Tuple(vec![TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    })])
                ),
                (
                    "Baz",
                    Variant::Struct(BTreeMap::from_iter([
                        (
                            "baz",
                            TypeSignature::Named(TypePath {
                                name: "int",
                                path: None
                            })
                        ),
                        (
                            "qux",
                            TypeSignature::Named(TypePath {
                                name: "bool",
                                path: None
                            })
                        )
                    ]))
                ),
            ]
        }
    );

    // generics
    let input = r#"enum Foo<T> {
        Foo,
        Bar(T),
        Baz { baz: T, qux: bool },
    }"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Enum {
            name: "Foo",
            generic_params: Some(vec!["T"]),
            variants: vec![
                ("Foo", Variant::Unit),
                (
                    "Bar",
                    Variant::Tuple(vec![TypeSignature::Named(TypePath {
                        name: "T",
                        path: None
                    })])
                ),
                (
                    "Baz",
                    Variant::Struct(BTreeMap::from_iter([
                        (
                            "baz",
                            TypeSignature::Named(TypePath {
                                name: "T",
                                path: None
                            })
                        ),
                        (
                            "qux",
                            TypeSignature::Named(TypePath {
                                name: "bool",
                                path: None
                            })
                        )
                    ]))
                ),
            ]
        }
    )
}

#[test]
fn const_decl() {
    let input = r#"const Foo: int = 1"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Constant {
            name: "Foo",
            ty: TypeSignature::Named(TypePath {
                name: "int",
                path: None
            }),
            value: Expr::Literal(Literal::Integer(1))
        }
    );

    let input = r#"const FOO: HashMap<Bar, Baz> = HashMap::new()"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Constant {
            name: "FOO",
            ty: TypeSignature::GenericApplication(
                TypePath {
                    name: "HashMap",
                    path: None
                },
                vec![
                    TypeSignature::Named(TypePath {
                        name: "Bar",
                        path: None
                    }),
                    TypeSignature::Named(TypePath {
                        name: "Baz",
                        path: None
                    }),
                ]
            ),
            value: Expr::Call(
                Box::new(Expr::StaticAccess(
                    Box::new(Expr::Identifier("HashMap")),
                    "new"
                )),
                vec![]
            )
        }
    );
}

#[test]
fn function_decl() {
    let input = r#"fn foo() int { 5 }"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Function {
            name: "foo",
            generic_params: None,
            params: vec![],
            ret: Some(TypeSignature::Named(TypePath {
                name: "int",
                path: None
            })),
            body: Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Literal(Literal::Integer(5))))
            }
        }
    );

    let input = r#"fn foo(bar: int) int { bar }"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Function {
            name: "foo",
            generic_params: None,
            params: vec![(
                "bar",
                TypeSignature::Named(TypePath {
                    name: "int",
                    path: None
                })
            )],
            ret: Some(TypeSignature::Named(TypePath {
                name: "int",
                path: None
            })),
            body: Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Identifier("bar")))
            }
        }
    );

    // generics
    let input = r#"fn foo<T>(bar: T) T { bar }"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Function {
            name: "foo",
            generic_params: Some(vec!["T"]),
            params: vec![(
                "bar",
                TypeSignature::Named(TypePath {
                    name: "T",
                    path: None
                })
            )],
            ret: Some(TypeSignature::Named(TypePath {
                name: "T",
                path: None
            })),
            body: Expr::Block {
                body: vec![],
                terminator: Some(Box::new(Expr::Identifier("bar")))
            }
        }
    );

    // multiple statements
    let input = r#"fn foo() int {
        let bar = 42;
        bar
    }"#;

    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::Function {
            name: "foo",
            generic_params: None,
            params: vec![],
            ret: Some(TypeSignature::Named(TypePath {
                name: "int",
                path: None
            })),
            body: Expr::Block {
                body: vec![Stmt::Expression(Expr::Let {
                    pattern: Box::new(Expr::Identifier("bar")),
                    ty: None,
                    value: Some(Box::new(Expr::Literal(Literal::Integer(42)))),
                }),],
                terminator: Some(Box::new(Expr::Identifier("bar")))
            }
        }
    );
}

#[test]
fn type_alias_decl() {
    let input = r#"type Foo = int"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output().unwrap(),
        &Declaration::TypeAlias {
            name: "Foo",
            generic_params: None,
            ty: TypeSignature::Named(TypePath {
                name: "int",
                path: None
            })
        }
    );

    let input = r#"type Foo<T> = HashMap<T, int>"#;
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::TypeAlias {
            name: "Foo",
            generic_params: Some(vec!["T"]),
            ty: TypeSignature::GenericApplication(
                TypePath {
                    name: "HashMap",
                    path: None
                },
                vec![
                    TypeSignature::Named(TypePath {
                        name: "T",
                        path: None
                    }),
                    TypeSignature::Named(TypePath {
                        name: "int",
                        path: None
                    })
                ]
            )
        }
    );
}

#[test]
fn parse_range() {
    let input = "0..10";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::Range(
            Some(Box::new(Expr::Literal(Literal::Integer(0)))),
            Some(Box::new(Expr::Literal(Literal::Integer(10))))
        ))
    );

    let input = "0..";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::Range(
            Some(Box::new(Expr::Literal(Literal::Integer(0)))),
            None
        ))
    );

    let input = "..10";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::Range(
            None,
            Some(Box::new(Expr::Literal(Literal::Integer(10))))
        ))
    );

    let input = "..";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output(), Some(&Expr::Range(None, None)));
}

#[test]
fn parse_range_inclusive() {
    let input = "0..=10";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::RangeInclusive(
            Some(Box::new(Expr::Literal(Literal::Integer(0)))),
            Some(Box::new(Expr::Literal(Literal::Integer(10))))
        ))
    );

    let input = "0..=";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::RangeInclusive(
            Some(Box::new(Expr::Literal(Literal::Integer(0)))),
            None
        ))
    );

    let input = "..=10";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::RangeInclusive(
            None,
            Some(Box::new(Expr::Literal(Literal::Integer(10))))
        ))
    );

    let input = "..=";
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(result.output(), Some(&Expr::RangeInclusive(None, None)));
}

#[test]
fn parse_use() {
    let input = "use std::collections::HashMap";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Use {
            alias: None,
            path: TypePath {
                name: "HashMap",
                path: Some(vec![
                    TypePathSegment::Ident("std"),
                    TypePathSegment::Ident("collections"),
                ])
            },
        }
    );

    let input = "use std::collections::HashMap as Map";
    let result = Declaration::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        *result.output().unwrap(),
        Declaration::Use {
            alias: Some("Map"),
            path: TypePath {
                name: "HashMap",
                path: Some(vec![
                    TypePathSegment::Ident("std"),
                    TypePathSegment::Ident("collections"),
                ])
            },
        }
    );
}

#[test]
fn use_in_statement_pos() {
    let input = r#"
    if true {
        use std::collections::HashMap as Map;
    }
    "#;
    let result = Expr::parser().parse(input);
    assert!(!result.has_errors(), "{:#?}", result.into_errors());
    assert_eq!(
        result.output(),
        Some(&Expr::If {
            cond: Box::new(Expr::Literal(Literal::Boolean(true))),
            body: Box::new(Expr::Block {
                body: vec![Stmt::Use {
                    alias: Some("Map"),
                    path: TypePath {
                        name: "HashMap",
                        path: Some(vec![
                            TypePathSegment::Ident("std"),
                            TypePathSegment::Ident("collections"),
                        ])
                    },
                }],
                terminator: None
            }),
            alternate: None,
        })
    );
}
