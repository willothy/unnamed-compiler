use chumsky::{
    primitive::{choice, just, one_of},
    text::keyword,
    Parser,
};

use crate::{expr::Expr, ty::TypeSignature, NodeParser};

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    /// A let binding, e.g. `let x: int = 42;`
    Let(Expr<'a>, Option<TypeSignature<'a>>, Option<Expr<'a>>),
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

impl<'a> NodeParser<'a, Stmt<'a>> for Stmt<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self> + Clone {
        let r#let = keyword("let")
            .ignore_then(Expr::parser().padded())
            .then(
                just(":")
                    .padded()
                    .ignore_then(TypeSignature::parser())
                    .or_not(),
            )
            .then(just("=").padded().ignore_then(Expr::parser()).or_not())
            .map(|((lhs, ty), rhs)| Self::Let(lhs, ty, rhs));

        let assignment = Expr::parser()
            .padded()
            .then_ignore(just("=").padded())
            .then(Expr::parser())
            .map(|(lhs, rhs)| Self::Assignment(lhs, rhs));

        let r#return = keyword("return")
            .ignore_then(Expr::parser())
            .map(Self::Return);

        choice((
            r#let,      //
            assignment, //
            r#return,
            // Self::return_stmt(),
            // Self::while_loop(),
            // Self::for_loop(),
            // Self::expression(),
        ))
        .then_ignore(one_of(";\n").or_not())
    }
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
    ShiftRight,
    ShiftLeft,
    GreaterThanOrEqual,
    LessThanOrEqual,
    GreaterThan,
    LessThan,
    NotEqual,
    Equal,
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
