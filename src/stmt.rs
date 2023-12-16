use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    Parser,
};

use crate::{expr::Expr, NodeParser};

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    /// An assignment, e.g. `x = 42;` or `let Point { x, y } = point;`
    Assignment(Expr<'a>, Expr<'a>),
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
    /// A return statement
    Return(Option<Expr<'a>>),
    /// A break statement, e.g. `break;` or `break 42;`
    Break(Option<Expr<'a>>),
}

impl<'a> Stmt<'a> {
    pub fn is_terminator(&self) -> bool {
        match self {
            Stmt::Return(_) | Stmt::Break(_) => true,
            Stmt::Expression(_) => true,
            Stmt::Assignment(_, _) | Stmt::While { .. } | Stmt::For { .. } => false,
        }
    }

    pub fn is_return(&self) -> bool {
        match self {
            Stmt::Return(_) => true,
            _ => false,
        }
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
    fn parser() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone + 'a {
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
    fn parser() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone + 'a {
        choice((
            just("*").to(Self::Dereference),
            just("&").to(Self::Reference),
            just("-").to(Self::Negation),
            just("!").to(Self::Not),
            just("~").to(Self::BitwiseNot),
        ))
    }
}
