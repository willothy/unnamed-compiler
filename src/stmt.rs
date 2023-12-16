use chumsky::{
    primitive::{choice, just},
    Parser,
};

use crate::{expr::Expr, module::Import, NodeParser};

#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// An assignment, e.g. `x = 42;` or `let Point { x, y } = point;`
    Assignment(Expr, Expr),
    /// An expression statement.
    Expression(Expr),
    /// While loops cannot be used as expressions because their resulting value cannot be
    /// statically guaranteed.
    While { condition: Expr, body: Expr },
    /// For loops follow the same rules as while loops
    For {
        pattern: Expr,
        iter: Expr,
        body: Expr,
    },
    /// A return statement
    Return(Option<Expr>),
    /// A break statement, e.g. `break;` or `break 42;`
    Break(Option<Expr>),
    /// A `use` in the statement position, e.g. `use foo::bar;`
    Use(Import),
}

impl Stmt {
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
    fn parser() -> impl crate::Parser<'a, Self> {
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
            just(">>").map(|_| Self::ShiftRight),
            just("<<").map(|_| Self::ShiftLeft),
            just(">=").map(|_| Self::GreaterThanOrEqual),
            just("<=").map(|_| Self::LessThanOrEqual),
            just(">").map(|_| Self::GreaterThan),
            just("<").map(|_| Self::LessThan),
            just("!=").map(|_| Self::NotEqual),
            just("==").map(|_| Self::Equal),
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
    fn parser() -> impl crate::Parser<'a, Self> {
        choice((
            just("*").to(Self::Dereference),
            just("&").to(Self::Reference),
            just("-").to(Self::Negation),
            just("!").to(Self::Not),
            just("~").to(Self::BitwiseNot),
        ))
    }
}
