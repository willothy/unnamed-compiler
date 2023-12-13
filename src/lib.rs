use chumsky::Parser;

pub mod expr;
pub mod module;
pub mod stmt;
pub mod ty;
pub mod util;

/// Defines the interface used to construct a parser for a node type.
///
/// Always outputs a `T` / `Self` and takes a `Self::Input` as input.
pub trait NodeParser<'a, T>
where
    Self: 'a,
{
    fn parser() -> impl Parser<'a, &'a str, T> + Clone + 'a;
}
