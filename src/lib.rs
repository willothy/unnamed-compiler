#![feature(trait_alias)]

use chumsky::{error::Rich, extra};
use module::Module;

pub mod expr;
pub mod module;
pub mod stmt;
pub mod ty;
pub mod util;

pub trait Parser<'a, T> =
    chumsky::Parser<'a, &'a str, T, extra::Full<Rich<'a, char>, Module, ()>> + Clone + 'a;

/// Defines the interface used to construct a parser for a node type.
///
/// Always outputs a `T` / `Self` and takes a `Self::Input` as input.
pub trait NodeParser<'a, T>
where
    Self: 'a,
{
    fn parser() -> impl Parser<'a, T>;
}
