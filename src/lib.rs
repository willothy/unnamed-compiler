use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    text::whitespace,
    IterParser, Parser,
};
use ty::TypePath;

pub mod expr;
pub mod module;
pub mod stmt;
pub mod ty;

/// Defines the interface used to construct a parser for a node type.
///
/// Always outputs a `T` / `Self` and takes a `Self::Input` as input.
pub trait NodeParser<'a, T>
where
    Self: 'a,
{
    fn parser() -> impl Parser<'a, &'a str, T> + Clone + 'a;
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeSignature<'a> {
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
        recursive(|this| {
            let unit = just("()").map(|_| TypeSignature::Unit);

            let array = just("[")
                .ignore_then(this.clone())
                .then_ignore(just("]"))
                .map(|t| TypeSignature::Array(Box::new(t)));

            let generic_application = TypePath::parser()
                .then(comma_separated(this.clone()).delimited_by(just("<"), just(">")))
                .map(|(name, args)| TypeSignature::GenericApplication(name, args));

            let tuple = comma_separated(this.clone())
                .delimited_by(just("("), just(")"))
                .map(|t| TypeSignature::Tuple(t));

            let reference = just("&")
                .ignore_then(whitespace().or_not())
                .ignore_then(this.clone())
                .map(|t| TypeSignature::Reference(Box::new(t)));

            let function = just("fn")
                .ignore_then(
                    whitespace()
                        .ignore_then(comma_separated(this.clone()))
                        .then_ignore(whitespace())
                        .delimited_by(just("("), just(")")),
                )
                .then_ignore(
                    whitespace()
                        .or_not()
                        .then(just("->"))
                        .then(whitespace().or_not()),
                )
                .then(this.clone())
                .map(|(input, output)| TypeSignature::Function(input, Box::new(output)));

            let named = TypePath::parser().map(TypeSignature::Named);

            choice((
                unit,
                array,
                generic_application,
                tuple,
                reference,
                function,
                named,
            ))
        })
    }
}
