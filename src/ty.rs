use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just},
    recursive::recursive,
    text::{ident, whitespace},
    IterParser, Parser,
};

use crate::{util::comma_separated, NodeParser};

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
    pub name: &'a str,
    /// The path to the type, if any, starting with the package name
    /// or `package`, `self` or `super`.
    pub path: Option<Vec<TypePathSegment<'a>>>,
}

impl<'a> TypePath<'a> {
    pub fn is_valid(&self) -> bool {
        use TypePathSegment::*;
        let mut package = false;
        match &self.path {
            Some(path) => path.iter().enumerate().all(|(i, segment)| match segment {
                // `package` and `self` are only valid at the start of the path.
                Package | SelfModule => {
                    package = true;
                    i == 0
                }
                // You could technically do `super::super::super::type,` but not
                // `super::super::super::super`
                //
                // You can also not do `package::super::type`, because `package` is
                // the root module.
                SuperModule => !package && i < path.len() - 1,
                _ => true,
            }),
            None => true,
        }
    }
}

impl<'a> NodeParser<'a, TypePathSegment<'a>> for TypePathSegment<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone + 'a {
        ident().map(|s| match s {
            "package" => Self::Package,
            "self" => Self::SelfModule,
            "super" => Self::SuperModule,
            name => Self::Ident(name),
        })
    }
}

impl<'a> NodeParser<'a, TypePath<'a>> for TypePath<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone + 'a {
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
pub enum TypeSignature<'a> {
    Unit,
    // A named type, e.g. `package::path::to::type` or `type`
    // This is the only type that can be a generic parameter, so generics will
    // need to be used in resolution of some of these names.
    Named(TypePath<'a>),
    // Struct, enum or union types with generic parameters
    GenericApplication(TypePath<'a>, Vec<TypeSignature<'a>>),
    Tuple(Vec<TypeSignature<'a>>),
    Array(Box<TypeSignature<'a>>),
    Function(Vec<TypeSignature<'a>>, Box<TypeSignature<'a>>),
    Reference(Box<TypeSignature<'a>>),
}

impl<'a> NodeParser<'a, TypeSignature<'a>> for TypeSignature<'a> {
    fn parser() -> impl Parser<'a, &'a str, Self, extra::Err<Rich<'a, char>>> + Clone + 'a {
        recursive(|this| {
            let unit = just("()").map(|_| TypeSignature::Unit);

            let array = just("[")
                .ignore_then(this.clone())
                .then_ignore(just("]"))
                .map(|t| TypeSignature::Array(Box::new(t)));

            let generic_application = TypePath::parser()
                .then(
                    this.clone()
                        .separated_by(just(",").then_ignore(whitespace()))
                        .collect::<Vec<_>>()
                        .delimited_by(just("<"), just(">"))
                        .boxed(),
                )
                .map(|(name, args)| TypeSignature::GenericApplication(name, args));

            let tuple = this
                .clone()
                .separated_by(just(",").then_ignore(whitespace()))
                .collect::<Vec<_>>()
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
