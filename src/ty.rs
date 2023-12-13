use chumsky::{primitive::just, text::ident, IterParser, Parser};

use crate::NodeParser;

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
