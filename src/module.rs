use std::collections::BTreeMap;

use chumsky::{
    error::Rich,
    extra,
    primitive::{choice, just, todo},
    text::{digits, ident, keyword},
    IterParser as _, Parser,
};

use crate::{
    expr::Expr,
    ty::{TypePath, TypeSignature},
    NodeParser,
};

/// A top-level import, e.g. `use package::path::to::type;`
#[derive(Debug, PartialEq, Eq)]
pub struct Import<'a> {
    pub path: TypePath<'a>,
    pub alias: Option<&'a str>,
}

impl<'a> NodeParser<'a, Self> for Import<'a> {
    fn parser() -> impl crate::Parser<'a, Self> {
        keyword("use")
            .ignore_then(TypePath::parser().padded())
            .then(
                keyword("as")
                    .padded()
                    .ignore_then(ident())
                    .padded()
                    .or_not(),
            )
            .map(|(path, alias)| Import { path, alias })
    }
}

/// A module at AST level, with no type information or import resolution.
pub struct Module<'a> {
    pub name: &'a str,
    pub declarations: BTreeMap<&'a str, Declaration<'a>>,
    pub imports: Vec<Import<'a>>,
}

impl<'a> Default for Module<'a> {
    fn default() -> Self {
        Self::new("main")
    }
}

impl<'a> Module<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            declarations: BTreeMap::new(),
            imports: Vec::new(),
        }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Declaration<'a>> {
        self.declarations.values()
    }

    pub fn declarations_mut(&mut self) -> impl Iterator<Item = &mut Declaration<'a>> {
        self.declarations.values_mut()
    }

    pub fn declaration(&self, name: &str) -> Option<&Declaration<'a>> {
        self.declarations.get(name)
    }

    pub fn declaration_mut(&mut self, name: &str) -> Option<&mut Declaration<'a>> {
        self.declarations.get_mut(name)
    }

    pub fn insert(&mut self, decl: Declaration<'a>) {
        self.declarations.insert(decl.name(), decl);
    }

    pub fn import(&mut self, path: TypePath<'a>, alias: Option<&'a str>) {
        self.imports.push(Import { path, alias });
    }

    pub fn add_import(&mut self, import: Import<'a>) {
        self.imports.push(import);
    }

    pub fn add_imports(&mut self, imports: Vec<Import<'a>>) {
        self.imports.extend(imports);
    }
}

impl<'a> NodeParser<'a, Self> for Module<'a> {
    fn parser(
    ) -> impl Parser<'a, &'a str, Self, extra::Full<Rich<'a, char>, Module<'a>, ()>> + Clone + 'a
    {
        // let decl = Declaration::parser();
        //
        // let r#use = Import::parser();
        //
        // decl.map_with(|d, e| {
        //     state.declarations.insert(d.name(), d);
        //     // todo
        // })
        todo()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Variant<'a> {
    /// A unit variant with no attached data, e.g. `UnitVariant`
    Unit,
    /// A tuple-style variant, e.g. `TupleVariant(int, int)`
    Tuple(Vec<TypeSignature<'a>>),
    /// A struct-style variant, e.g. `StructVariant { field: int }`
    Struct(BTreeMap<&'a str, TypeSignature<'a>>),
    /// A variant with a single value, e.g. `CStyleVariant = 42`
    Integer(i64),
}

#[derive(Debug, PartialEq)]
pub enum Declaration<'a> {
    Function {
        name: &'a str,
        params: Vec<(&'a str, TypeSignature<'a>)>,
        generic_params: Option<Vec<&'a str>>,
        ret: Option<TypeSignature<'a>>,
        body: Expr<'a>,
    },
    Struct {
        name: &'a str,
        generic_params: Option<Vec<&'a str>>,
        value: Variant<'a>,
    },
    Enum {
        name: &'a str,
        generic_params: Option<Vec<&'a str>>,
        variants: Vec<(&'a str, Variant<'a>)>,
    },
    Union {
        name: &'a str,
        generic_params: Option<Vec<&'a str>>,
        variants: BTreeMap<&'a str, TypeSignature<'a>>,
    },
    Static {
        name: &'a str,
        ty: TypeSignature<'a>,
        value: Expr<'a>,
    },
    Constant {
        name: &'a str,
        ty: TypeSignature<'a>,
        value: Expr<'a>,
    },
    /// A type alias, e.g. `type Foo<T> = Vec<T>;`
    TypeAlias {
        name: &'a str,
        /// The generic parameters of the type alias, if any.
        ///
        /// The `T` in `type Foo<T> = Vec<T>;` is a generic parameter.
        generic_params: Option<Vec<&'a str>>,
        /// The type the alias points to. Unresolved at this stage.
        ty: TypeSignature<'a>,
    },
}

impl<'a> Declaration<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            Declaration::Function { name, .. }
            | Declaration::Struct { name, .. }
            | Declaration::Enum { name, .. }
            | Declaration::Union { name, .. }
            | Declaration::Static { name, .. }
            | Declaration::Constant { name, .. }
            | Declaration::TypeAlias { name, .. } => name,
        }
    }
}

impl<'a> NodeParser<'a, Self> for Declaration<'a> {
    fn parser(
    ) -> impl chumsky::prelude::Parser<'a, &'a str, Self, extra::Full<Rich<'a, char>, Module<'a>, ()>>
           + Clone
           + 'a {
        let generic_params = ident()
            .padded()
            .separated_by(just(",").padded())
            .collect::<Vec<_>>()
            .delimited_by(just("<").padded(), just(">").padded())
            .padded()
            .or_not();

        let r#fn = keyword("fn")
            .ignore_then(ident().padded().then(generic_params))
            .then(
                ident()
                    .padded()
                    .then_ignore(just(":").padded())
                    .then(TypeSignature::parser().padded())
                    .separated_by(just(",").padded())
                    .collect::<Vec<_>>()
                    .delimited_by(just("(").padded(), just(")").padded()),
            )
            .then(TypeSignature::parser().padded().or_not())
            .then(Expr::block_parser(Expr::parser()))
            .map(
                |((((name, generic_params), params), ret), body)| Declaration::Function {
                    name,
                    params,
                    ret,
                    body,
                    generic_params,
                },
            );

        let struct_variant = ident()
            .padded()
            .then_ignore(just(":").padded())
            .then(TypeSignature::parser().padded())
            .separated_by(just(",").padded())
            .allow_trailing()
            .collect::<BTreeMap<_, _>>()
            .padded()
            .delimited_by(just("{").padded(), just("}").padded())
            .map(|fields| Variant::Struct(fields));

        let tuple_variant = TypeSignature::parser()
            .padded()
            .separated_by(just(",").padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just("(").padded(), just(")").padded())
            .map(|fields| Variant::Tuple(fields));

        let r#struct = keyword("struct")
            .ignore_then(
                ident().padded().then(generic_params).then(
                    struct_variant
                        .clone()
                        .or(tuple_variant.clone())
                        .or_not()
                        .map(|variant| variant.unwrap_or(Variant::Unit)),
                ),
            )
            .map(|((name, generic_params), value)| Declaration::Struct {
                name,
                value,
                generic_params,
            });

        let int_variant = just("=")
            .padded()
            .ignore_then(digits(10).to_slice())
            .map(|s: &str| {
                let n = s.parse::<i64>().expect("to parse an integer");
                Variant::Integer(n)
            });

        let r#enum = keyword("enum")
            .padded()
            .ignore_then(ident().padded())
            .then(generic_params)
            .then(
                ident()
                    .padded()
                    .then(
                        choice((struct_variant, tuple_variant, int_variant))
                            .or_not()
                            .map(|variant| variant.unwrap_or(Variant::Unit)),
                    )
                    .separated_by(just(",").padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just("{").padded(), just("}").padded()),
            )
            .map(|((name, generic_params), variants)| Declaration::Enum {
                name,
                generic_params,
                variants,
            });

        let r#union = keyword("union")
            .ignore_then(
                ident().padded().then(generic_params).then(
                    ident()
                        .padded()
                        .then_ignore(just(":").padded())
                        .then(TypeSignature::parser().padded())
                        .separated_by(just(",").padded())
                        .collect::<BTreeMap<_, _>>()
                        .delimited_by(just("{").padded(), just("}").padded()),
                ),
            )
            .map(|((name, generic_params), variants)| Declaration::Union {
                name,
                generic_params,
                variants,
            });

        let r#static = keyword("static")
            .ignore_then(
                ident()
                    .padded()
                    .then_ignore(just(":").padded())
                    .then(TypeSignature::parser().padded())
                    .then_ignore(just("=").padded())
                    .then(Expr::parser().padded()),
            )
            .map(|((name, ty), value)| Declaration::Static { name, ty, value });

        let r#const = keyword("const")
            .ignore_then(
                ident()
                    .padded()
                    .then_ignore(just(":").padded())
                    .then(TypeSignature::parser().padded())
                    .then_ignore(just("=").padded())
                    .then(Expr::parser().padded()),
            )
            .map(|((name, ty), value)| Declaration::Constant { name, ty, value });

        let r#alias = keyword("type")
            .ignore_then(
                ident()
                    .padded()
                    .then(generic_params)
                    .then_ignore(just("=").padded())
                    .then(TypeSignature::parser().padded()),
            )
            .map(|((name, generic_params), ty)| Declaration::TypeAlias {
                name,
                generic_params,
                ty,
            });

        r#fn.or(r#struct)
            .or(r#enum)
            .or(r#union)
            .or(r#static)
            .or(r#const)
            .or(r#alias)
    }
}
