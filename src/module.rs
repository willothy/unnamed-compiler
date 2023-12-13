use std::collections::BTreeMap;

use crate::{
    expr::{Expr, VariantValue},
    ty::TypePath,
    TypeSignature,
};

#[derive(Debug, PartialEq)]
pub enum Declaration<'a> {
    Function {
        name: &'a str,
        ty: TypeSignature<'a>,
        body: Expr<'a>,
    },
    Struct {
        name: &'a str,
        fields: BTreeMap<&'a str, TypeSignature<'a>>,
    },
    Enum {
        name: &'a str,
        variants: Vec<(&'a str, VariantValue<'a>)>,
    },
    Union {
        name: &'a str,
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
        generic_params: Vec<&'a str>,
        /// The type the alias points to. Unresolved at this stage.
        ty: TypeSignature<'a>,
    },
    /// A top-level import, e.g. `use package::path::to::type;`
    ///
    /// Not sure if this should be in [`Declaration`] or not...
    Use { path: TypePath<'a> },
}
