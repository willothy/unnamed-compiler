use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{BTreeMap, HashMap},
    path::PathBuf,
    rc::Rc,
    sync::{RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use chumsky::{
    error::Rich,
    primitive::{choice, just},
    text::{digits, ident, keyword},
    IterParser as _, ParseResult, Parser,
};
use lasso::{Rodeo, Spur};

use crate::{
    expr::Expr,
    ty::{TypePath, TypeSignature},
    NodeParser,
};

/// A top-level import, e.g. `use package::path::to::type;`
#[derive(Debug, PartialEq, Eq)]
pub struct Import {
    pub path: TypePath,
    pub alias: Option<Spur>,
}

impl<'a> NodeParser<'a, Self> for Import {
    fn parser() -> impl crate::Parser<'a, Self> {
        keyword("use")
            .ignore_then(TypePath::parser().padded())
            .then(
                keyword("as")
                    .padded()
                    .ignore_then(ident().map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    }))
                    .padded()
                    .or_not(),
            )
            .map(|(path, alias)| Import { path, alias })
    }
}

/// A module at AST level, with no type information or import resolution.
#[derive(Debug)]
pub struct Module {
    pub name: Spur,
    pub source: PathBuf,
    pub declarations: BTreeMap<Spur, Declaration>,
    pub imports: Vec<Import>,
    pub submodules: BTreeMap<Spur, Module>,
    // TODO: abstract over file cache to fetch files
    file_cache: Rc<RefCell<HashMap<PathBuf, Rc<str>>>>,
    // TODO: multithreading (later on, but using RwLock now should make that easier)
    interner: Rc<RwLock<Rodeo>>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.source == other.source
    }
}

impl<'a> Default for Module {
    fn default() -> Self {
        Self::new("main", PathBuf::new(), None, None)
    }
}

impl Module {
    pub fn new(
        name: &str,
        source: PathBuf,
        cache: Option<Rc<RefCell<HashMap<PathBuf, Rc<str>>>>>,
        interner: Option<Rc<RwLock<Rodeo>>>,
    ) -> Self {
        let interner = interner.unwrap_or_default();
        let name = interner
            .write()
            .expect("to lock interner")
            .get_or_intern(name);
        Self {
            name,
            source,
            imports: Vec::new(),
            declarations: BTreeMap::new(),
            submodules: BTreeMap::new(),
            file_cache: cache.unwrap_or_default(),
            interner,
        }
    }

    pub fn file_cache(&self) -> Ref<'_, HashMap<PathBuf, Rc<str>>> {
        self.file_cache.borrow()
    }

    pub fn file_cache_mut(&mut self) -> RefMut<'_, HashMap<PathBuf, Rc<str>>> {
        self.file_cache.borrow_mut()
    }

    pub fn interner(&self) -> RwLockReadGuard<'_, Rodeo> {
        self.interner.read().expect("to lock interner")
    }

    pub fn interner_mut(&mut self) -> RwLockWriteGuard<'_, Rodeo> {
        self.interner.write().expect("to lock interner")
    }

    pub fn source(&self) -> &PathBuf {
        &self.source
    }

    pub fn name<'a>(&self) -> Spur {
        self.name
    }

    pub fn declarations(&self) -> impl Iterator<Item = &Declaration> {
        self.declarations.values()
    }

    pub fn declarations_mut(&mut self) -> impl Iterator<Item = &mut Declaration> {
        self.declarations.values_mut()
    }

    pub fn declaration(&self, name: &str) -> Option<&Declaration> {
        let name = self
            .interner
            .write()
            .expect("to get a read lock")
            .get_or_intern(name);
        self.declarations.get(&name)
    }

    pub fn declaration_mut(&mut self, name: &str) -> Option<&mut Declaration> {
        let name = self
            .interner
            .write()
            .expect("to get a read lock")
            .get_or_intern(name);
        self.declarations.get_mut(&name)
    }

    pub fn insert(&mut self, decl: Declaration) {
        self.declarations.insert(decl.name().into(), decl);
    }

    pub fn import(&mut self, path: TypePath, alias: Option<Spur>) {
        self.imports.push(Import { path, alias });
    }

    pub fn add_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    pub fn add_imports(&mut self, imports: Vec<Import>) {
        self.imports.extend(imports);
    }

    pub fn add_submodule(&mut self, module: Module) {
        self.submodules.insert(module.name, module);
    }

    pub fn submodule(&self, name: &str) -> Option<&Module> {
        let name = self
            .interner
            .write()
            .expect("to get a read lock")
            .get_or_intern(name);
        self.submodules.get(&name)
    }

    pub fn submodule_mut(&mut self, name: &str) -> Option<&mut Module> {
        let name = self
            .interner
            .write()
            .expect("to get a read lock")
            .get_or_intern(name);
        self.submodules.get_mut(&name)
    }

    pub fn parse_submodules(&mut self) {
        for (_, module) in self.submodules.iter_mut() {
            let input = self.file_cache.borrow();
            // TODO: Handle submodules properly with subdirs and files
            let input = input.get(&module.source).unwrap();
            module.parse(input);
        }
    }

    pub fn parse<'a>(&mut self, input: &'a str) -> ParseResult<(), Rich<'a, char>> {
        Self::parser()
            .or_not()
            .padded()
            .map(|_| ())
            .parse_with_state(input, self)
    }
}

impl<'a> NodeParser<'a, ()> for Module {
    fn parser() -> impl crate::Parser<'a, ()> {
        let decl = Declaration::parser();

        let r#use = Import::parser();

        decl.map_with(|d, e| match d {
            Declaration::Module { name } => {
                let state = e.state();
                // TODO: Handle submodules properly with subdirs and files
                let interner = state.interner.read().expect("to get a read lock");
                let name_str = interner.resolve(&name);
                let src = state.source.join(name_str).with_extension("cr");
                state.submodules.insert(
                    name,
                    Module::new(
                        name_str,
                        src,
                        Some(Rc::clone(&state.file_cache)),
                        Some(Rc::clone(&state.interner)),
                    ),
                );
            }
            _ => {
                e.state().declarations.insert(d.name().into(), d);
            }
        })
        .or(r#use.map_with(|i, e| {
            e.state().imports.push(i);
        }))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Variant {
    /// A unit variant with no attached data, e.g. `UnitVariant`
    Unit,
    /// A tuple-style variant, e.g. `TupleVariant(int, int)`
    Tuple(Vec<TypeSignature>),
    /// A struct-style variant, e.g. `StructVariant { field: int }`
    Struct(BTreeMap<Spur, TypeSignature>),
    /// A variant with a single value, e.g. `CStyleVariant = 42`
    Integer(i64),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function {
        name: Spur,
        params: Vec<(Spur, TypeSignature)>,
        generic_params: Option<Vec<Spur>>,
        ret: Option<TypeSignature>,
        body: Expr,
    },
    Struct {
        name: Spur,
        generic_params: Option<Vec<Spur>>,
        value: Variant,
    },
    Enum {
        name: Spur,
        generic_params: Option<Vec<Spur>>,
        variants: Vec<(Spur, Variant)>,
    },
    Union {
        name: Spur,
        generic_params: Option<Vec<Spur>>,
        variants: BTreeMap<Spur, TypeSignature>,
    },
    Static {
        name: Spur,
        ty: TypeSignature,
        value: Expr,
    },
    Constant {
        name: Spur,
        ty: TypeSignature,
        value: Expr,
    },
    /// A type alias, e.g. `type Foo<T> = Vec<T>;`
    TypeAlias {
        name: Spur,
        /// The generic parameters of the type alias, if any.
        ///
        /// The `T` in `type Foo<T> = Vec<T>;` is a generic parameter.
        generic_params: Option<Vec<Spur>>,
        /// The type the alias points to. Unresolved at this stage.
        ty: TypeSignature,
    },
    Module {
        name: Spur,
    },
}

impl Declaration {
    pub fn name(&self) -> Spur {
        match self {
            Declaration::Function { name, .. }
            | Declaration::Struct { name, .. }
            | Declaration::Enum { name, .. }
            | Declaration::Union { name, .. }
            | Declaration::Static { name, .. }
            | Declaration::Constant { name, .. }
            | Declaration::TypeAlias { name, .. } => *name,
            Declaration::Module { name } => *name,
        }
    }
}

impl<'a> NodeParser<'a, Self> for Declaration {
    fn parser() -> impl crate::Parser<'a, Self> {
        let generic_params = ident()
            .map_with(|s, e| {
                let state: &mut Module = e.state();
                let mut interner = state.interner.write().expect("to get a write lock");
                interner.get_or_intern(s)
            })
            .padded()
            .separated_by(just(",").padded())
            .collect::<Vec<_>>()
            .delimited_by(just("<").padded(), just(">").padded())
            .padded()
            .or_not();

        let r#fn = keyword("fn")
            .ignore_then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
                    .padded()
                    .then(generic_params),
            )
            .then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
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
            .map_with(|s, e| {
                let state: &mut Module = e.state();
                let mut interner = state.interner.write().expect("to get a write lock");
                interner.get_or_intern(s)
            })
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
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
                    .padded()
                    .then(generic_params)
                    .then(
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
            .ignore_then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
                    .padded(),
            )
            .then(generic_params)
            .then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
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
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
                    .padded()
                    .then(generic_params)
                    .then(
                        ident()
                            .map_with(|s, e| {
                                let state: &mut Module = e.state();
                                let mut interner =
                                    state.interner.write().expect("to get a write lock");
                                interner.get_or_intern(s)
                            })
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

        let r#mod = keyword("mod")
            .ignore_then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
                    .padded(),
            )
            .map(|name| Declaration::Module { name });

        let r#static = keyword("static")
            .ignore_then(
                ident()
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
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
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
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
                    .map_with(|s, e| {
                        let state: &mut Module = e.state();
                        let mut interner = state.interner.write().expect("to get a write lock");
                        interner.get_or_intern(s)
                    })
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
            .or(r#mod)
    }
}
