use std::collections::HashMap;

/// A full package with submodules.
pub struct Module {
    pub name: String,
    pub submodules: Vec<Module>,
    pub declarations: HashMap<String, Declaration>,
}

/// A module-level declaration.
pub enum Declaration {
    Function {
        parameters: Vec<(String, TypeName)>,
        return_ty: TypeName,
        body: Expression,
    },
    Constant {
        ty: TypeName,
        value: Expression,
    },
    Struct {
        fields: Vec<(String, TypeName)>,
    },
    TypeAlias {
        ty: TypeName,
    },
}

/// A type annotation at the AST level. Represents the parsed form of a type in argument, return
/// type or variable type annotation position, before it is resolved. For example,
/// `List<T>` would be represented as a `TypeAnnotation::Constructor` with `base` set to
/// `TypeAnnotation::Concrete(TypeName { name: "List", path: ... })` and
/// `parameters` set to `[TypeAnnotation::Variable("T")]`.
pub enum TypeAnnotation {
    Unit,
    /// A concrete named type, such as `bool` or `std::Vec`.
    Concrete(TypeName),
    /// A tuple type, such as `(T, U)`.
    Tuple(Vec<TypeAnnotation>),
    /// An array type, such as `[T]`.
    Array(Box<TypeAnnotation>),
    /// A reference to a value, such as `&T`.
    Reference(Box<TypeAnnotation>),
    /// A type variable, such as `T`.
    Variable(String),
    /// A generic type constructor, such as `std::Vec<T>`.
    Constructor {
        /// The base type path, such as `std::Vec` in `std::Vec<T>`.
        ty: TypeName,
        /// The type parameters to the constructor, such as `T` in `std::Vec<T>`.
        parameters: Vec<TypeAnnotation>,
    },
    /// A function type, such as `fn() -> bool`.
    Function {
        parameters: Vec<TypeAnnotation>,
        return_ty: Box<TypeAnnotation>,
    },
}

/// Represents the full path to a type, including the name of the type itself.
pub struct TypeName {
    pub name: String,
    pub path: TypePath,
}

/// Represents the path to a type, not including the name of the type itself.
pub enum TypePath {
    /// Absolute paths are relative to the root module of the current package, and start with `::`.
    Absolute(Vec<String>),
    /// Relative paths are relative to the current module, and start with `self::`.
    Relative(Vec<String>),
    /// External paths are relative to the root module of an external package/module.
    ///
    /// NOTE: All external paths are resolved from unqualified paths before AST lowering. Thus,
    /// there is not way to specify an external path in a text format.
    External(Vec<String>),
    /// Unqualified paths are relative to the current module or the module specified by the root of
    /// the path. The first element is the name of the package, and the rest are the
    /// path relative to the root module of that package. If the name exists in the current module,
    /// it is preferred over an external package to avoid name conflicts.
    ///
    /// NOTE: Unqualified paths need to be resolved to either an absolute, relative, or external paths before
    /// AST lowering.
    Unqualified(Vec<String>),
}

/// A primitive literal value.
pub enum Literal {
    /// A unit literal, denoted by `()`.
    Unit,
    /// An n-bit integer literal, such as `1` or `0b1010`. The default type is `i32`.
    ///
    /// Prefixes:
    /// - Decimal: `1`
    /// - Binary: `0b1010`
    /// - Octal: `0o755`
    /// - Hexadecimal: `0xff`
    /// - Byte: `b'A'`
    ///
    /// Suffices:
    ///   num  | signedness | size
    ///   1    | [u,i]      | [8,16,32,64,128,size]
    ///
    /// Examples:
    /// - `1usize` is an unsigned, pointer-sized integer with value `1`.
    /// - `0b1010` is the default integer type, with value `10`.
    /// - `0xffu8` is an 8-bit unsigned integer with value `255`.
    Int(i64),
    /// A floating-point literal, such as `1.0`, `2.5f32` or `1.0e10`. The default type is `f64`.
    ///
    /// Suffices:
    ///  num  | magnitude | size
    ///  1.1  | e[n]      | f[32,64]
    Float(f32),
    /// A string literal, such as `"foo"`.
    Str(String),
    /// A character literal, such as `'a'`.
    Char(char),
    /// A boolean literal, either `true` or `false`.
    Bool(bool),
}

/// A statement in a block.
pub enum Statement {
    /// A variable binding, such as `let foo = 1`.
    VariableBinding {
        pattern: LhsExpression,
        value: Expression,
    },
    /// A destructuring assignment, such as `let (x, y) = foo`.
    DestructuringAssignment {
        pattern: LhsExpression,
        value: Expression,
    },
    /// An expression statement, such as `foo(1, 2)`.
    Expression(Expression),
    /// A return statement, such as `return 1`.
    Return(Expression),
    /// A break statement, such as `break` or `break 5`.
    Break {
        /// Optional label to break out of. For example, in `foo: loop { break foo }`, `foo` is the label. If not
        /// specified, breaks out of the innermost loop.
        block: Option<String>,
        /// Optional value to return from the loop. For example, in `loop { break 5 }`, `5` is the value. If not
        /// specified, results in `()` if captured by a variable.
        ///
        /// NOTE: Not allowed in `for` or `while` loops as their output cannot be guaranteed at
        /// compile time.
        value: Option<Expression>,
    },
    /// A continue statement in a loop.
    Continue,
    /// A while loop, such as `while foo { bar }`.
    While {
        label: Option<String>,
        condition: Expression,
        body: Expression,
    },
    /// A for loop, such as `for x in foo { bar }`.
    For {
        label: Option<String>,
        pattern: LhsExpression,
        iterator: Expression,
        body: Expression,
    },
}

/// A unary operator.
pub enum UnaryOperator {
    /// Represents the - in a negation expression, such as `-foo`.
    Negate, // -
    /// Represents the ! in a boolean not expression, such as `!foo`.
    LogicalNot, // !
    /// Represents the ~ in a bitwise not expression, such as `~foo`.
    BitwiseNot, // ~
    /// Represents the * in a dereference expression, such as `*foo`.
    Deref, // *
    /// Represents the & in a reference to a value, such as `&foo`.
    Ref, // &
}

/// A binary operator.
pub enum BinaryOperator {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // **

    // Comparison
    Equal,          // ==
    NotEqual,       // !=
    GreaterThan,    // >
    GreaterOrEqual, // >=
    LessThan,       // <
    LessOrEqual,    // <=

    // Logical (short-circuiting)
    LogicalOr,  // ||
    LogicalAnd, // &&

    // Bitwise
    BitwiseOr,  // |
    BitwiseAnd, // &
    Xor,        // ^
}

/// An expression which results in a value.
pub enum Expression {
    /// A primitive literal, such as `1`, `"foo"`, or `()`.
    Literal { value: Literal },
    /// Represents the use of a variable, such as `foo`.
    Variable { name: String },
    Unary {
        op: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        op: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// A call expression, such as `foo(1, 2)`.
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    /// A struct init (or struct literal), such as `Point { x: 1, y: 2 }`.
    StructInit {
        ty: TypeName,
        fields: Vec<(String, Expression)>,
    },
    /// An array init (or array literal), such as `[1, 2]`.
    ArrayInit {
        ty: TypeName,
        elements: Vec<Expression>,
    },
    /// A tuple init (or tuple literal), such as `(1, 2)`.
    TupleInit { fields: Vec<Expression> },
    /// A struct field access expression. For example, in `foo.bar`, `foo` (the
    /// `struct_expr`) is an `Expression::Variable`, and `bar` is the `field` in `Expression::FieldAccess`.
    StructAccess {
        struct_expr: Box<Expression>,
        field: String,
    },
    IndexAccess {
        array_expr: Box<Expression>,
        index_expr: Box<Expression>,
    },
    /// A tuple field access expression. For example, in `foo.0`, `foo` (the `tuple_expr`) is
    /// an `Expression::Variable`, and `0` is the `index` in `Expression::TupleFieldAccess`.
    TupleAccess {
        tuple_expr: Box<Expression>,
        index: usize,
    },
    /// A block expression, such as `{ foo; bar }`.
    Block {
        statements: Vec<Statement>,
        /// The value of the block is the value of the last statement in the block.
        value: Box<Expression>,
    },
    /// A match expression, such as `match foo { 1 => 2, _ => 3 }`.
    Match {
        value: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    /// An if expression, such as `if foo { 1 } else { 2 }`.
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        // Required for if-else expressions, optional in expression statements.
        otherwise: Option<Box<Expression>>,
    },
    /// A loop expression, such as `loop { break foo; }`.
    Loop { body: Box<Expression> },
}

pub struct MatchArm {
    pub pattern: LhsExpression,
    pub body: Expression,
}

/// A type expression in pattern matching, or a left-hand side expression in a destructuring assignment.
pub enum LhsExpression {
    /// A unit literal in a pattern. For example, in
    /// `() = foo`, `()` is a `LhsExpression::Unit`.
    ///
    /// NOTE: Not allowed in destructuring assignments.
    Unit,
    /// A wildcard in a destructuring assignment. For example, in `let (_, y) = foo`,
    /// `_` is a `LhsExpression::Wildcard`.
    Wildcard,
    /// A variable binding in pattern matching or assignment. For example, in
    /// `foo = 1`, `foo` is a `LhsExpression::Variable`. Also applies to destructuring, where the
    /// variable is bound to the value being destructured. For example, in
    /// `let (x, y) = foo`, `x` and `y` are both `LhsExpression::Variable`s.
    Variable(String),
    /// A field access on the left-hand side of an assignment. For example, in
    /// `foo.bar = 1`, `foo.bar` is a `LhsExpression::FieldAccess`.
    ///
    /// NOTE: Not allowed to be nested inside a struct or tuple destructuring, or in pattern matching.
    StructAccess {
        struct_expr: Expression,
        field: String,
    },
    /// A tuple field access on the left-hand side of an assignment. For example, in
    /// `foo.0 = 1`, `foo.0` is a `LhsExpression::TupleAccess`.
    TupleAccess {
        tuple_expr: Expression,
        index: usize,
    },
    /// An index access on the left-hand side of an assignment. For example, in
    /// `foo[0] = 1`, `foo[0]` is a `LhsExpression::IndexAccess`.
    /// NOTE: Not allowed to be nested inside a struct or tuple destructuring, or in pattern matching.
    IndexAccess {
        array_expr: Expression,
        index_expr: Expression,
    },
    /// A call expression on the left-hand side of an assignment. For example, in
    /// `*foo(1) = 1`, `foo(1)` is a `LhsExpression::Call` containing a `LhsExpression::Variable`.
    /// NOTE: Not allowed to be nested inside a struct or tuple destructuring, or in pattern matching.
    /// Must be nested inside a `LhsExpression::Deref`, `LhsExpression::FieldAccess` or similar.
    Call {
        function: Expression,
        arguments: Vec<Expression>,
    },
    /// A dereference on the left-hand side of an assignment. For example, in
    /// `*foo = 1`, `*foo` is a `LhsExpression::Deref` containing a `LhsExpression::Variable`.
    ///
    /// NOTE: Not allowed to be nested inside a struct or tuple destructuring, or in pattern matching.
    Deref { inner: Expression },
    /// A struct destructuring on the left-hand side of an assignment. For example, in
    /// `let Point { x, y } = foo`, `Point { x, y }` is a `LhsExpression::Struct` containing two
    /// `LhsExpression::Variable`s.
    Struct {
        ty: TypeName,
        /// Destructuring can be nested (e.g. `Rect { origin: Point { x, y }, size }`), where
        /// origin is destructured into `x` and `y`, and size is bound directly to `size`.
        fields: Vec<LhsExpression>,
    },
    /// A tuple destructuring on the left-hand side of an assignment, such as
    /// `let (x, y) = foo`.
    Tuple(Vec<LhsExpression>),
    /// An enum variant destructuring on the left-hand side of an assignment. For
    /// example, in `let Some(x) = foo`, `Some(x)` is a `LhsExpression::EnumVariant` containing a
    /// `LhsExpression::Variable`.
    EnumVariant {
        ty: TypeName,
        variant: String,
        fields: Vec<LhsExpression>,
    },
}

fn main() {}
