//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

use std::fmt;
use std::sync::Arc;

/// Represents a single module within the current package
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Import(ImportPath),
    Struct(Struct),
    Impl(Impl),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportPath {
    /// The path to import from (represents the root module if empty)
    pub path: Vec<PathComponent>,
    /// The items selected from the path
    pub selection: ImportSelection,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelection {
    /// A single path component
    Component(PathComponent),
    /// A specific list of names being imported
    Names(Vec<ImportName>),
    /// A wildcard import (all items)
    All,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportName {
    Name {name: Ident, alias: Option<Ident>},
    SelfValue {alias: Option<Ident>},
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    /// The name of the struct
    pub name: Ident,
    /// The fields of the struct
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl {
    /// The Self type of this impl block
    pub self_ty: Ty,
    /// The method decls of this impl block
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub sig: FuncSig,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncSig {
    pub params: Vec<FuncParam>,
    pub return_type: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncParam {
    SelfValue,
    Named {
        name: Ident,
        ty: Ty,
    },
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Block {
    pub decls: Vec<Decl>,
    pub stmts: Vec<Stmt>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr>,
}

impl Block {
    pub fn is_empty(&self) -> bool {
        let Block {decls, stmts, ret} = self;
        decls.is_empty() && stmts.is_empty() && ret.is_none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Cond(Cond),
    WhileLoop(WhileLoop),
    VarDecl(VarDecl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    /// The condition for which the loop is expected to continue
    pub cond: Expr,
    /// The body of the loop, executed until the condition is false
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    /// The identifier to assign a value to
    pub name: Ident,
    /// The type of the variable (or None if the type is to be inferred)
    pub ty: Option<Ty>,
    /// The expression for the value to assign to the variable
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Box<Assign>),
    RangeOp(Option<Box<Expr>>, RangeOp, Option<Box<Expr>>),
    BoolOp(Box<Expr>, BoolOp, Box<Expr>),
    CompareOp(Box<Expr>, CompareOp, Box<Expr>),
    BitwiseOp(Box<Expr>, BitwiseOp, Box<Expr>),
    NumericOp(Box<Expr>, NumericOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    CastAs(Box<Expr>, Ty),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    Cond(Box<Cond>),
    Call(Box<FuncCall>),
    Index(Box<Index>),
    Return(Option<Box<Expr>>),
    Break,
    Continue,
    Block(Box<Block>),
    StructLiteral(StructLiteral),
    BStrLiteral(Arc<[u8]>),
    IntegerLiteral(IntegerLiteral),
    RealLiteral(f64),
    ComplexLiteral(f64),
    BoolLiteral(bool),
    UnitLiteral,
    SelfValue,
    Path(Path),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RangeOp {
    /// The `..` operator
    Exclusive,
    /// The `..=` operator
    Inclusive,
}

/// All boolean binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    /// The `||` operator
    Or,
    /// The `&&` operator
    And,
}

/// All comparison operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompareOp {
    /// The `==` operator
    Eq,
    /// The `!=` operator
    Ne,
    /// The `<` operator
    Lt,
    /// The `<=` operator
    Le,
    /// The `>` operator
    Gt,
    /// The `>=` operator
    Ge,
}

/// All bitwise operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BitwiseOp {
    /// The `|` operator (bitwise OR)
    Or,
    /// The `~` operator (bitwise XOR)
    Xor,
    /// The `&` operator (bitwise AND)
    And,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
}

/// All numeric operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumericOp {
    /// The `+` operator
    Add,
    /// The `-` operator
    Sub,
    /// The `*` operator
    Mul,
    /// The `/` operator
    Div,
    /// The `%` operator
    Rem,
    /// The `^` operator
    Pow,
}

/// All unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// The `+` operator
    Pos,
    /// The `-` operator
    Neg,
    /// The `!` operator
    Not,
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lhs: Expr,
    /// The expression for the value to assign to the left-hand side
    pub rhs: Expr,
}

/// A method call in the form `<expr> . <call-expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall {
    /// The expression of the left-hand side of the method call
    pub lhs: Expr,
    /// The method being called
    pub method_name: Ident,
    /// The arguments to the method call
    pub args: Vec<Expr>,
}

/// A field access in the form `<expr> . <ident>`
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    /// The expression of the left-hand side of the field access
    pub lhs: Expr,
    /// The field being accessed
    pub field: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr, Block)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    /// The value being called
    pub value: Expr,
    /// The arguments passed to the value
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    /// The value being indexed
    pub value: Expr,
    /// The index expression
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: NamedTy,
    pub field_values: Vec<StructFieldValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldValue {
    /// The name of the field
    pub name: Ident,
    /// The expression being assigned to the field
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub suffix: Option<LiteralSuffix>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralSuffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

/// A type explicitly named with an identifier or path (as opposited to (), [T], etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum NamedTy {
    SelfType,
    Named(Path),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit,
    SelfType,
    Named(Path),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    /// The components of the path (guaranteed to be non-empty)
    pub components: Vec<PathComponent>,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {components} = self;

        write!(f, "{}", components[0])?;
        for comp in &components[1..] {
            write!(f, "::{}", comp)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathComponent {
    Ident(Ident),
    /// The `package` keyword
    Package,
    /// The `Self` keyword
    SelfType,
    /// The `self` keyword
    SelfValue,
    /// The `super` keyword
    Super,
}

impl fmt::Display for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PathComponent::*;
        match self {
            Ident(ident) => write!(f, "{}", ident),
            Package => write!(f, "package"),
            SelfType => write!(f, "Self"),
            SelfValue => write!(f, "self"),
            Super => write!(f, "super"),
        }
    }
}

pub type Ident = Arc<str>;
