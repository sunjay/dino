//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

use std::fmt;
use std::sync::Arc;

use crate::span::Span;

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
    All(Span),
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
    SelfValue(Span),
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
    Return(Box<Return>),
    Break(Span),
    Continue(Span),
    Block(Box<Block>),
    StructLiteral(StructLiteral),
    BStrLiteral(Literal<Arc<[u8]>>),
    IntegerLiteral(IntegerLiteral),
    RealLiteral(Literal<f64>),
    ComplexLiteral(Literal<f64>),
    BoolLiteral(Literal<bool>),
    UnitLiteral(Span),
    SelfValue(Span),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    /// The span of the `return` keyword
    pub return_span: Span,
    /// The expression being returned (optional)
    pub expr: Option<Expr>
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
    ///
    /// If `None`, the expression is a variable with the same name as the field
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub suffix: Option<LiteralSuffix>,
    /// The span for the entire integer literal, including its suffix
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralSuffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit(Span),
    SelfType(Span),
    Named(Path),
}

/// A type explicitly named with an identifier or path (as opposited to (), [T], etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum NamedTy {
    SelfType(Span),
    Named(Path),
}

impl From<NamedTy> for Ty {
    fn from(ty: NamedTy) -> Self {
        match ty {
            NamedTy::SelfType(span) => Ty::SelfType(span),
            NamedTy::Named(path) => Ty::Named(path),
        }
    }
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
    Package(Span),
    /// The `Self` keyword
    SelfType(Span),
    /// The `self` keyword
    SelfValue(Span),
    /// The `super` keyword
    Super(Span),
}

impl fmt::Display for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PathComponent::*;
        match self {
            Ident(ident) => write!(f, "{}", ident),
            Package(_) => write!(f, "package"),
            SelfType(_) => write!(f, "Self"),
            SelfValue(_) => write!(f, "self"),
            Super(_) => write!(f, "super"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<T> {
    pub value: T,
    pub span: Span,
}

impl<T: fmt::Display> fmt::Display for Literal<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub value: Arc<str>,
    pub span: Span,
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
