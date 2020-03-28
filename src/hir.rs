//! High-level IR - Completely Desugared AST

use std::sync::Arc;

use crate::ast;
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
    /// The prefix of the path (if any)
    pub prefix: Option<PathPrefix>,
    /// The path (within the prefix) to import from
    pub path: Vec<Ident>,
    /// The items selected from the path
    pub selection: ImportSelection,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelection {
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
    pub self_param: Option<Span>,
    pub params: Vec<FuncParam>,
    pub return_type: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub decls: Vec<Decl>,
    pub stmts: Vec<Stmt>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr>,
    /// The span of the entire block
    pub span: Span,
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
    BoolOr(Box<BoolOr>),
    BoolAnd(Box<BoolAnd>),
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    Cond(Box<Cond>),
    Call(Box<FuncCall>),
    Return(Box<Return>),
    Break(Span),
    Continue(Span),
    StructLiteral(StructLiteral),
    BStrLiteral(Arc<[u8]>, Span),
    IntegerLiteral(IntegerLiteral),
    RealLiteral(f64, Span),
    ComplexLiteral(f64, Span),
    BoolLiteral(bool, Span),
    UnitLiteral(Span),
    SelfValue(Span),
    Path(Path),
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lvalue: LValue,
    /// The expression for the value to assign to the left-hand side
    pub expr: Expr,
}

/// The short-circuiting `||` operator
#[derive(Debug, Clone, PartialEq)]
pub struct BoolOr {
    pub lhs: Expr,
    /// The span of the `||` operator
    pub span: Span,
    pub rhs: Expr,
}

/// The short-circuiting `&&` operator
#[derive(Debug, Clone, PartialEq)]
pub struct BoolAnd {
    pub lhs: Expr,
    /// The span of the `&&` operator
    pub span: Span,
    pub rhs: Expr,
}

/// Expressions that can be on the left-hand side of assignment
#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    FieldAccess(FieldAccess),
    Path(Path),
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
    /// The span of the entire method call
    pub span: Span,
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
    /// The span of the entire conditional expression
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    /// The value being called
    pub value: Expr,
    /// The arguments passed to the value
    pub args: Vec<Expr>,
    /// The span of the entire call expression
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    /// The span of the `return` keyword
    pub return_span: Span,
    /// The expression being returned (optional)
    pub expr: Option<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub name: NamedTy,
    pub field_values: Vec<StructFieldValue>,
    /// The span of the entire struct literal
    pub span: Span,
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

/// A type explicitly named with an identifier or path (as opposed to (), [T], etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum NamedTy {
    SelfType(Span),
    Named(Path),
}

pub type Path = ast::Path;
pub type PathPrefix = ast::PathPrefix;

pub type Ident = ast::Ident;
