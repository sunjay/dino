//! Nameless IR - An IR with all names replaced with `DefId`s.
//!
//! This is the result of name resolution. Names are still kept around so they can be retrieved
//! based on the `DefId` when we're generating errors.
//!
//! Note that this IR still contains field and method names as `Ident`s since those don't get
//! resolved until later when we know the types.

mod type_info;
pub mod def_data;
pub mod def_store2;
pub mod def_table;

use std::sync::Arc;

use crate::hir;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// ALL the functions in the entire program, including functions from all submodules, methods
    /// from impls, and even functions that were once nested within functions, etc. This works
    /// because after name resolution, there is no longer any reason to keep functions within their
    /// inner scopes. Anything needing scope information has been resolved at this point.
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: DefSpan,
    pub sig: FuncSig,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncSig {
    pub self_param: Option<DefSpan>,
    pub params: Vec<FuncParam>,
    pub return_type: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: DefSpan,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
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
    pub name: DefSpan,
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
    Def(DefSpan),
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lvalue: LValue,
    /// The expression for the value to assign to the left-hand side
    pub expr: Expr,
}

/// Expressions that can be on the left-hand side of assignment
#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    FieldAccess(FieldAccess),
    Path(DefSpan),
}

/// The short-circuiting `||` operator
#[derive(Debug, Clone, PartialEq)]
pub struct BoolOr {
    pub lhs: Expr,
    /// The span of the `||` operator
    pub op_span: Span,
    pub rhs: Expr,
}

/// The short-circuiting `&&` operator
#[derive(Debug, Clone, PartialEq)]
pub struct BoolAnd {
    pub lhs: Expr,
    /// The span of the `&&` operator
    pub op_span: Span,
    pub rhs: Expr,
}

/// A method call in the form `<expr> . <call-expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall {
    /// The expression of the left-hand side of the method call
    pub lhs: Expr,
    /// The method being called (not resolved during name resolution)
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
    /// The field being accessed (not resolved during name resolution)
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
    /// The struct being initialized
    pub name: DefSpan,
    /// Each struct field and its initialization expression
    ///
    /// The order of these fields may not match the order that the fields are declared in the struct.
    ///
    /// The fields are guaranteed to be unique.
    pub field_values: Vec<StructFieldValue>,
    /// The span of the entire struct literal
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldValue {
    /// The name of the field
    pub name: DefSpan,
    /// The expression being assigned to the field
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub type_hint: Option<def_store2::DefId>,
    /// The span for the entire integer literal, including its suffix
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Def(DefSpan),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSpan {
    pub id: def_store2::DefId,
    pub span: Span,
}

/// Only used for names that could not be resolved
pub type Ident = hir::Ident;
