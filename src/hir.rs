//! High-level IR - Completely Desugared AST

use std::fmt;

use crate::ast;

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
    pub path: IdentPath,
    pub selection: ImportSelection,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelection {
    /// A specific list of names being imported
    Names(Vec<Ident>),
    /// A wildcard import (all items)
    All,
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
    pub return_type: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: Ident,
    pub ty: Ty,
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
    MethodCall(Box<MethodCall>),
    FieldAccess(Box<FieldAccess>),
    Cond(Box<Cond>),
    Call(FuncCall),
    Return(Option<Box<Expr>>),
    StructLiteral(StructLiteral),
    BStrLiteral(Vec<u8>),
    IntegerLiteral(IntegerLiteral),
    RealLiteral(f64),
    ComplexLiteral(f64),
    BoolLiteral(bool),
    UnitLiteral,
    SelfLiteral,
    Path(IdentPath),
    /// Either a variable or function in the module scope
    Var(Ident),
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lhs: LValue,
    /// The expression for the value to assign to the left-hand side
    pub expr: Expr,
}

/// Expressions that can be on the left-hand side of assignment
#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    FieldAccess(FieldAccess),
    Var(Ident),
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
    pub func_name: IdentPath,
    pub args: Vec<Expr>,
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
    Named(IdentPath),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Unit,
    SelfType,
    Named(IdentPath),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentPath {
    /// The components of the path (non-empty only if root is false)
    pub components: Vec<Ident>,
    /// If true, this path is relative to the root of the current package
    ///
    /// i.e. the path started with the `package` keyword
    pub root: bool,
}

impl fmt::Display for IdentPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {components, root} = self;

        if *root {
            write!(f, "package")?;
            if components.is_empty() {
                return Ok(());
            }
            write!(f, "::")?;
        }

        write!(f, "{}", components[0])?;
        for comp in &components[1..] {
            write!(f, "::{}", comp)?;
        }

        Ok(())
    }
}

pub type Ident = ast::Ident;
