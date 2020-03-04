//! Nameless IR - An IR with all names replaced with `DefId`s.
//!
//! This is the result of name resolution. Names are still kept around so they can be retrieved
//! based on the `DefId` when we're generating errors.
//!
//! Note that this IR still contains field and method names as `DefId`s since those don't get
//! resolved until later when we know the types.

mod type_info;
mod def_id;

pub use type_info::*;
pub use def_id::*;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Package {
    pub root: Module,
}

#[derive(Debug)]
pub struct Decls {
    /// Mapping of struct name (`DefId`) to the struct data
    ///
    /// Using a `HashMap` so we have fast lookup when adding methods from impl blocks
    pub structs: HashMap<DefId, Struct>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Module {
    pub decls: Decls,
}

#[derive(Debug)]
pub struct Struct {
    pub name: DefId,
    pub fields: Vec<NamedField>,
    pub methods: Vec<Function>,
}

impl Struct {
    pub fn new(name: DefId) -> Self {
        Self {
            name,
            fields: Vec::new(),
            methods: Vec::new(),
        }
    }
}

/// A named-field of a type
#[derive(Debug)]
pub struct NamedField {
    pub name: DefId,
    pub ty: DefId,
}

#[derive(Debug)]
pub struct Function {
    pub name: DefId,
    pub sig: FuncSig,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub params: Vec<FuncParam>,
    pub return_type: DefId,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: DefId,
    pub ty: DefId,
}

#[derive(Debug)]
pub struct Block {
    pub decls: Decls,
    pub stmts: Vec<Stmt>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr>,
}

#[derive(Debug)]
pub enum Stmt {
    Cond(Cond),
    WhileLoop(WhileLoop),
    VarDecl(VarDecl),
    Expr(Expr),
}

#[derive(Debug)]
pub struct WhileLoop {
    /// The condition for which the loop is expected to continue
    pub cond: Expr,
    /// The body of the loop, executed until the condition is false
    pub body: Block,
}

#[derive(Debug)]
pub struct VarDecl {
    /// The identifier to assign a value to
    pub name: DefId,
    /// The type of the variable (or None if the type is to be inferred)
    pub ty: Option<DefId>,
    /// The expression for the value to assign to the variable
    pub expr: Expr,
}

#[derive(Debug)]
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
    Var(DefId),
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug)]
pub struct Assign {
    /// The left-hand expression to assign a value to
    pub lhs: LValue,
    /// The expression for the value to assign to the left-hand side
    pub expr: Expr,
}

/// Expressions that can be on the left-hand side of assignment
#[derive(Debug)]
pub enum LValue {
    FieldAccess(FieldAccess),
    Var(DefId),
}

/// A method call in the form `<expr> . <call-expr>`
#[derive(Debug)]
pub struct MethodCall {
    /// The expression of the left-hand side of the method call
    pub lhs: Expr,
    /// The method being called (not resolved during name resolution)
    pub method_name: String,
    /// The arguments to the method call
    pub args: Vec<Expr>,
}

/// A field access in the form `<expr> . <ident>`
#[derive(Debug)]
pub struct FieldAccess {
    /// The expression of the left-hand side of the field access
    pub lhs: Expr,
    /// The field being accessed (not resolved during name resolution)
    pub field: String,
}

#[derive(Debug)]
pub struct Cond {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr, Block)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block>,
}

#[derive(Debug)]
pub struct FuncCall {
    pub func_name: DefId,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct StructLiteral {
    pub name: DefId,
    /// Mapping of struct field name to expression
    ///
    /// The order that the fields are provided does not matter here
    pub field_values: HashMap<DefId, Expr>,
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub type_hint: Option<DefId>,
}
