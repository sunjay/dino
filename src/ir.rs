//! An intermediate representation of the program designed for easier code generation.
//!
//! By creating values of the types in this module, you guarantee that:
//! * All types are inferred and checked at this point
//! * Method resolution has been completed
//!     * Every call knows all its types and operators have been desugared
//! * All declaration names are unique within any given module

pub use crate::ast::Ident;

use crate::resolve::TyId;

#[derive(Debug)]
pub struct Program<'a> {
    pub top_level_module: Module<'a>,
}

#[derive(Debug)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Function(Function<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub sig: FuncSig<'a>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct FuncSig<'a> {
    pub return_type: TyId,
    pub params: Vec<FuncParam<'a>>,
}

#[derive(Debug)]
pub struct FuncParam<'a> {
    pub name: Ident<'a>,
    pub ty: TyId,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr<'a>>,
    /// The return type of the block. Must match TyId in `ret` if `ret` is not None
    ///
    /// Must always be stored because the return expression is optional.
    pub ret_ty: TyId,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Cond(Cond<'a>),
    WhileLoop(WhileLoop<'a>),
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct WhileLoop<'a> {
    /// The condition for which the loop is expected to continue
    pub cond: Expr<'a>,
    /// The body of the loop, executed until the condition is false
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type of the identifier
    pub ty: TyId,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Cond(Box<Cond<'a>>, TyId),
    Call(CallExpr<'a>, TyId),
    IntegerLiteral(i64, TyId),
    RealLiteral(f64, TyId),
    ComplexLiteral(f64, TyId),
    BoolLiteral(bool, TyId),
    UnitLiteral(TyId),
    Var(Ident<'a>, TyId),
}

impl<'a> Expr<'a> {
    pub fn ty_id(&self) -> TyId {
        use Expr::*;
        match *self {
            Cond(_, ty_id) => ty_id,
            Call(_, ty_id) => ty_id,
            IntegerLiteral(_, ty_id) => ty_id,
            RealLiteral(_, ty_id) => ty_id,
            ComplexLiteral(_, ty_id) => ty_id,
            BoolLiteral(_, ty_id) => ty_id,
            UnitLiteral(ty_id) => ty_id,
            Var(_, ty_id) => ty_id,
        }
    }
}

#[derive(Debug)]
pub struct Cond<'a> {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr<'a>, Block<'a>)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block<'a>>,
}

#[derive(Debug)]
pub struct CallExpr<'a> {
    /// The name of the function to call
    pub func_name: Ident<'a>,
    /// The argument expressions to pass to the function
    pub args: Vec<Expr<'a>>,
}
