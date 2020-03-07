//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

use std::fmt;

/// Represents a single module within the current package
#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl<'a> {
    Import(ImportPath<'a>),
    Struct(Struct<'a>),
    Impl(Impl<'a>),
    Function(Function<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportPath<'a> {
    pub path: IdentPath<'a>,
    /// If `None`, only the path itself is imported
    pub selection: Option<ImportSelection<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSelection<'a> {
    /// A specific list of names being imported
    Names(Vec<Ident<'a>>),
    /// A wildcard import (all items)
    All,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct<'a> {
    /// The name of the struct
    pub name: Ident<'a>,
    /// The fields of the struct
    pub fields: Vec<StructField<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField<'a> {
    pub name: Ident<'a>,
    pub ty: Ty<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Impl<'a> {
    /// The Self type of this impl block
    pub self_ty: Ty<'a>,
    /// The method decls of this impl block
    pub methods: Vec<Function<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub sig: FuncSig<'a>,
    pub body: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncSig<'a> {
    pub params: Vec<FuncParam<'a>>,
    pub return_type: Ty<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam<'a> {
    pub name: Ident<'a>,
    pub ty: Ty<'a>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Block<'a> {
    pub decls: Vec<Decl<'a>>,
    pub stmts: Vec<Stmt<'a>>,
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr<'a>>,
}

impl<'a> Block<'a> {
    pub fn is_empty(&self) -> bool {
        let Block {decls, stmts, ret} = self;
        decls.is_empty() && stmts.is_empty() && ret.is_none()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'a> {
    Cond(Cond<'a>),
    WhileLoop(WhileLoop<'a>),
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop<'a> {
    /// The condition for which the loop is expected to continue
    pub cond: Expr<'a>,
    /// The body of the loop, executed until the condition is false
    pub body: Block<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub name: Ident<'a>,
    /// The type of the variable (or None if the type is to be inferred)
    pub ty: Option<Ty<'a>>,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Assign(Box<Assign<'a>>),
    MethodCall(Box<MethodCall<'a>>),
    FieldAccess(Box<FieldAccess<'a>>),
    Cond(Box<Cond<'a>>),
    Call(FuncCall<'a>),
    Return(Option<Box<Expr<'a>>>),
    StructLiteral(StructLiteral<'a>),
    BStrLiteral(Vec<u8>),
    IntegerLiteral(IntegerLiteral<'a>),
    RealLiteral(f64),
    ComplexLiteral(f64),
    BoolLiteral(bool),
    UnitLiteral,
    SelfLiteral,
    Path(IdentPath<'a>),
    /// Either a variable or function in the module scope
    Var(Ident<'a>),
}

/// An assignment expression in the form `<lvalue> = <value>`
#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'a> {
    /// The left-hand expression to assign a value to
    pub lhs: LValue<'a>,
    /// The expression for the value to assign to the left-hand side
    pub expr: Expr<'a>,
}

/// Expressions that can be on the left-hand side of assignment
#[derive(Debug, Clone, PartialEq)]
pub enum LValue<'a> {
    FieldAccess(FieldAccess<'a>),
    Var(Ident<'a>),
}

/// A method call in the form `<expr> . <call-expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct MethodCall<'a> {
    /// The expression of the left-hand side of the method call
    pub lhs: Expr<'a>,
    /// The method being called
    pub method_name: Ident<'a>,
    /// The arguments to the method call
    pub args: Vec<Expr<'a>>,
}

/// A field access in the form `<expr> . <ident>`
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess<'a> {
    /// The expression of the left-hand side of the field access
    pub lhs: Expr<'a>,
    /// The field being accessed
    pub field: Ident<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond<'a> {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr<'a>, Block<'a>)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall<'a> {
    pub func_name: IdentPath<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral<'a> {
    pub name: NamedTy<'a>,
    pub field_values: Vec<StructFieldValue<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldValue<'a> {
    /// The name of the field
    pub name: Ident<'a>,
    /// The expression being assigned to the field
    pub value: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral<'a> {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub type_hint: Option<&'a str>,
}

/// A type explicitly named with an identifier or path (as opposited to (), [T], etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum NamedTy<'a> {
    SelfType,
    Named(IdentPath<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty<'a> {
    Unit,
    SelfType,
    Named(IdentPath<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentPath<'a> {
    /// The components of the path (guaranteed to be non-empty)
    pub components: Vec<PathComponent<'a>>,
}

impl<'a> fmt::Display for IdentPath<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {components} = self;

        write!(f, "{}", components[0])?;
        for comp in &components[1..] {
            write!(f, "::{}", comp)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PathComponent<'a> {
    Ident(Ident<'a>),
    /// The `package` keyword
    Package,
    /// The `Self` keyword
    SelfType,
    /// The `self` keyword
    SelfValue,
    /// The `super` keyword
    Super,
}

impl<'a> fmt::Display for PathComponent<'a> {
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

pub type Ident<'a> = &'a str;
