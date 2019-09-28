//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

mod parser;

pub use parser::Error as ParseError;

#[derive(Debug, PartialEq)]
pub struct Program<'a> {
    pub top_level_module: Module<'a>,
}

impl<'a> Program<'a> {
    pub fn parse(input: &'a str) -> Result<Self, ParseError> {
        Ok(Program {
            top_level_module: parser::parse_module(input)?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Decl<'a> {
    Function(Function<'a>),
}

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    /// The name of the function
    pub name: Ident<'a>,
    /// The type signature of the function
    pub sig: FuncSig<'a>,
    /// The body of the function. Not used if `is_extern` is true.
    pub body: Block<'a>,
    /// True if the function is meant to be linked in externally
    pub is_extern: bool,
}

impl<'a> Function<'a> {
    pub fn new_extern(name: &'a str, sig: FuncSig<'a>) -> Self {
        Self {
            name,
            sig,
            body: Block::default(),
            is_extern: true,
        }
    }
}

/// The type signature of a function
#[derive(Debug, PartialEq)]
pub struct FuncSig<'a> {
    pub return_type: Ty<'a>,
    pub params: Vec<FuncParam<'a>>
}

#[derive(Debug, PartialEq)]
pub struct FuncParam<'a> {
    pub name: Ident<'a>,
    pub ty: Ty<'a>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

impl<'a> Block<'a> {
    pub fn is_empty(&self) -> bool {
        let Block {stmts} = self;
        stmts.is_empty()
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type of the variable (or None if the type is to be inferred)
    pub ty: Option<Ident<'a>>,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Call(CallExpr<'a>),
    IntegerLiteral(IntegerLiteral<'a>),
    RealLiteral(f64),
    ComplexLiteral(f64),
    BoolLiteral(bool),
    Var(Ident<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral<'a> {
    pub value: i64,
    /// You can append "int" or "real" to help disambiguate the literal
    /// e.g. 132int or 32real
    pub type_hint: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr<'a> {
    pub func_name: Ident<'a>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Ty<'a> {
    Unit,
    Named(Ident<'a>),
}

pub type Ident<'a> = &'a str;
