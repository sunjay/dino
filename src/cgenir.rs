//! C Generation IR - Similar to C, but a minimal subset that still encodes details of things like
//! our calling convention, program structure, etc. The goal of this IR is to limit flexibility and
//! help us generate the correct code.
//!
//! Although most types here map directly to concepts expressible directly in C, we may need to do
//! some minimal processing in order to produce valid C code. For example, recursive struct types
//! do a check on the struct type name. This isn't done in C IR.

use std::iter::once;

use crate::gc_lib::GC_LIB_HEADER_FILENAME;
use crate::runtime::{ALLOCATE, RUNTIME_HEADER_FILENAME};
use crate::dino_std::DINO_STD_HEADER_FILENAME;
use crate::cir;

pub trait GenerateC {
    /// The type from `cir` that this generates
    type Output;

    /// Generates C IR for the program, adding to the symbol table as necessary
    fn to_c(&self) -> Self::Output;
}

impl<'a, T: GenerateC> GenerateC for &'a T {
    type Output = <T as GenerateC>::Output;

    fn to_c(&self) -> Self::Output {
        (*self).to_c()
    }
}

impl<T: GenerateC> GenerateC for Vec<T> {
    type Output = Vec<<T as GenerateC>::Output>;

    fn to_c(&self) -> Self::Output {
        self.iter()
            .map(|stmt| stmt.to_c())
            .collect()
    }
}

/// Represents a complete C program with an optional entry point (for executables)
///
/// The program is written as follows:
/// 1. Forward declarations for all structs
/// 2. All struct declarations
/// 3. Forward declarations for all functions
/// 4. All function declarations
///
/// Printing forward declarations first enables structs and functions to refer to each other
/// without forcing us to write them out in dependency order. This even works if there are cyclic
/// dependencies between functions and types.
#[derive(Debug, Clone)]
pub struct Program {
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
    /// If provided, the program will end with an entry point (`int main`) whose body is the given
    /// statements
    pub entry_point: Option<Vec<Stmt>>,
}

impl GenerateC for Program {
    type Output = cir::Program;

    fn to_c(&self) -> Self::Output {
        let Self {structs, functions, entry_point} = self;

        let mut decls = Vec::new();

        decls.push(cir::Decl::Include(GC_LIB_HEADER_FILENAME.into()));
        decls.push(cir::Decl::Include(RUNTIME_HEADER_FILENAME.into()));
        decls.push(cir::Decl::Include(DINO_STD_HEADER_FILENAME.into()));
        decls.push(cir::Decl::BlankLine);

        decls.push(cir::Decl::Comment("struct forward declarations".into()));
        decls.reserve(structs.len());
        for struct_decl in structs {
            decls.push(cir::Decl::TypedefForwardDecl(struct_decl.forward_decl().to_c()));
        }
        decls.push(cir::Decl::BlankLine);

        decls.push(cir::Decl::Comment("struct declarations".into()));
        for struct_decl in structs {
            decls.push(cir::Decl::Typedef(struct_decl.to_c()));
        }
        decls.push(cir::Decl::BlankLine);

        decls.push(cir::Decl::Comment("function forward declarations".into()));
        for func in functions {
            decls.push(cir::Decl::FunctionForwardDecl(func.forward_decl().to_c()));
        }
        decls.push(cir::Decl::BlankLine);

        decls.push(cir::Decl::Comment("function declarations".into()));
        for func in functions {
            decls.push(cir::Decl::Function(func.to_c()));
        }

        if let Some(entry_point_body) = entry_point {
            decls.push(cir::Decl::BlankLine);
            decls.push(cir::Decl::Function(cir::Function {
                sig: cir::FuncSig {
                    name: "main".into(),
                    ret_type: cir::Type::Int32,
                    params: Vec::new(),
                },

                body: entry_point_body.iter()
                    .map(|stmt| stmt.to_c())
                    .chain(once(cir::Stmt::Return(Some(cir::Expr::Int32(0)))))
                    .collect(),
            }));
        }

        cir::Program {
            decls,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    /// If any of the fields have the same name as the struct, the
    /// `struct` C keyword will be added before them
    pub fields: Vec<StructField>,
}

impl Struct {
    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> GenerateC for ForwardDecl<'a, Struct> {
    type Output = cir::TypedefForwardDecl;

    fn to_c(&self) -> Self::Output {
        let &Struct {name, fields: _} = self.value;

        cir::TypedefForwardDecl {
            struct_name: name,
            typedef_name: name,
        }
    }
}

impl GenerateC for Struct {
    type Output = cir::Typedef;

    fn to_c(&self) -> Self::Output {
        let &Self {name, ref fields} = self;

        cir::Typedef {
            struct_type: cir::Struct {
                name,
                fields: fields.iter().map(|field| {
                    let &StructField {name: field_name, ptr_typ} = field;
                    if ptr_typ == name {
                        // Recursive type
                        cir::StructField {
                            name: field_name,
                            ty: cir::Type::StructPtr(ptr_typ),
                        }
                    } else {
                        cir::StructField {
                            name: field_name,
                            ty: cir::Type::Ptr(ptr_typ),
                        }
                    }
                }).collect(),
            },
            typedef_name: name,
        }
    }
}

/// All struct fields are pointers to a type
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    /// The type of the pointer stored in this field
    pub ptr_typ: Ident,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    /// The input parameters of the function (possibly empty)
    pub in_params: Vec<InParam>,
    /// Functions always have an out parameter since in our language you always return *something*,
    /// even if that something is just `()`.
    pub out_param: OutParam,
    pub body: Vec<Stmt>,
}

impl Function {
    fn to_c_sig(&self) -> cir::FuncSig {
        let Self {name, in_params, out_param, body: _} = self;

        cir::FuncSig {
            name: name.into(),
            ret_type: cir::Type::Void,
            params: in_params.iter()
                .map(|param| param.to_c())
                .chain(once(out_param.to_c()))
                .collect(),
        }
    }

    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> GenerateC for ForwardDecl<'a, Function> {
    type Output = cir::FunctionForwardDecl;

    fn to_c(&self) -> Self::Output {
        cir::FunctionForwardDecl {
            sig: self.value.to_c_sig(),
        }
    }
}

impl GenerateC for Function {
    type Output = cir::Function;

    fn to_c(&self) -> Self::Output {
        let Self {name: _, in_params: _, out_param: _, body} = self;

        cir::Function {
            sig: self.to_c_sig(),
            body: body.to_c(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InParam {
    pub name: Ident,
    /// The type of the pointer this parameter represents
    pub ptr_typ: Ident,
}

impl GenerateC for InParam {
    type Output = cir::FuncParam;

    fn to_c(&self) -> Self::Output {
        let &Self {name, ptr_typ} = self;

        cir::FuncParam {
            name,
            ty: cir::Type::Ptr(ptr_typ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OutParam {
    pub name: Ident,
    /// The type this out pointer represents
    pub ptr_typ: Ident,
}

impl GenerateC for OutParam {
    type Output = cir::FuncParam;

    fn to_c(&self) -> Self::Output {
        let &Self {name, ptr_typ} = self;

        cir::FuncParam {
            name,
            ty: cir::Type::OutPtr(ptr_typ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// An uninitialized variable declaration
    VarDecl(VarDecl),
    /// Calls a function
    FuncCall(FuncCall),
    /// Calls a literal constructor
    ///
    /// This variant exists because while it works in general to always call functions with only
    /// variables as args, we still need other literals for this specific case only.
    LitFuncCall(LitFuncCall),
    /// Variable Assignment
    Assign(Assign),
    /// A conditional statement
    If(Cond),
    /// An infinite loop
    Loop(InfiniteLoop),
    /// Stops a loop
    Break,
    /// Returns early from a function
    Return,
}

impl GenerateC for Stmt {
    type Output = cir::Stmt;

    fn to_c(&self) -> Self::Output {
        use Stmt::*;
        match self {
            VarDecl(decl) => cir::Stmt::VarDecl(decl.to_c()),
            FuncCall(call) => cir::Stmt::FuncCall(call.to_c()),
            LitFuncCall(call) => cir::Stmt::FuncCall(call.to_c()),
            Assign(assign) => cir::Stmt::Assign(assign.to_c()),
            If(cond) => cir::Stmt::If(cond.to_c()),
            Loop(cloop) => cir::Stmt::WhileLoop(cloop.to_c()),
            Break => cir::Stmt::Break,
            Return => cir::Stmt::Return(None),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Ident,
    pub ty: VarType,
}

impl GenerateC for VarDecl {
    type Output = cir::VarDecl;

    fn to_c(&self) -> Self::Output {
        let &Self {name, ty} = self;

        cir::VarDecl {
            name,
            ty: ty.to_c(),
            body: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VarType {
    /// A pointer to the given type
    Ptr {ty: Ident},
    /// A plain `bool` type
    Bool,
}

impl GenerateC for VarType {
    type Output = cir::Type;

    fn to_c(&self) -> Self::Output {
        use VarType::*;
        match self {
            &Ptr {ty} => cir::Type::Ptr(ty),
            Bool => cir::Type::Bool,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    /// The name of the function to call
    pub name: Ident,
    /// The input arguments to the function. Each one is a plain variable whose type must be a
    /// pointer.
    pub in_args: Vec<Ident>,
    /// Since all functions take an out parameter, we always have to provide it
    pub out_arg: OutArg,
}

impl GenerateC for FuncCall {
    type Output = cir::FuncCall;

    fn to_c(&self) -> Self::Output {
        let Self {name, in_args, out_arg} = self;

        cir::FuncCall {
            name: name.into(),
            args: in_args.iter()
                .copied()
                .map(cir::Expr::Var)
                .chain(once(out_arg.to_c()))
                .collect(),
        }
    }
}

/// An output argument to a function. Must be a pointer variable.
///
/// This will cause the address of that variable to be passed to the function.
#[derive(Debug, Clone)]
pub struct OutArg {
    /// The name of the variable to write output to
    pub name: Ident,
}

impl GenerateC for OutArg {
    type Output = cir::Expr;

    fn to_c(&self) -> Self::Output {
        let &Self {name} = self;

        cir::Expr::AddrOf(name)
    }
}

#[derive(Debug, Clone)]
pub struct LitFuncCall {
    /// The name of the literal constructor
    pub lit_func_name: Ident,
    /// The input arguments to the literal constructor
    pub in_args: LitFuncArgs,
    /// The output argument for the literal constructor
    pub out_arg: OutArg,
}

impl GenerateC for LitFuncCall {
    type Output = cir::FuncCall;

    fn to_c(&self) -> Self::Output {
        let Self {lit_func_name, in_args, out_arg} = self;

        cir::FuncCall {
            name: lit_func_name.into(),
            args: in_args.to_c()
                .into_iter()
                .chain(once(out_arg.to_c()))
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LitFuncArgs {
    /// A byte-string literal and a second argument: its length
    StrLen(Vec<u8>),
    Int64(i64),
    Double(i64),
    Bool(bool),
}

impl GenerateC for LitFuncArgs {
    type Output = Vec<cir::Expr>;

    fn to_c(&self) -> Self::Output {
        use LitFuncArgs::*;
        match self {
            StrLen(lit) => vec![
                cir::Expr::BStr(cir::BStrLiteral(lit.clone())),
                cir::Expr::UInt64(lit.len() as u64),
            ],
            &Int64(lit) => vec![cir::Expr::Int64(lit)],
            &Double(lit) => vec![cir::Expr::Double(lit)],
            &Bool(lit) => vec![cir::Expr::Bool(lit)],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    /// The left-hand side of the assignment
    pub target: AssignTarget,
    /// The right-hand side of the assignment
    pub value: AssignValue,
}

impl GenerateC for Assign {
    type Output = cir::Assign;

    fn to_c(&self) -> Self::Output {
        let Self {target, value} = self;

        cir::Assign {
            target: target.to_c(),
            value: value.to_c(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignTarget {
    /// Writes to the variable with the given name
    Var {
        /// The name of the variable
        name: Ident,
    },
    /// Writes to one of the fields of an in-pointer
    InPtrField {
        /// The name of the variable that is an in-pointer
        name: Ident,
        /// The name of the field to write to
        field: Ident,
    },
    /// Dereferences and writes to an out pointer
    OutPtr {
        /// The name of the variable that is an out pointer
        name: Ident,
    },
    /// Dereferences an out pointer and writes to one of its fields
    OutPtrField {
        /// The name of the variable that is an out pointer
        name: Ident,
        /// The name of the field to write to
        field: Ident,
    },
}

impl GenerateC for AssignTarget {
    type Output = cir::LValue;

    fn to_c(&self) -> Self::Output {
        use AssignTarget::*;
        match *self {
            Var {name} => cir::LValue::Var(name),
            InPtrField {name, field} => cir::LValue::Field {name, field},
            OutPtr {name} => cir::LValue::Deref {name},
            OutPtrField {name, field} => cir::LValue::DerefField {name, field},
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignValue {
    /// Allocates memory for the size of the given type
    Alloc {
        ty: Ident,
    },
    /// Accesses a field on an in-pointer
    FieldAccess {
        /// The variable name to access a field from
        name: Ident,
        /// The name of the field to access
        field: Ident,
    },
    /// Accesses a plain variable
    Var {
        /// The name of the variable
        name: Ident,
    },
}

impl GenerateC for AssignValue {
    type Output = cir::Expr;

    fn to_c(&self) -> Self::Output {
        use AssignValue::*;
        match *self {
            Alloc {ty} => cir::Expr::FuncCall(cir::FuncCall {
                name: ALLOCATE.into(),
                args: vec![cir::Expr::Sizeof(cir::Type::Named(ty))],
            }),
            FieldAccess {name, field} => cir::Expr::FieldAccess(cir::FieldAccess {name, field}),
            Var {name} => cir::Expr::Var(name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cond {
    /// The condition variable (must be type `bool`)
    cond: Ident,
    /// The `if` body, executed if `cond` is true
    if_body: Vec<Stmt>,
    /// The `else` body, executed if `cond` is false
    else_body: Vec<Stmt>,
}

impl GenerateC for Cond {
    type Output = cir::Cond;

    fn to_c(&self) -> Self::Output {
        let Self {cond, if_body, else_body} = self;

        cir::Cond {
            cond: cir::Expr::Var(*cond),
            if_body: if_body.to_c(),
            else_body: else_body.to_c(),
        }
    }
}

/// Represents a C loop that runs infinitely (e.g. `while (true)`)
///
/// This loop will only terminate due to break or return statement.
#[derive(Debug, Clone)]
pub struct InfiniteLoop {
    body: Vec<Stmt>,
}

impl GenerateC for InfiniteLoop {
    type Output = cir::WhileLoop;

    fn to_c(&self) -> Self::Output {
        let Self {body} = self;

        cir::WhileLoop::infinite_loop(body.to_c())
    }
}

pub type Ident = cir::Ident;

/// Represents a foward declaration of the given type
///
/// Allows us to specialize the implementation of `GenerateC` for forward declarations
struct ForwardDecl<'a, T> {
    value: &'a T,
}
