//! C IR - Types for representing the minimal subset of C used for code generation.
//!
//! All types here should directly map to concepts expressible in C. This is the last step in code
//! generation and no further processing should be required in order to convert these types to C.

use std::fmt;

use crate::{cwrite, cwriteln};
use crate::runtime::ALLOCATE;
use crate::fmt_ctx::DisplayCtx;
use crate::symbol_table::{SymbolTable, SymId, GenId};

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
pub struct CProgram {
    pub structs: Vec<CStruct>,
    pub functions: Vec<CFunction>,
    /// If provided, the program will end with an entry point (`int main`) whose body is the given
    /// statements
    pub entry_point: Option<Vec<CStmt>>,
}

impl DisplayCtx<CSymbols> for CProgram {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {structs, functions, entry_point} = self;

        cwriteln!(f, ctx, "// struct forward declarations")?;
        for struct_decl in structs {
            cwriteln!(f, ctx, "{}", struct_decl.forward_decl())?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// struct declarations")?;
        for struct_decl in structs {
            cwriteln!(f, ctx, "{}", struct_decl)?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// function forward declarations")?;
        for func in functions {
            cwriteln!(f, ctx, "{}", func.forward_decl())?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// function declarations")?;
        for func in functions {
            cwriteln!(f, ctx, "{}", func)?;
        }
        cwriteln!(f, ctx)?;

        if let Some(entry_point_body) = entry_point {
            cwriteln!(f, ctx, "int main() {{")?;
            for stmt in entry_point_body {
                cwriteln!(f, ctx, "{}", stmt)?;
            }
            cwriteln!(f, ctx, "return 0;")?;
            cwriteln!(f, ctx, "}}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CStruct {
    pub name: CIdent,
    /// If any of the fields have the same name as the struct, the
    /// `struct` C keyword will be added before them
    pub fields: Vec<CStructField>,
}

impl CStruct {
    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> DisplayCtx<CSymbols> for ForwardDecl<'a, CStruct> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let CStruct {name, fields: _} = self.value;
        cwrite!(f, ctx, "typedef struct {} {};", name, name)
    }
}

impl DisplayCtx<CSymbols> for CStruct {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, fields} = self;
        cwriteln!(f, ctx, "typedef struct {} {{", name)?;
        for field in fields {
            let CStructField {name: field_name, ptr_typ} = field;
            if ptr_typ == name {
                // Recursive type
                cwriteln!(f, ctx, "struct {}* {};", ptr_typ, field_name)?;
            } else {
                cwriteln!(f, ctx, "{}* {};", ptr_typ, field_name)?;
            }
        }
        cwrite!(f, ctx, "}} {};", name)
    }
}

/// All struct fields are pointers to a type
#[derive(Debug, Clone)]
pub struct CStructField {
    pub name: CIdent,
    /// The type of the pointer stored in this field
    pub ptr_typ: CIdent,
}

#[derive(Debug, Clone)]
pub struct CFunction {
    pub name: CIdent,
    /// The input parameters of the function (possibly empty)
    pub in_params: Vec<CInParam>,
    /// Functions always have an out parameter since in our language you always return *something*,
    /// even if that something is just `()`.
    pub out_param: COutParam,
    pub body: Vec<CStmt>,
}

impl CFunction {
    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> DisplayCtx<CSymbols> for ForwardDecl<'a, CFunction> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let CFunction {name, in_params, out_param, body: _} = self.value;
        cwrite!(f, ctx, "void {}({});", name, Commas {values: in_params, last: out_param})
    }
}

impl DisplayCtx<CSymbols> for CFunction {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, in_params, out_param, body} = self;
        cwriteln!(f, ctx, "void {}({}) {{", name, Commas {values: in_params, last: out_param})?;
        for stmt in body {
            cwriteln!(f, ctx, "{}", stmt)?;
        }
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct CInParam {
    pub name: CIdent,
    /// The type of the pointer this parameter represents
    pub ptr_typ: CIdent,
}

impl DisplayCtx<CSymbols> for CInParam {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ptr_typ} = self;
        cwrite!(f, ctx, "{}* {}", ptr_typ, name)
    }
}

#[derive(Debug, Clone)]
pub struct COutParam {
    pub name: CIdent,
    /// The type this out pointer represents
    pub ptr_typ: CIdent,
}

impl DisplayCtx<CSymbols> for COutParam {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ptr_typ} = self;
        cwrite!(f, ctx, "{}** {}", ptr_typ, name)
    }
}

#[derive(Debug, Clone)]
pub enum CStmt {
    /// An uninitialized variable declaration
    VarDecl(CVarDecl),
    /// Calls a function
    FuncCall(CFuncCall),
    /// Variable Assignment
    Assign(CAssign),
}

impl DisplayCtx<CSymbols> for CStmt {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CStmt::*;
        match self {
            VarDecl(decl) => cwrite!(f, ctx, "{};", decl),
            FuncCall(decl) => cwrite!(f, ctx, "{};", decl),
            Assign(decl) => cwrite!(f, ctx, "{};", decl),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CVarDecl {
    pub name: CIdent,
    pub typ: CVarType,
}

impl DisplayCtx<CSymbols> for CVarDecl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, typ} = self;
        cwrite!(f, ctx, "{} {}", typ, name)
    }
}

#[derive(Debug, Clone)]
pub enum CVarType {
    /// A pointer to the given type
    Ptr {typ: CIdent},
    /// A plain `bool` type
    Bool,
}

impl DisplayCtx<CSymbols> for CVarType {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CVarType::*;
        match self {
            Ptr {typ} => cwrite!(f, ctx, "{}*", typ),
            Bool => cwrite!(f, ctx, "bool"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CFuncCall {
    pub func_name: CIdent,
    /// The input arguments to the function. Each one is a plain variable whose type must be a
    /// pointer.
    pub in_args: Vec<CIdent>,
    /// Since all functions take an out parameter, we always have to provide it
    pub out_arg: COutArg,
}

impl DisplayCtx<CSymbols> for CFuncCall {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {func_name, in_args, out_arg} = self;
        cwrite!(f, ctx, "{}({})", func_name, Commas {values: in_args, last: out_arg})
    }
}

/// An output argument to a function. Must be a pointer variable.
///
/// This will cause the address of that variable to be passed to the function.
#[derive(Debug, Clone)]
pub struct COutArg {
    /// The name of the variable to write output to
    pub name: CIdent,
}

impl DisplayCtx<CSymbols> for COutArg {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name} = self;
        cwrite!(f, ctx, "&{}", name)
    }
}

#[derive(Debug, Clone)]
pub struct CAssign {
    /// The left-hand side of the assignment
    pub target: CAssignTarget,
    /// The right-hand side of the assignment
    pub value: CAssignValue,
}

impl DisplayCtx<CSymbols> for CAssign {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {target, value} = self;
        cwrite!(f, ctx, "{} = {}", target, value)
    }
}

#[derive(Debug, Clone)]
pub enum CAssignTarget {
    /// Dereferences and writes to an out pointer
    OutPtr {
        /// The name of the variable that is an out pointer
        name: CIdent,
    },
    /// Dereferences an out pointer and writes to one of its fields
    OutPtrField {
        /// The name of the variable that is an out pointer
        name: CIdent,
        /// The name of the field to write to
        field: CIdent,
    },
    /// Writes to one of the fields of an in-pointer
    InPtrField {
        /// The name of the variable that is an in-pointer
        name: CIdent,
        /// The name of the field to write to
        field: CIdent,
    },
    /// Writes to the variable with the given name
    Var {
        /// The name of the variable
        name: CIdent,
    },
}

impl DisplayCtx<CSymbols> for CAssignTarget {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CAssignTarget::*;
        match *self {
            OutPtr {name} => cwrite!(f, ctx, "*{}", name),
            OutPtrField {name, field} => cwrite!(f, ctx, "(*{})->{}", name, field),
            InPtrField {name, field} => cwrite!(f, ctx, "{}->{}", name, field),
            Var {name} => cwrite!(f, ctx, "{}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CAssignValue {
    /// Allocates memory for the size of the given type
    Alloc {
        typ: CIdent,
    },
    /// Accesses a field on an in-pointer
    FieldAccess {
        /// The value to access a field from
        value: CIdent,
        /// The name of the field to access
        field: CIdent,
    },
    /// Accesses a plain variable
    Var {
        /// The name of the variable
        name: CIdent,
    },
}

impl DisplayCtx<CSymbols> for CAssignValue {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CAssignValue::*;
        match *self {
            Alloc {typ} => cwrite!(f, ctx, "{}(sizeof({}))", ALLOCATE, typ),
            FieldAccess {value, field} => cwrite!(f, ctx, "{}->{}", value, field),
            Var {name} => cwrite!(f, ctx, "{}", name),
        }
    }
}

/// Symbol table for C identifiers
pub type CSymbols = SymbolTable<String, CIdent>;

/// The ID of an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIdent(usize);

impl DisplayCtx<CSymbols> for CIdent {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        write!(f, "{}", ctx.symbol(*self))
    }
}

impl SymId for CIdent {
    type Gen = CIdentGen;
}

#[derive(Debug, Default)]
pub struct CIdentGen {
    next: usize,
}

impl GenId<CIdent> for CIdentGen {
    fn next_id(&mut self) -> CIdent {
        let id = self.next;
        self.next += 1;
        CIdent(id)
    }
}

/// Represents a foward declaration of the given type
///
/// Allows us to specialize the implementation of `DisplayCtx` for forward declarations
struct ForwardDecl<'a, T> {
    value: &'a T,
}

/// Writes out a non-empty comma-separated list
struct Commas<'a, T: DisplayCtx<CSymbols>, L: DisplayCtx<CSymbols>> {
    /// The values to write out in a comma-separated list
    values: &'a [T],
    /// The final value, always present
    last: &'a L,
}

impl<'a, T: DisplayCtx<CSymbols>, L: DisplayCtx<CSymbols>> DisplayCtx<CSymbols> for Commas<'a, T, L> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let &Self {values, last} = self;

        for value in values {
            cwrite!(f, ctx, "{}, ", value)?;
        }
        // C does not support trailing commas
        cwrite!(f, ctx, "{}", last)?;

        Ok(())
    }
}
