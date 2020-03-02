use crate::ir;

#[derive(Debug)]
pub struct FunctionInfo<'a> {
    /// The name of the function
    pub name: ir::Ident<'a>,
    /// The type signature of the function
    pub sig: ir::FuncSig<'a>,
    /// True if the function is meant to be linked in externally
    pub is_extern: bool,
}

impl<'a> FunctionInfo<'a> {
    /// Creates a new extern function with the given name and signature
    pub fn new_extern(name: ir::Ident<'a>, sig: ir::FuncSig<'a>) -> Self {
        Self {
            name,
            sig,
            is_extern: true,
        }
    }
}
