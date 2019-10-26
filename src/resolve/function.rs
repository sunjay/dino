use crate::{ast, ir};
use crate::primitives::Primitives;

use super::Error;

/// Represents a function with its signature fully resolved
#[derive(Debug)]
pub struct Function<'a> {
    /// The name of the function
    pub name: ir::Ident<'a>,
    /// The type signature of the function
    pub sig: ir::FuncSig<'a>,
    /// The body of the function. Not used if `is_extern` is true.
    pub body: ast::Block<'a>,
    /// True if the function is meant to be linked in externally
    pub is_extern: bool,
}

impl<'a> Function<'a> {
    pub fn new_extern(name: &'a str, sig: ir::FuncSig<'a>) -> Self {
        Self {
            name,
            sig,
            body: ast::Block::default(),
            is_extern: true,
        }
    }

    /// Resolves all of the types in the function signature and returns the resolved function
    pub fn resolve(
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<Self, Error> {
        let ast::Function {name, sig, body, is_extern} = func;
        let ast::FuncSig {return_type, params} = sig;

        let return_type = self.lookup_type(return_type)?;
        let params = params.iter().map(|param| {
            let ast::FuncParam {name, ty} = param;
            Ok(ir::FuncParam {name, ty: self.lookup_type(ty)?})
        }).collect::<Result<Vec<_>, _>>()?;

        let sig = ir::FuncSig {return_type, params};

        Self {}
    }
}
