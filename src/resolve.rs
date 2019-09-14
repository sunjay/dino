//! Name resolution code. Takes the AST and extracts all the named items.

mod decl_map;

pub use decl_map::*;

use crate::ast;

#[derive(Debug)]
pub struct ProgramDecls<'a> {
    /// The top-level declarations in the program
    decls: DeclMap<'a>,
}

impl<'a> ProgramDecls<'a> {
    pub fn new(prog: ast::Program<'a>) -> Result<Self, DuplicateDecl> {
        let ast::Program {top_level_module} = prog;
        let ast::Module {decls} = top_level_module;

        Ok(Self {
            decls: DeclMap::new(decls)?,
        })
    }
}
