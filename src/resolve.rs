//! Name resolution code. Takes the AST and extracts all the named items.

mod decl_map;
mod type_info;

pub use decl_map::*;
pub use type_info::*;

use crate::ast;
use crate::primitives::Primitives;

#[derive(Debug)]
pub struct ProgramDecls<'a> {
    /// The top-level declarations in the program
    pub top_level_decls: DeclMap<'a>,
    pub prims: Primitives,
}

impl<'a> ProgramDecls<'a> {
    pub fn new(prog: ast::Program<'a>) -> Result<Self, DuplicateDecl> {
        let ast::Program {top_level_module} = prog;
        let ast::Module {decls} = top_level_module;

        let mut top_level_decls = DeclMap::default();
        let prims = Primitives::new(&mut top_level_decls);

        for decl in decls {
            match decl {
                ast::Decl::Struct(struct_decl) => unimplemented!(),
                ast::Decl::Impl(impl_block) => unimplemented!(),
                ast::Decl::Function(func) => top_level_decls.insert_func(func)?,
            }
        }

        Ok(Self {
            top_level_decls,
            prims,
        })
    }
}
