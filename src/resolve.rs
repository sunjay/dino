//! Name resolution code. Takes the AST and extracts all the named items.

mod decl_map;

pub use decl_map::*;

use crate::ast;

macro_rules! primitives {
    (
        $(#[$attr:meta])*
        $vis:vis struct $struct_name:ident {
            $($prim:ident => $name:literal),* $(,)?
        }
    ) => {
        $(#[$attr])*
        $vis struct $struct_name {
            $(
                $prim: TyId
            ),*
        }

        impl $struct_name {
            /// Inserts all of the primitives into the declaration map
            fn new(decls: &mut DeclMap) -> Self {
                Self {
                    $(
                        $prim: decls.insert_ty($name)
                            .expect("bug: primitive type defined more than once"),
                    )*
                }
            }

            $(
                pub fn $prim(&self) -> TyId {
                    self.$prim
                }
            )*
        }
    };
}

primitives! {
    /// The core primitives of the compiler
    #[derive(Debug)]
    pub struct Primitives {
        // The unit type has no name
        unit => "",
        bool => "bool",
        int => "int",
        real => "real",
    }
}

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
                ast::Decl::Function(func) => top_level_decls.insert_func(func)?,
            }
        }

        Ok(Self {
            top_level_decls,
            prims,
        })
    }
}
