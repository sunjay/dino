//! Name resolution code. Takes the AST and extracts all the named items.

mod decl_map;
mod function;
mod type_info;

pub use decl_map::*;
pub use function::*;
pub use type_info::*;

use snafu::{Snafu, OptionExt};

use crate::ast;
use crate::primitives::Primitives;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("the name '{}' is defined multiple times", duplicate))]
    DuplicateDecl {
        /// The name that was declared multiple times
        duplicate: String,
    },
    #[snafu(display("field `{}` is already declared for type `{}`", duplicate, type_name))]
    DuplicateField {
        /// The type that had a duplicate field
        type_name: String,
        /// The name of the repeated field
        duplicate: String,
    },
    #[snafu(display("cannot find type '{}' in this scope", name))]
    UnresolvedType {
        name: String,
    },
}

#[derive(Debug)]
pub struct ProgramDecls<'a> {
    /// The top-level declarations in the program
    pub top_level_decls: DeclMap<'a>,
    pub prims: Primitives,
}

impl<'a> ProgramDecls<'a> {
    /// Extracts the declarations from the given program
    pub fn extract(prog: ast::Program<'a>) -> Result<Self, Error> {
        let ast::Program {top_level_module} = prog;
        let ast::Module {decls} = top_level_module;

        let mut top_level_decls = DeclMap::default();
        let prims = Primitives::new(&mut top_level_decls);

        // Do a first pass and insert all the types so they are available for functions/impls
        for decl in &decls {
            match decl {
                ast::Decl::Struct(struct_decl) => {
                    let ast::Struct {name, fields: parsed_fields} = struct_decl;

                    // Use a loop to explicitly check for duplicate fields
                    let mut fields = Fields::new();
                    for field in parsed_fields {
                        let ast::StructField {name: field_name, ty} = field;
                        if fields.insert(field_name, *ty).is_some() {
                            return Err(Error::DuplicateField {
                                type_name: name.to_string(),
                                duplicate: field_name.to_string(),
                            });
                        }
                    }

                    let type_info = TypeInfo::new(name, fields);
                    top_level_decls.insert_type(name, type_info)?;
                },

                // Ignore in this pass
                ast::Decl::Impl(_) |
                ast::Decl::Function(_) => {},
            }
        }

        // Insert everything else, now that the types are there
        for decl in decls {
            match decl {
                // Already handled in the first pass
                ast::Decl::Struct(_) => {},

                ast::Decl::Impl(impl_block) => {
                    let ast::Impl {self_ty, methods} = impl_block;
                    let ty = match self_ty {
                        ast::Ty::Unit => prims.unit(),
                        ast::Ty::SelfType => return Err(Error::UnresolvedType {
                            name: "Self".to_string(),
                        }),
                        ast::Ty::Named(ty_name) => top_level_decls.type_id(&ty_name)
                            .with_context(|| UnresolvedType {name: ty_name})?,
                    };

                    for func in methods {
                        top_level_decls.insert_method(ty, func.name, func)?;
                    }
                },

                ast::Decl::Function(func) => {
                    top_level_decls.insert_func(func)?
                },
            }
        }

        Ok(Self {
            top_level_decls,
            prims,
        })
    }
}
