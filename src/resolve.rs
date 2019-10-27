//! Name resolution code. Takes the AST and extracts all the named items.

mod decl_map;
mod func_info;
mod type_info;

pub use decl_map::*;
pub use func_info::*;
pub use type_info::*;

use std::collections::HashSet;

use snafu::{Snafu, OptionExt};

use crate::ast;
use crate::ir;
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
    #[snafu(display("identifier `{}` is bound more than once in this parameter list", duplicate))]
    DuplicateFuncParam {
        /// The name of the repeated parameter name
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
    pub fn extract(prog: &ast::Program<'a>) -> Result<Self, Error> {
        let mut top_level_decls = DeclMap::default();
        let prims = Primitives::new(&mut top_level_decls);
        let program_decls = Self {top_level_decls, prims};

        let ast::Program {top_level_module} = prog;
        let ast::Module {decls} = top_level_module;

        program_decls.reserve_types(&decls)?;
        program_decls.resolve_fields(&decls)?;
        program_decls.resolve_funcs_methods(&decls)?;

        Ok(program_decls)
    }

    /// Reserves type IDs for the declared types
    fn reserve_types(&mut self, decls: &[ast::Decl<'a>]) -> Result<(), Error> {
        // Inserts all the types so they are available for everything resolved after
        for decl in decls {
            match decl {
                ast::Decl::Struct(struct_decl) => {
                    let ast::Struct {name, fields: _} = struct_decl;

                    self.top_level_decls.reserve_type(name)?;
                },

                // Ignore in this pass
                ast::Decl::Impl(_) |
                ast::Decl::Function(_) => {},
            }
        }

        Ok(())
    }

    /// Attempts to resolve all the field types in each struct
    ///
    /// Assumes that all types (user-defined or otherwise) have been given a type ID at this point.
    fn resolve_fields(&mut self, decls: &[ast::Decl<'a>]) -> Result<(), Error> {
        for decl in decls {
            match decl {
                ast::Decl::Struct(struct_decl) => {
                    let ast::Struct {name, fields: parsed_fields} = struct_decl;

                    let self_ty = self.top_level_decls.type_id(name)
                        .expect("bug: all types should have been inserted by now");

                    // Use a loop to explicitly check for duplicate fields
                    let mut fields = ir::FieldTys::new();
                    for field in parsed_fields {
                        let ast::StructField {name: field_name, ty} = field;
                        let field_ty = self.resolve_ty(ty, Some(self_ty))?;

                        if fields.insert(field_name, field_ty).is_some() {
                            return Err(Error::DuplicateField {
                                type_name: name.to_string(),
                                duplicate: field_name.to_string(),
                            });
                        }
                    }

                    let type_info = TypeInfo::new(name, fields);
                    self.top_level_decls.insert_type(name, type_info)?;
                },

                // Ignore in this pass
                ast::Decl::Impl(_) |
                ast::Decl::Function(_) => {},
            }
        }

        Ok(())
    }

    /// Stores all of the function/method signatures for all declared types
    ///
    /// Assumes that all types (user-defined or otherwise) have been inserted at this point.
    fn resolve_funcs_methods(&mut self, decls: &[ast::Decl<'a>]) -> Result<(), Error> {
        // Insert everything else, now that the types are there
        for decl in decls {
            match decl {
                // Already handled in the first pass
                ast::Decl::Struct(_) => {},

                ast::Decl::Impl(impl_block) => self.resolve_impl_block(impl_block)?,

                ast::Decl::Function(func) => {
                    let func_info = self.resolve_function(func, None)?;
                    self.top_level_decls.insert_func(func_info)?;
                },
            }
        }

        Ok(())
    }

    fn resolve_impl_block(&mut self, impl_block: &ast::Impl<'a>) -> Result<(), Error> {
        let ast::Impl {self_ty, methods} = impl_block;
        let self_ty = self.resolve_ty(self_ty, None)?;

        for func in methods {
            let func_info = self.resolve_function(func, Some(self_ty))?;
            self.top_level_decls.insert_method(self_ty, func.name, func_info)?;
        }

        Ok(())
    }

    fn resolve_function(&self, func: &ast::Function<'a>, self_ty: Option<TyId>) -> Result<FunctionInfo<'a>, Error> {
        let &ast::Function {name, ref sig, body: _, is_extern} = func;

        Ok(FunctionInfo {
            name,
            sig: self.resolve_sig(sig, self_ty)?,
            is_extern,
        })
    }

    fn resolve_sig(&self, sig: &ast::FuncSig<'a>, self_ty: Option<TyId>) -> Result<ir::FuncSig<'a>, Error> {
        let ast::FuncSig {return_type, params} = sig;

        let return_type = self.resolve_ty(return_type, self_ty)?;

        // Ensure that parameter names are unique
        let param_names = HashSet::new();
        let params = params.iter().map(|param| {
            let ast::FuncParam {name, ty} = param;

            if !param_names.insert(name) {
                return Err(Error::DuplicateFuncParam {duplicate: name.to_string()});
            }

            let ty = self.resolve_ty(ty, self_ty)?;
            Ok(ir::FuncParam {name, ty})
        }).collect::<Result<Vec<_>, _>>()?;

        Ok(ir::FuncSig {return_type, params})
    }

    fn resolve_ty(&self, ty: &ast::Ty<'a>, self_ty: Option<TyId>) -> Result<TyId, Error> {
        match ty {
            ast::Ty::Unit => Ok(self.prims.unit()),

            ast::Ty::SelfType => match self_ty {
                Some(ty_id) => Ok(ty_id),
                    None => return Err(Error::UnresolvedType {
                    name: "Self".to_string(),
                }),
            },

            &ast::Ty::Named(ty_name) => self.top_level_decls.type_id(&ty_name)
                .with_context(|| UnresolvedType {name: ty_name}),
        }
    }
}
