//! Type inference and checking.

mod subst;
mod scope;
mod constraints;
mod solve;
mod tyir;

use std::sync::RwLock;
use std::collections::HashMap;

use snafu::Snafu;
use rayon::prelude::*;

use crate::{ast, ir};
use crate::resolve2::{ModuleDecls, ProgramDecls, DeclMap, TyId};
use crate::primitives::Primitives;

use constraints::ConstraintSet;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("cannot find value '{}' in this scope", name))]
    UnresolvedName {
        name: String,
    },
    #[snafu(display("cannot find type '{}' in this scope", name))]
    UnresolvedType {
        name: String,
    },
    #[snafu(display("cannot find function '{}' in this scope", name))]
    UnresolvedFunction {
        name: String,
    },
    #[snafu(display("no field named '{}' for type 'TODO'", field_name))]
    UnresolvedField {
        field_name: String,
        ty: TyId,
    },
    #[snafu(display("no method named '{}' in the current scope for type 'TODO'", method_name))]
    UnresolvedMethod {
        method_name: String,
        ty: TyId,
    },
    #[snafu(display("function '{}' takes {} parameter(s) but {} parameter(s) were supplied", func_name, expected, actual))]
    ArityMismatch {
        func_name: String,
        expected: usize,
        actual: usize,
    },
    #[snafu(display("cannot infer type, type annotations needed"))]
    AmbiguousType {
        //TODO: Add span info
    },
    #[snafu(display("type must be known at this point, type annotations needed"))]
    AmbiguousMethodCall {
        //TODO: Add span info
    },
    #[snafu(display("type must be known at this point, type annotations needed"))]
    AmbiguousFieldAccess {
        //TODO: Add span info
    },
    #[snafu(display("associated functions cannot be called as methods"))]
    UnexpectedAssociatedFunction {
        //TODO: Add span info
    },
    #[snafu(display("mismatched types"))]
    MismatchedTypes {
        //TODO: Add span info
        expected: TyId,
        actual: TyId,
    },
    #[snafu(display("invalid type for integer literal"))]
    InvalidIntLitType {
        actual: TyId,
    },
    #[snafu(display("invalid type for real number literal"))]
    InvalidRealLitType {
        actual: TyId,
    },
    #[snafu(display("field `{}` specified more than once", duplicate))]
    DuplicateField {
        /// The name of the repeated field
        duplicate: String,
    },
}

pub fn infer_and_check<'a>(
    module_decls: ModuleDecls<'a>,
    decls: &'a ProgramDecls<'a>,
) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    let mod_tycheck = ModuleTycheck {
        decls: top_level_decls,
        prims,
    };
    let top_level_module = mod_tycheck.infer_and_check_module(module_decls)?;

    Ok(ir::Program {top_level_module})
}

#[derive(Debug)]
struct ModuleTycheck<'a> {
    decls: &'a DeclMap<'a>,
    prims: &'a Primitives,
}

impl<'a> ModuleTycheck<'a> {
    fn infer_and_check_module(
        &self,
        module_decls: ModuleDecls<'a>,
    ) -> Result<ir::Module<'a>, Error> {
        let ModuleDecls {types, methods, functions} = module_decls;

        // Able to use concurrency here because types can be checked in any order

        // Creates a map of type name to its ir::Struct where the struct can be accessed concurrently
        let types = types.into_par_iter()
            .map(|(ty_id, struct_decl)| (ty_id, RwLock::new(struct_decl)))
            .collect::<HashMap<_, _>>();

        methods.into_par_iter().map(|(self_ty, methods)| {
            let types = &types;
            methods.into_par_iter().map(move |(sig, method)| {
                let method = self.infer_and_check_method(self_ty, sig, method)?;
                //TODO: Is method.name right here??
                let mut type_data = types[&self_ty].write().expect("bug: lock was poisoned");
                type_data.methods.insert(method.name, method);
                Ok(())
            })
        }).flatten().collect::<Result<(), _>>()?;

        let functions = functions.into_par_iter()
            // No need to check external functions
            .filter(|(_, func)| !func.is_extern)
            .map(|(sig, func)| self.infer_and_check_func(sig, func))
            .collect::<Result<Vec<_>, _>>()?;

        let types = types.into_par_iter()
            .map(|(_, struct_decl)| struct_decl.into_inner().expect("bug: lock was poisoned"))
            .collect();

        Ok(ir::Module {types, functions})
    }

    fn infer_and_check_method(
        &self,
        self_ty: TyId,
        sig: ir::FuncSig<'a>,
        method: &'a ast::Function<'a>,
    ) -> Result<ir::Function<'a>, Error> {
        // `ty_ir_method` is a copy of the function's AST with any generated type variables placed inline
        let (constraints, ty_ir_method) = ConstraintSet::method(self_ty, sig, method, self.decls, self.prims)?;
        let solution = constraints.solve(self.prims)?;
        Ok(ty_ir_method.apply_subst(&solution))
    }

    fn infer_and_check_func(
        &self,
        sig: ir::FuncSig<'a>,
        func: &'a ast::Function<'a>,
    ) -> Result<ir::Function<'a>, Error> {
        // `ty_ir_func` is a copy of the function's AST with any generated type variables placed inline
        let (constraints, ty_ir_func) = ConstraintSet::function(sig, func, self.decls, self.prims)?;
        let solution = constraints.solve(self.prims)?;
        Ok(ty_ir_func.apply_subst(&solution))
    }
}
