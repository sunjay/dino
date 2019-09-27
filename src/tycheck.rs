//! Type inference and checking.

mod constraints;
mod solve;
mod tyir;

use std::collections::HashSet;

use snafu::Snafu;
use rayon::prelude::*;

use crate::{ast, ir};
use crate::resolve::{ProgramDecls, DeclMap, TyId};
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
    #[snafu(display("mismatched types"))]
    MismatchedTypes {
        //TODO: Add type and span info
    },
}

pub fn infer_and_check<'a>(
    decls: &'a ProgramDecls<'a>,
    resolve_ambiguity: impl Fn(&HashSet<TyId>) -> Option<TyId> + Send + Sync,
) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    let top_level_module = infer_and_check_module(top_level_decls, prims, &resolve_ambiguity)?;

    Ok(ir::Program {top_level_module})
}

fn infer_and_check_module<'a, F: Fn(&HashSet<TyId>) -> Option<TyId> + Send + Sync>(
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
    resolve_ambiguity: &F,
) -> Result<ir::Module<'a>, Error> {
    // Able to use par_bridge here because functions can be type checked in any order
    let decls = mod_decls.functions()
        .par_bridge()
        // No need to check external functions
        .filter(|func| !func.is_extern)
        .map(|func| {
            infer_and_check_func(func, mod_decls, prims, resolve_ambiguity).map(ir::Decl::Function)
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ir::Module {decls})
}

fn infer_and_check_func<'a>(
    func: &'a ast::Function<'a>,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
    resolve_ambiguity: impl Fn(&HashSet<TyId>) -> Option<TyId> + Send + Sync,
) -> Result<ir::Function<'a>, Error> {
    // `ty_ir_func` is a copy of the function's AST with any generated type variables placed inline
    let (constraints, ty_ir_func) = ConstraintSet::generate(func, mod_decls, prims)?;
    let solution = constraints.solve(resolve_ambiguity)?;
    Ok(ty_ir_func.apply_subst(&solution))
}
