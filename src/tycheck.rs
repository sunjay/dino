//! Type inference and checking.

mod subst;
mod scope;
mod constraints;
mod solve;
mod tyir;

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
}

pub fn infer_and_check<'a>(decls: &'a ProgramDecls<'a>) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    let top_level_module = infer_and_check_module(top_level_decls, prims)?;

    Ok(ir::Program {top_level_module})
}

fn infer_and_check_module<'a>(
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Module<'a>, Error> {
    //TODO: Type check methods

    // Able to use par_bridge here because functions can be type checked in any order
    let functions = mod_decls.functions()
        .par_bridge()
        // No need to check external functions
        .filter(|func| !func.is_extern)
        .map(|func| {
            infer_and_check_func(func, mod_decls, prims)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ir::Module {functions})
}

fn infer_and_check_func<'a>(
    func: &'a ast::Function<'a>,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Function<'a>, Error> {
    // `ty_ir_func` is a copy of the function's AST with any generated type variables placed inline
    let (constraints, ty_ir_func) = ConstraintSet::generate(func, mod_decls, prims)?;
    let solution = constraints.solve(prims)?;
    Ok(ty_ir_func.apply_subst(&solution))
}
