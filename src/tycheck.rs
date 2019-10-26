//! Type inference and checking.

mod subst;
mod scope;
mod constraints;
mod solve;
mod tyir;

use snafu::{Snafu, OptionExt};
use rayon::prelude::*;

use crate::{ast, ir};
use crate::resolve::{ProgramDecls, DeclMap, TyId, TypeInfo};
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

pub fn infer_and_check<'a>(decls: &'a ProgramDecls<'a>) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    let top_level_module = infer_and_check_module(top_level_decls, prims)?;

    Ok(ir::Program {top_level_module})
}

fn infer_and_check_module<'a>(
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Module<'a>, Error> {
    // Able to use par_bridge here because types can be checked in any order
    let types = mod_decls.types()
        .par_bridge()
        // No need to check external types
        .filter(|(_, ty_info)| !ty_info.is_extern)
        .map(|(ty_id, ty_info)| check_ty_decl(ty_id, ty_info, mod_decls, prims))
        .collect::<Result<Vec<_>, _>>()?;

    // Able to use par_bridge here because functions can be type checked in any order
    let functions = mod_decls.functions()
        .par_bridge()
        // No need to check external functions
        .filter(|func| !func.is_extern)
        .map(|func| infer_and_check_func(func, mod_decls, prims))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(ir::Module {types, functions})
}

fn check_ty_decl<'a>(
    self_ty: TyId,
    ty_info: &'a TypeInfo,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Struct<'a>, Error> {
    let TypeInfo {name, is_extern, constructors: _, fields, methods} = ty_info;
    assert!(!is_extern, "bug: should not be type checking extern type");

    // Ensure that all field types actually exist
    let fields = fields.iter().map(|(&field_name, ty)| Ok(match ty {
        ast::Ty::Unit => (field_name, prims.unit()),
        ast::Ty::SelfType => (field_name, self_ty),
        &ast::Ty::Named(ty_name) => {
            let ty_id = mod_decls.type_id(&ty_name)
                .with_context(|| UnresolvedType {name: ty_name})?;
            (field_name, ty_id)
        },
    })).collect::<Result<ir::FieldTys, _>>()?;

    // Type check methods
    let methods = methods.iter().map(|(&method_name, method)| {
        let method = infer_and_check_method(self_ty, method, mod_decls, prims)?;
        Ok((method_name, method))
    }).collect::<Result<ir::MethodDecls, _>>()?;

    Ok(ir::Struct {name, fields, methods})
}

fn infer_and_check_method<'a>(
    self_ty: TyId,
    method: &'a ast::Function<'a>,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Function<'a>, Error> {
    // `ty_ir_method` is a copy of the function's AST with any generated type variables placed inline
    let (constraints, ty_ir_method) = ConstraintSet::method(self_ty, method, mod_decls, prims)?;
    let solution = constraints.solve(prims)?;
    Ok(ty_ir_method.apply_subst(&solution))
}

fn infer_and_check_func<'a>(
    func: &'a ast::Function<'a>,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Function<'a>, Error> {
    // `ty_ir_func` is a copy of the function's AST with any generated type variables placed inline
    let (constraints, ty_ir_func) = ConstraintSet::function(func, mod_decls, prims)?;
    let solution = constraints.solve(prims)?;
    Ok(ty_ir_func.apply_subst(&solution))
}
