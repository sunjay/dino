//! Type inference and checking.

mod constraints;
mod tyir;

pub use constraints::Error;

use crate::{ast, ir};
use crate::resolve::{ProgramDecls, DeclMap, Primitives};

use constraints::ConstraintSet;

pub fn infer_and_check<'a>(decls: &'a ProgramDecls<'a>) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    let top_level_module = infer_and_check_module(top_level_decls, prims)?;

    Ok(ir::Program {top_level_module})
}

fn infer_and_check_module<'a>(
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Module<'a>, Error> {
    let decls = mod_decls.functions()
        // No need to check external functions
        .filter(|func| !func.is_extern)
        .map(|func| infer_and_check_func(func, mod_decls, prims).map(ir::Decl::Function))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ir::Module {decls})
}

fn infer_and_check_func<'a>(
    func: &'a ast::Function<'a>,
    mod_decls: &'a DeclMap<'a>,
    prims: &Primitives,
) -> Result<ir::Function<'a>, Error> {
    // A copy of the function's AST with any generated type variables placed inline
    let (constraints, ty_ir_func) = ConstraintSet::generate(func, mod_decls, prims)?;
    let solution = constraints.solve()?;
    Ok(ty_ir_func.apply_subst(&solution))
}
