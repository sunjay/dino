//! Translates IR into generated code

mod mangler;
mod function;

use snafu::Snafu;

use crate::ir;
use crate::resolve::{ProgramDecls, DeclMap};
use crate::primitives::Primitives;
use crate::codegen::*;

use function::FunctionCodeGenerator;

/// Code generation errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`main` function not found"))]
    NoEntryPoint,
    #[snafu(display("`main` function has wrong type"))]
    InvalidEntryPointType,
}

/// Generates an executable program from the given IR
pub fn executable(prog: &ir::Program, program_scope: &ProgramDecls) -> Result<CExecutableProgram, Error> {
    let ir::Program {top_level_module} = prog;
    let ir::Module {types, functions} = top_level_module;

    let ProgramDecls {top_level_decls: mod_scope, prims} = program_scope;

    let mut entry_point = None;
    let functions = gen_functions(functions, mod_scope, prims, &mut entry_point)?;

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}

fn gen_functions(
    functions: &[ir::Function],
    mod_scope: &DeclMap,
    prims: &Primitives,
    entry_point: &mut Option<CEntryPoint>,
) -> Result<Vec<CFunction>, Error> {
    functions.iter().filter_map(|func| {
        // A "main" function in the top level declarations of a program must be the entry point
        if func.name == "main" {
            let ir::Function {sig, ..} = func;
            // The main function must have no return type and no arguments
            if sig.return_type != prims.unit() || !sig.params.is_empty() {
                return Some(Err(Error::InvalidEntryPointType));
            }

            // Note that it is guaranteed that `entry_point` will only be assigned once since
            // the IR assumes that all declaration names have been checked to be unique within
            // a given module.
            debug_assert!(entry_point.is_none(), "bug: allowed multiple entry points");

            // Take the generated body and put it in the right struct
            let CFunction {sig: _, body} = match FunctionCodeGenerator::generate(func, mod_scope) {
                Ok(func) => func,
                Err(err) => return Some(Err(err)),
            };
            *entry_point = Some(CEntryPoint {body});

            // Filter this function out
            None

        } else {
            Some(FunctionCodeGenerator::generate(func, mod_scope))
        }
    }).collect()
}
