//! Translates IR into generated code

mod mangler;
mod function;

use snafu::Snafu;

use crate::ir;
use crate::resolve::ProgramDecls;
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
    let ir::Module {functions: mod_functions} = top_level_module;

    let ProgramDecls {top_level_decls: mod_scope, prims} = program_scope;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for func in mod_functions {
        // A "main" function in the top level declarations of a program must be the entry point
        if func.name == "main" {
            let ir::Function {sig, ..} = func;
            // The main function must have no return type and no arguments
            if sig.return_type != prims.unit() || !sig.params.is_empty() {
                return Err(Error::InvalidEntryPointType);
            }

            // Note that it is guaranteed that `entry_point` will only be assigned once since
            // the IR assumes that all declaration names have been checked to be unique within
            // a given module.
            debug_assert!(entry_point.is_none(), "bug: allowed multiple entry points");

            // Take the generated body and put it in the right struct
            let CFunction {sig: _, body} = FunctionCodeGenerator::generate(func, mod_scope)?;
            entry_point = Some(CEntryPoint {body});

        } else {
            functions.push(FunctionCodeGenerator::generate(func, mod_scope)?);
        }
    }

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}
