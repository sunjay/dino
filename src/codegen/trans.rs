//! Translates IR into generated code

use crate::ir;

use super::*;

use snafu::Snafu;

/// Code generation errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`main` function not found"))]
    NoEntryPoint,
}

/// Generates an executable program from the given IR
pub fn executable(prog: &ir::Program) -> Result<CExecutableProgram, Error> {
    let ir::Program {top_level_module} = prog;
    let ir::Module {decls} = top_level_module;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for decl in decls {
        match decl {
            ir::Decl::Function(ir::Function {name}) if *name == "main" => {
                entry_point = Some(CEntryPoint {
                    body: CFunctionBody {}, //TODO
                });
            },
            ir::Decl::Function(func) => functions.push(gen_function(func)?),
        }
    }

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}

fn gen_function(_func: &ir::Function) -> Result<CFunction, Error> {
    unimplemented!() //TODO
}
