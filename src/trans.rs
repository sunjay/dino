//! Translates IR into generated code

mod mangler;
mod function;

use std::iter;

use snafu::Snafu;

use crate::ir;
use crate::resolve2::{ProgramDecls, DeclMap};
use crate::primitives2::Primitives;
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

    let mut structs = Vec::new();
    let methods = gen_types(types, mod_scope, prims, &mut structs)?;

    let mut entry_point = None;
    let mut functions = gen_functions(functions, mod_scope, prims, &mut entry_point)?;
    functions.extend(methods);

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {structs, functions, entry_point})
}

/// Returns the functions generated for the methods of all the types
fn gen_types(
    types: &[ir::Struct],
    mod_scope: &DeclMap,
    _prims: &Primitives,
    structs: &mut Vec<CStruct>,
) -> Result<Vec<CFunction>, Error> {
    types.iter().map(|struct_decl| {
        let ir::Struct {name, is_extern: _, fields, methods} = struct_decl;

        //TODO: Mangle struct names based on `is_extern`
        let struct_mangled_name = name.to_string();
        structs.push(CStruct {
            mangled_name: struct_mangled_name.clone(),
            fields: fields.iter().map(|(name, &ty_id)| CStructField {
                //TODO: Mangle struct field names
                mangled_name: name.to_string(),
                //TODO: Get mangled name
                ty: CTy::pointer(mod_scope.type_name(ty_id).to_string()),
            }).collect(),
        });

        let methods = methods.iter().map(move |(method_name, func)| {
            let func = ir::Function {
                //TODO: Figure out a better way to generate this name
                name: &format!("{}__{}", struct_mangled_name, method_name),
                ..func.clone()
            };
            FunctionCodeGenerator::generate(&func, mod_scope)
        });

        iter::once({
            // Generate a constructor that takes a value of this struct and puts it on the heap
            //let constructor = ir::Function {
            //    //TODO: Figure out a better way to generate this name and store it with the struct
            //    // for use later on in code generation
            //    name: &format!("init_{}", struct_mangled_name),
            //};
            //FunctionCodeGenerator::generate(&constructor, mod_scope)
            unimplemented!()
        }).chain(methods)
    }).flatten().collect()
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
