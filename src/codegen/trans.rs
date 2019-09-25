//! Translates IR into generated code

use super::*;

use snafu::Snafu;

use crate::ir;
use crate::resolve::{TyId, ProgramDecls, DeclMap};

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
    let ir::Module {decls} = top_level_module;

    let ProgramDecls {top_level_decls: mod_scope, prims} = program_scope;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for decl in decls {
        match decl {
            ir::Decl::Function(ir::Function {name, sig, body}) if *name == "main" => {
                // The main function must have no return type and no arguments
                if sig.return_type != prims.unit() || !sig.params.is_empty() {
                    return Err(Error::InvalidEntryPointType);
                }

                // Note that it is guaranteed that `entry_point` will only be assigned once since
                // the IR assumes that all declaration names have been checked to be unique within
                // a given module.
                debug_assert!(entry_point.is_none(), "bug: allowed multiple entry points");

                entry_point = Some(CEntryPoint {
                    body: gen_function_body(body, mod_scope)?,
                });
            },
            ir::Decl::Function(func) => functions.push(gen_function(func, mod_scope)?),
        }
    }

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}

fn gen_function(
    func: &ir::Function,
    mod_scope: &DeclMap,
) -> Result<CFunction, Error> {
    unimplemented!() //TODO
}

fn gen_function_body(
    block: &ir::Block,
    mod_scope: &DeclMap,
) -> Result<CFunctionBody, Error> {
    let ir::Block {stmts} = block;

    let stmts = stmts.iter().map(|stmt| Ok(match stmt {
        ir::Stmt::VarDecl(var_decl) => CStmt::VarDecl(gen_var_decl(var_decl, mod_scope)?),
        ir::Stmt::Expr(expr) => CStmt::Expr(gen_expr(expr, mod_scope)?),
    })).collect::<Result<_, _>>()?;

    Ok(CFunctionBody {stmts})
}

fn gen_var_decl(
    var_decl: &ir::VarDecl,
    mod_scope: &DeclMap,
) -> Result<CVarDecl, Error> {
    let ir::VarDecl {ident, ty, expr} = var_decl;

    //TODO: Support variable shadowing by mangling name appropriately and then re-assigning everywhere
    Ok(CVarDecl {
        mangled_name: ident.to_string(),
        ty: lookup_type(ty, mod_scope),
        init_expr: CInitializerExpr::Expr(gen_expr(expr, mod_scope)?)
    })
}

fn gen_expr(
    expr: &ir::Expr,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    //TODO: Determine function names to call by dispatching on the type
    Ok(match expr {
        ir::Expr::Call(call, ty) => unimplemented!(),
        &ir::Expr::IntegerLiteral(value, ty) => CExpr::IntegerLiteral(value),
        &ir::Expr::Var(name, ty) => unimplemented!(),
    })
}

fn lookup_type(ty: &TyId, mod_scope: &DeclMap) -> String {
    mod_scope.type_extern_name(ty)
        .expect("bug: unknown type was allowed to get through to codegen")
        .to_string()
}
