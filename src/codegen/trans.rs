//! Translates IR into generated code

use crate::ir;

use super::*;

use snafu::Snafu;

/// Code generation errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`main` function not found"))]
    NoEntryPoint,
    #[snafu(display("cannot find type `{}` in this scope", ty))]
    UnknownType {
        ty: String,
    },
}

/// Generates an executable program from the given IR
pub fn executable(prog: &ir::Program) -> Result<CExecutableProgram, Error> {
    let ir::Program {top_level_module} = prog;
    let ir::Module {decls} = top_level_module;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for decl in decls {
        match decl {
            //TODO: Validate that `main` takes zero arguments and has no return type
            ir::Decl::Function(ir::Function {name, body}) if *name == "main" => {
                entry_point = Some(CEntryPoint {
                    body: gen_function_body(body)?,
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

fn gen_function_body(block: &ir::Block) -> Result<CFunctionBody, Error> {
    let ir::Block {stmts} = block;

    let stmts = stmts.iter().map(|stmt| Ok(match stmt {
        ir::Stmt::VarDecl(var_decl) => CStmt::VarDecl(gen_var_decl(var_decl)?),
    })).collect::<Result<_, _>>()?;

    Ok(CFunctionBody {stmts})
}

fn gen_var_decl(var_decl: &ir::VarDecl) -> Result<CVarDecl, Error> {
    let ir::VarDecl {ident, ty, expr} = var_decl;

    //TODO: Support variable shadowing by mangling name appropriately and then re-assigning everywhere
    Ok(CVarDecl {
        mangled_name: ident.to_string(),
        ty: lookup_type(ty)?,
        init_expr: CInitializerExpr::Expr(gen_expr(expr)?)
    })
}

fn gen_expr(expr: &ir::Expr) -> Result<CExpr, Error> {
    Ok(match expr {
        &ir::Expr::IntegerLiteral(value) => CExpr::IntegerLiteral(value),
    })
}

fn lookup_type(ty: &ir::Ident) -> Result<CType, Error> {
    match *ty {
        "int" => Ok(CType::DInt),
        ty => Err(Error::UnknownType {ty: ty.to_string()}),
    }
}