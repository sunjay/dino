//! Type inference and checking.

use snafu::Snafu;

use crate::{ast, ir};
use crate::resolve::ProgramDecls;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
}

pub fn infer_and_check(decls: ProgramDecls) -> Result<ir::Program, Error> {
    let ProgramDecls {top_level_decls} = decls;

    let top_level_module = ir::Module {
        decls: top_level_decls.iter().map(infer_and_check_decl).collect::<Result<_, _>>()?,
    };

    Ok(ir::Program {top_level_module})
}

fn infer_and_check_decl<'a>(decl: &ast::Decl<'a>) -> Result<ir::Decl<'a>, Error> {
    Ok(match decl {
        ast::Decl::Function(func) => ir::Decl::Function(infer_and_check_function(func)?),
    })
}

fn infer_and_check_function<'a>(func: &ast::Function<'a>) -> Result<ir::Function<'a>, Error> {
    let ast::Function {name, body} = func;

    //TODO: Support variable shadowing
    let body = ir::Block {
        stmts: body.stmts.iter().map(|stmt| Ok(match stmt {
            //TODO: Perform type checking
            ast::Stmt::VarDecl(ast::VarDecl {ident, ty, expr}) => ir::Stmt::VarDecl(ir::VarDecl {
                ident,
                ty,
                expr: infer_and_check_expr(expr)?,
            }),
        })).collect::<Result<_, _>>()?,
    };

    Ok(ir::Function {name, body})
}

fn infer_and_check_expr(expr: &ast::Expr) -> Result<ir::Expr, Error> {
    //TODO: Perform type checking
    Ok(match expr {
        &ast::Expr::IntegerLiteral(value) => ir::Expr::IntegerLiteral(value),
    })
}
