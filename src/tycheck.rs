//! Type inference and checking.

use snafu::Snafu;

use crate::{ast, ir};
use crate::resolve::ProgramDecls;

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
    let ast::Function {name} = func;
    Ok(ir::Function {name})
}
