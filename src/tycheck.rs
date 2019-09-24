//! Type inference and checking.

use snafu::Snafu;

use crate::{ast, ir};
use crate::resolve::ProgramDecls;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
}

pub fn infer_and_check<'a>(decls: &'a ProgramDecls<'a>) -> Result<ir::Program<'a>, Error> {
    let ProgramDecls {top_level_decls, prims} = decls;

    unimplemented!()
}

