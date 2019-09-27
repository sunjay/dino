pub mod ast;
pub mod codegen;
pub mod ir;
pub mod resolve;
pub mod primitives;
pub mod tycheck;
pub mod runtime;
pub mod dino_std;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::collections::HashSet;

use maplit::hashset;
use snafu::{Snafu, ResultExt};

use crate::codegen::CExecutableProgram;
use crate::resolve::TyId;
use crate::primitives::Primitives;

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Could not read '{}': {}", path.display(), source))]
    IOError {
        path: PathBuf,
        source: io::Error,
    },
    #[snafu(display("Parse error while reading '{}': {}", path.display(), source))]
    ParseError {
        path: PathBuf,
        source: ast::ParseError,
    },
    #[snafu(display("In '{}': {}", path.display(), source))]
    DuplicateDecl {
        path: PathBuf,
        source: resolve::DuplicateDecl,
    },
    #[snafu(display("In '{}': {}", path.display(), source))]
    TypeError {
        path: PathBuf,
        source: tycheck::Error,
    },
    #[snafu(display("In '{}': {}", path.display(), source))]
    CodeGenerationError {
        path: PathBuf,
        source: codegen::Error,
    },
}

/// Compiles the given file into executable code
pub fn compile_executable<P: AsRef<Path>>(path: P) -> Result<CExecutableProgram, Error> {
    let path = path.as_ref();
    let input_program = fs::read_to_string(path)
        .with_context(|| IOError {path: path.to_path_buf()})?;
    let program = ast::Program::parse(&input_program)
        .with_context(|| ParseError {path: path.to_path_buf()})?;
    let mut decls = resolve::ProgramDecls::new(program)
        .with_context(|| DuplicateDecl {path: path.to_path_buf()})?;
    insert_prelude(&mut decls);
    let program_ir = tycheck::infer_and_check(&decls, |tys| resolve_ambiguity(tys, &decls.prims))
        .with_context(|| TypeError {path: path.to_path_buf()})?;
    let code = codegen::executable(&program_ir, &decls)
        .with_context(|| CodeGenerationError {path: path.to_path_buf()})?;

    Ok(code)
}

/// Resolves ambiguous type checking situations where possible
fn resolve_ambiguity(tys: &HashSet<TyId>, prims: &Primitives) -> Option<TyId> {
    // Since integer literals can be {int, real}, resolve to {int} where possible
    if *tys == hashset!{prims.int(), prims.real()} {
        Some(prims.int())
    } else {
        None
    }
}

fn insert_prelude(decls: &mut resolve::ProgramDecls) {
    //TODO: Figure out how to do this properly without hard coding things
    use crate::ast::*;

    let decls = &mut decls.top_level_decls;

    decls.insert_func(Function::new_extern("add_int", FuncSig {
        return_type: Ty::Named("int"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("sub_int", FuncSig {
        return_type: Ty::Named("int"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_int", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("int")},
        ],
    })).unwrap();

    decls.insert_func(Function::new_extern("add_real", FuncSig {
        return_type: Ty::Named("real"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("real")},
            FuncParam {name: "right", ty: Ty::Named("real")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("sub_real", FuncSig {
        return_type: Ty::Named("real"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("real")},
            FuncParam {name: "right", ty: Ty::Named("real")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_real", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("real")},
        ],
    })).unwrap();

    decls.insert_func(Function::new_extern("add_complex", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("complex")},
            FuncParam {name: "right", ty: Ty::Named("complex")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("add_real_complex", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("real")},
            FuncParam {name: "right", ty: Ty::Named("complex")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("add_complex_real", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("complex")},
            FuncParam {name: "right", ty: Ty::Named("real")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("sub_complex", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("complex")},
            FuncParam {name: "right", ty: Ty::Named("complex")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("sub_real_complex", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("real")},
            FuncParam {name: "right", ty: Ty::Named("complex")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("sub_complex_real", FuncSig {
        return_type: Ty::Named("complex"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("complex")},
            FuncParam {name: "right", ty: Ty::Named("real")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_complex", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("complex")},
        ],
    })).unwrap();
}
