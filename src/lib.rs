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
///
/// This function MUST return one of the types provided in `tys`.
fn resolve_ambiguity(tys: &HashSet<TyId>, prims: &Primitives) -> Option<TyId> {
    let has_int = tys.contains(&prims.int());
    let has_real = tys.contains(&prims.real());
    let has_complex = tys.contains(&prims.complex());

    // Need to be very careful here to always match on the entire set of types. We don't want to
    // turn {int, real, string} into {int}. (Not that this should be possible.)
    match tys.len() {
        // Since integer literals can be {int, real} or {int, complex} or {int, real, complex},
        // resolve to {int} where possible
        2 if has_int && has_real => Some(prims.int()),
        2 if has_int && has_complex => Some(prims.int()),
        3 if has_int && has_real && has_complex => Some(prims.int()),

        // Similarly, real number literals can be {real, complex}, so resolve to {real} if there is
        // an ambiguity
        2 if has_real && has_complex => Some(prims.real()),

        // Did not successfully resolve the ambiguity
        _ => None,
    }
}

fn insert_prelude(decls: &mut resolve::ProgramDecls) {
    //TODO: Figure out how to do this properly without hard coding things
    use crate::ast::*;

    let decls = &mut decls.top_level_decls;

    decls.insert_func(Function::new_extern("unit_eq", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Unit},
            FuncParam {name: "right", ty: Ty::Unit},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_unit", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Unit},
        ],
    })).unwrap();

    decls.insert_func(Function::new_extern("bool_eq", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("bool")},
            FuncParam {name: "right", ty: Ty::Named("bool")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bool_and", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("bool")},
            FuncParam {name: "right", ty: Ty::Named("bool")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bool_or", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("bool")},
            FuncParam {name: "right", ty: Ty::Named("bool")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bool_not", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("bool")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_bool", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("bool")},
        ],
    })).unwrap();

    decls.insert_func(Function::new_extern("int_eq", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("int_gt", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("int_gte", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("int_lt", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("int_lte", FuncSig {
        return_type: Ty::Named("bool"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();

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
    decls.insert_func(Function::new_extern("mul_int", FuncSig {
        return_type: Ty::Named("int"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("int")},
            FuncParam {name: "right", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("div_int", FuncSig {
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

    decls.insert_func(Function::new_extern("bstr_len", FuncSig {
        return_type: Ty::Named("int"),
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("bstr")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bstr_concat", FuncSig {
        return_type: Ty::Named("bstr"),
        params: vec![
            FuncParam {name: "left", ty: Ty::Named("bstr")},
            FuncParam {name: "right", ty: Ty::Named("bstr")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bstr_slice", FuncSig {
        return_type: Ty::Named("bstr"),
        params: vec![
            FuncParam {name: "string", ty: Ty::Named("bstr")},
            FuncParam {name: "start", ty: Ty::Named("int")},
            FuncParam {name: "end", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("bstr_get", FuncSig {
        return_type: Ty::Named("bstr"),
        params: vec![
            FuncParam {name: "string", ty: Ty::Named("bstr")},
            FuncParam {name: "index", ty: Ty::Named("int")},
        ],
    })).unwrap();
    decls.insert_func(Function::new_extern("print_bstr", FuncSig {
        return_type: Ty::Unit,
        params: vec![
            FuncParam {name: "value", ty: Ty::Named("bstr")},
        ],
    })).unwrap();
}
