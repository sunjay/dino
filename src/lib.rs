#![deny(unused_must_use)]

pub mod ast;
pub mod codegen;
pub mod trans;
pub mod ir;
pub mod resolve2;
pub mod primitives2;
pub mod tycheck;
pub mod runtime;
pub mod dino_std;
pub mod gc_lib;

pub mod diagnostics;
pub mod fmt_ctx;
pub mod primitives;
pub mod hir;
pub mod nir;
pub mod resolve;
pub mod cgenir;
pub mod cir;
pub mod package;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use snafu::{Snafu, ResultExt};

use crate::codegen::CExecutableProgram;

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
    ResolveError {
        path: PathBuf,
        source: resolve2::Error,
    },
    #[snafu(display("In '{}': {}", path.display(), source))]
    TypeError {
        path: PathBuf,
        source: tycheck::Error,
    },
    #[snafu(display("In '{}': {}", path.display(), source))]
    CodeGenerationError {
        path: PathBuf,
        source: trans::Error,
    },
}

/// Compiles the given file into executable code
pub fn compile_executable<P: AsRef<Path>>(path: P) -> Result<CExecutableProgram, Error> {
    let path = path.as_ref();
    let input_program = fs::read_to_string(path)
        .with_context(|| IOError {path: path.to_path_buf()})?;
    let program = ast::Program::parse(&input_program)
        .with_context(|| ParseError {path: path.to_path_buf()})?;
    let (mut decls, resolved_ast) = resolve2::ProgramDecls::extract(&program)
        .with_context(|| ResolveError {path: path.to_path_buf()})?;
    insert_prelude(&mut decls);
    let program_ir = tycheck::infer_and_check(resolved_ast, &decls)
        .with_context(|| TypeError {path: path.to_path_buf()})?;
    let code = trans::executable(&program_ir, &decls)
        .with_context(|| CodeGenerationError {path: path.to_path_buf()})?;

    Ok(code)
}

fn insert_prelude(decls: &mut resolve2::ProgramDecls) {
    //TODO: Figure out how to do this properly without hard coding things

    use crate::ir::{FuncSig, FuncParam};
    use crate::resolve2::FunctionInfo;

    let prims = &decls.prims;
    let decls = &mut decls.top_level_decls;

    decls.insert_func(FunctionInfo::new_extern("unit_eq", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.unit()},
            FuncParam {name: "right", ty: prims.unit()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_unit", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.unit()},
        ],
    })).unwrap();

    decls.insert_func(FunctionInfo::new_extern("bool_eq", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bool()},
            FuncParam {name: "right", ty: prims.bool()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bool_and", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bool()},
            FuncParam {name: "right", ty: prims.bool()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bool_or", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bool()},
            FuncParam {name: "right", ty: prims.bool()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bool_not", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "value", ty: prims.bool()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_bool", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.bool()},
        ],
    })).unwrap();

    decls.insert_method(prims.int(), "eq", FunctionInfo::new_extern("int__eq", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "gt", FunctionInfo::new_extern("int__gt", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "gte", FunctionInfo::new_extern("int__gte", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "lt", FunctionInfo::new_extern("int__lt", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "lte", FunctionInfo::new_extern("int__lte", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();

    decls.insert_method(prims.int(), "add", FunctionInfo::new_extern("int__add", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "other", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "sub", FunctionInfo::new_extern("int__sub", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "mul", FunctionInfo::new_extern("int__mul", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "div", FunctionInfo::new_extern("int__div", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "rem", FunctionInfo::new_extern("int__rem", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
            FuncParam {name: "right", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_method(prims.int(), "neg", FunctionInfo::new_extern("int__neg", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "self", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_int", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.int()},
        ],
    })).unwrap();

    decls.insert_func(FunctionInfo::new_extern("add_real", FuncSig {
        return_type: prims.real(),
        params: vec![
            FuncParam {name: "left", ty: prims.real()},
            FuncParam {name: "right", ty: prims.real()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("sub_real", FuncSig {
        return_type: prims.real(),
        params: vec![
            FuncParam {name: "left", ty: prims.real()},
            FuncParam {name: "right", ty: prims.real()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_real", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.real()},
        ],
    })).unwrap();

    decls.insert_func(FunctionInfo::new_extern("add_complex", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.complex()},
            FuncParam {name: "right", ty: prims.complex()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("add_real_complex", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.real()},
            FuncParam {name: "right", ty: prims.complex()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("add_complex_real", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.complex()},
            FuncParam {name: "right", ty: prims.real()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("sub_complex", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.complex()},
            FuncParam {name: "right", ty: prims.complex()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("sub_real_complex", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.real()},
            FuncParam {name: "right", ty: prims.complex()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("sub_complex_real", FuncSig {
        return_type: prims.complex(),
        params: vec![
            FuncParam {name: "left", ty: prims.complex()},
            FuncParam {name: "right", ty: prims.real()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_complex", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.complex()},
        ],
    })).unwrap();

    decls.insert_func(FunctionInfo::new_extern("bstr_len", FuncSig {
        return_type: prims.int(),
        params: vec![
            FuncParam {name: "value", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_eq", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_gt", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_gte", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_lt", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_lte", FuncSig {
        return_type: prims.bool(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_concat", FuncSig {
        return_type: prims.bstr(),
        params: vec![
            FuncParam {name: "left", ty: prims.bstr()},
            FuncParam {name: "right", ty: prims.bstr()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_slice", FuncSig {
        return_type: prims.bstr(),
        params: vec![
            FuncParam {name: "string", ty: prims.bstr()},
            FuncParam {name: "start", ty: prims.int()},
            FuncParam {name: "end", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("bstr_get", FuncSig {
        return_type: prims.bstr(),
        params: vec![
            FuncParam {name: "string", ty: prims.bstr()},
            FuncParam {name: "index", ty: prims.int()},
        ],
    })).unwrap();
    decls.insert_func(FunctionInfo::new_extern("print_bstr", FuncSig {
        return_type: prims.unit(),
        params: vec![
            FuncParam {name: "value", ty: prims.bstr()},
        ],
    })).unwrap();

    decls.insert_func(FunctionInfo::new_extern("read_line_bstr", FuncSig {
        return_type: prims.bstr(),
        params: Vec::new(),
    })).unwrap();
}
