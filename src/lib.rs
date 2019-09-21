/// Information about the language runtime
pub mod runtime {
    // Load information about the language runtime
    include!(concat!(env!("OUT_DIR"), "/runtime.rs"));

    use std::{fs, io};
    use std::path::Path;

    /// Places the language runtime in the given directory.
    ///
    /// This files must be present in the same directory as the generated code file.
    pub fn write_runtime_files<P: AsRef<Path>>(dir_path: P) -> io::Result<()> {
        let dir_path = dir_path.as_ref();
        fs::write(dir_path.join(RUNTIME_HEADER_FILENAME), RUNTIME_HEADER_CONTENTS)?;
        fs::write(dir_path.join(RUNTIME_LIB_FILENAME), RUNTIME_LIB_CONTENTS)?;
        Ok(())
    }
}

pub mod ast;
pub mod codegen;
pub mod ir;
pub mod resolve;
pub mod tycheck;

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
    let decls = resolve::ProgramDecls::new(program)
        .with_context(|| DuplicateDecl {path: path.to_path_buf()})?;
    let program_ir = tycheck::infer_and_check(&decls)
        .with_context(|| TypeError {path: path.to_path_buf()})?;
    let code = codegen::executable(&program_ir)
        .with_context(|| CodeGenerationError {path: path.to_path_buf()})?;

    Ok(code)
}
