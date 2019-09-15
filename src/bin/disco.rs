use std::io::Write;
use std::fs::File;
use std::error::Error;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;
use disco::{
    ast,
    tycheck,
    codegen,
    resolve::ProgramDecls,
};

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = Path::new("foo.disco");

    let input_program = r#"
        fn main() {}
    "#;

    let program = ast::Program::parse(input_program)?;
    let decls = ProgramDecls::new(program)?;
    let program_ir = tycheck::infer_and_check(decls)?;
    let code = codegen::executable(&program_ir)?;

    // Check that the path and stem are valid
    let stem = match (input_path.file_stem(), input_path.extension()) {
        (Some(stem), Some(ext)) if ext == "disco" => stem,
        _ => Err("Invalid input path. Must use extension `disco`".to_string())?,
    };

    // Default output path is the input path without its stem
    let output_path = Path::new(stem);

    //TODO: Add proper logging
    //TODO: Add support for outputing the generated C code (CLI flag)
    println!("{}", code);

    // Write the generated code to a temporary file so we can run it through a C compiler
    let tmp_dir = TempDir::new()?;
    let code_file_path = tmp_dir.path().join("main.c");

    // Drop the file as soon as possible so it finishes being written to
    // and so it is closed when we close the temporary directory
    {
        let mut code_file = File::create(&code_file_path)?;
        writeln!(code_file, "{}", code)?;
    }

    // Enabling all the warnings and making them an error because this compiler should never get to
    // this point if it can't produce completely valid C.
    // See: https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html
    //  Or: https://clang.llvm.org/docs/DiagnosticsReference.html
    let warning_flags = &["-Werror", "-Wall", "-Wextra", "-Wformat=2", "-Wshadow",
        "-Wpointer-arith", "-Wcast-qual", "-Wno-missing-braces"];
    // Run the C compiler and copy the result back
    let status = Command::new("clang")
        .arg("-std=c99")
        // Maximum optimization level
        .arg("-O3")
        //TODO: Link with runtime
        .args(warning_flags)
        .arg(code_file_path)
        .arg("-o")
        .arg(output_path)
        .status()?;

    if !status.success() {
        panic!("bug: code generation failed");
    }

    // By closing the `TempDir` explicitly we can check that it has been deleted successfully. If
    // we don't close it explicitly, the directory will still be deleted when `tmp_dir` goes out of
    // scope, but we won't know whether deleting the directory succeeded.
    // IMPORTANT: All handles to files in this directory must be closed at this point.
    tmp_dir.close()?;

    Ok(())
}
