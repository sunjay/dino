use std::env;
use std::io::Write;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::TempDir;
use structopt::StructOpt;
use terminator::Terminator;

#[derive(Debug, StructOpt)]
#[structopt(name = "dino", about)]
struct CompilerOptions {
    /// The program to compile
    #[structopt(name = "input", parse(from_os_str))]
    program_path: PathBuf,
    /// Write output to <file>
    #[structopt(short = "o", name = "file")]
    output_path: Option<PathBuf>,
}

fn main() -> Result<(), Terminator> {
    let CompilerOptions {program_path, output_path} = CompilerOptions::from_args();

    let code = dino::compile_executable2(&program_path)?;

    // Check that the path and stem are valid
    let program_stem = match (program_path.file_stem(), program_path.extension()) {
        (Some(stem), Some(ext)) if !stem.is_empty() && ext == "dino" => stem,
        _ => Err("Invalid input path. Must use extension `dino`".to_string())?,
    };

    // Default output path is the input path without its stem
    let output_path = output_path.as_ref().map(|p| p.as_path())
        .unwrap_or_else(|| Path::new(program_stem));
    // Append the current directory to the output path if necessary
    let output_path = if output_path.is_absolute() {
        output_path.to_path_buf()
    } else {
        let current_dir = env::current_dir().map_err(|_| "Could not access current directory")?;
        current_dir.join(output_path)
    };

    //TODO: Add proper logging
    //TODO: Add support for outputing the generated C code (CLI flag)
    println!("{}", code);

    // Write the generated code to a temporary file so we can run it through a C compiler
    let tmp_dir = TempDir::new()?;

    // Write out the runtime and std libraries and associated header files
    dino::gc_lib::write_gc_lib_files(tmp_dir.path())?;
    dino::runtime::write_runtime_files(tmp_dir.path())?;
    dino::dino_std::write_std_files(tmp_dir.path())?;

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
        "-Wpointer-arith", "-Wcast-qual", "-Wno-unused-variable", "-Wno-unused-parameter",
        "-Wno-unused-value"];
    // Run the C compiler and copy the result back
    let status = Command::new("clang")
        .current_dir(tmp_dir.path())
        .arg("-std=c99")
        // Maximum optimization level
        .arg("-O3")
        .args(warning_flags)
        .arg("-g")
        .arg(code_file_path)
        // Must link AFTER source code or else the linker will discard all the symbols
        // These must be linked in *reverse* dependency order
        .arg(format!("-l{}", dino::dino_std::DINO_STD_LIB_NAME))
        .arg(format!("-l{}", dino::runtime::RUNTIME_LIB_NAME))
        .arg(format!("-l{}", dino::gc_lib::GC_LIB_LIB_NAME))
        .arg("-lpthread")
        // Search for libraries in the current directory (the temp dir)
        .arg("-L.")
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
