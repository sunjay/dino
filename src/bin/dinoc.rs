use std::env;
use std::io::Write;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::process::{self, Command};
use std::str::FromStr;

use tempfile::TempDir;
use structopt::StructOpt;
use termcolor::ColorChoice;
use parking_lot::RwLock;

use dino::{
    cprintln,
    cwriteln,
    source_files::SourceFiles,
    diagnostics::Diagnostics,
    parser,
    desugar::Desugar,
    cgenir::{self, GenerateC},
    cir::CSymbols,
};

/// A command line argument that configures the coloring of the output
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ColorArg(pub ColorChoice);

impl Default for ColorArg {
    fn default() -> Self {
        ColorArg(ColorChoice::Auto)
    }
}

impl ColorArg {
    /// Allowed values the argument
    pub const VARIANTS: &'static [&'static str] = &["auto", "always", "ansi", "never"];
}

impl FromStr for ColorArg {
    type Err = &'static str;

    fn from_str(src: &str) -> Result<ColorArg, &'static str> {
        match src {
            _ if src.eq_ignore_ascii_case("auto") => Ok(ColorArg(ColorChoice::Auto)),
            _ if src.eq_ignore_ascii_case("always") => Ok(ColorArg(ColorChoice::Always)),
            _ if src.eq_ignore_ascii_case("ansi") => Ok(ColorArg(ColorChoice::AlwaysAnsi)),
            _ if src.eq_ignore_ascii_case("never") => Ok(ColorArg(ColorChoice::Never)),
            _ => Err("valid values: auto, always, ansi, never"),
        }
    }
}

impl Into<ColorChoice> for ColorArg {
    fn into(self) -> ColorChoice {
        self.0
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "dino", about)]
struct CompilerOptions {
    /// The program to compile
    #[structopt(name = "input", parse(from_os_str))]
    program_path: PathBuf,
    /// Write output to <file>
    #[structopt(short = "o", name = "file")]
    output_path: Option<PathBuf>,
    /// Configure coloring of output
    #[structopt(long = "color", parse(try_from_str), default_value = "auto",
        possible_values = ColorArg::VARIANTS, case_insensitive = true)]
    pub color: ColorArg,
}

macro_rules! quit {
    ($diag:expr, $($args:tt)*) => {
        {
            $diag.error(format!($($args)*)).emit();
            process::exit(1);
        }
    };
}

macro_rules! check_errors {
    ($diag:expr) => {
        let diag = $diag;
        match diag.emitted_errors() {
            0 => {},
            1 => quit!(diag, "aborting due to 1 previous error"),
            errors => quit!(diag, "aborting due to {} previous errors", errors),
        }
    };
}

fn main() {
    let CompilerOptions {program_path, output_path, color} = CompilerOptions::from_args();

    let source_files = Arc::new(RwLock::new(SourceFiles::default()));
    let diag = Diagnostics::new(source_files.clone(), color.into());

    // Check that the path and stem are valid
    let program_stem = match (program_path.file_stem(), program_path.extension()) {
        (Some(stem), Some(ext)) if !stem.is_empty() && ext == "dino" => stem,
        _ => quit!(&diag, "Invalid input path. Must use extension `dino`"),
    };

    // Default output path is the input path without its stem
    let output_path = output_path.as_ref().map(|p| p.as_path())
        .unwrap_or_else(|| Path::new(program_stem));
    // Append the current directory to the output path if necessary
    let output_path = if output_path.is_absolute() {
        output_path.to_path_buf()
    } else {
        let current_dir = env::current_dir()
            .unwrap_or_else(|err| quit!(&diag, "Could not access current directory: {}", err));
        current_dir.join(output_path)
    };

    let root_file = source_files.write().add_file(&program_path)
        .unwrap_or_else(|err| quit!(&diag, "Could not read source file `{}`: {}", program_path.display(), err));
    let program = {
        // New scope because we want to drop this lock guard as soon as possible
        let files = source_files.read();
        parser::parse_module(files.source(root_file), &diag)
    };
    check_errors!(&diag);
    let desugared_program = program.desugar(&diag);
    check_errors!(&diag);

    let csyms = CSymbols::default();

    //TODO: Replace this with actual code generation once the compiler stages are implemented
    let cgen_program = cgenir::Program {
        structs: Vec::new(),
        functions: Vec::new(),
        entry_point: Some(Vec::new()),
    };
    let cprogram = cgen_program.to_c();

    //TODO: Add proper logging
    //TODO: Add support for outputing the generated C code (CLI flag)
    cprintln!(&csyms, "{}", cprogram);

    // Write the generated code to a temporary file so we can run it through a C compiler
    let tmp_dir = TempDir::new()
        .unwrap_or_else(|err| quit!(&diag, "Unable to create temporary directory: {}", err));

    // Write out the runtime and std libraries and associated header files
    dino::gc_lib::write_gc_lib_files(tmp_dir.path())
        .unwrap_or_else(|err| quit!(&diag, "Unable to write GC runtime: {}", err));
    dino::runtime::write_runtime_files(tmp_dir.path())
        .unwrap_or_else(|err| quit!(&diag, "Unable to write dino runtime: {}", err));
    dino::dino_std::write_std_files(tmp_dir.path())
        .unwrap_or_else(|err| quit!(&diag, "Unable to write std library: {}", err));

    let code_file_path = tmp_dir.path().join("main.c");
    // Drop the file as soon as possible so it finishes being written to
    // and so it is closed when we close the temporary directory
    {
        let mut code_file = File::create(&code_file_path)
            .unwrap_or_else(|err| quit!(&diag, "Unable to create file `{}` for generated code: {}", code_file_path.display(), err));
        cwriteln!(code_file, &csyms, "{}", cprogram)
            .unwrap_or_else(|err| quit!(&diag, "Unable to write generated code: {}", err));
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
        .status()
        .unwrap_or_else(|err| quit!(&diag, "Failed to run clang: {}", err));

    if !status.success() {
        panic!("bug: code generation failed");
    }

    // By closing the `TempDir` explicitly we can check that it has been deleted successfully. If
    // we don't close it explicitly, the directory will still be deleted when `tmp_dir` goes out of
    // scope, but we won't know whether deleting the directory succeeded.
    // IMPORTANT: All handles to files in this directory must be closed at this point.
    tmp_dir.close()
        .unwrap_or_else(|err| quit!(&diag, "Failed remove temporary directory: {}", err));
}
