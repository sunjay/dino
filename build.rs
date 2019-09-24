use std::fmt;
use std::io::Write;
use std::fs::File;
use std::error::Error;
use std::path::Path;

use build_helper::{profile, cargo, windows, unix, out_dir};

/// A Rust module that represents a static library
///
/// The generated module will place the entire library and its header into a static in the code.
struct StaticLibraryModule<'a> {
    /// The uppercase prefix of all generated constants
    prefix: &'a str,
    /// The C header filename (.h)
    header_filename: &'a str,
    /// The full path to the header file
    header_path: &'a Path,
    /// The static library name to be provided to the C compiler
    lib_name: &'a str,
    /// The filename of the static library (platform dependent)
    lib_filename: &'a str,
    /// The full path to the static library file
    lib_path: &'a Path,
}

impl<'a> fmt::Display for StaticLibraryModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {prefix, header_filename, header_path, lib_name, lib_filename, lib_path} = self;

        writeln!(f, r#"pub static {prefix}_HEADER_FILENAME: &str = "{header_filename}";"#,
            prefix = prefix, header_filename = header_filename)?;
        writeln!(f, r#"pub static {prefix}_HEADER_CONTENTS: &[u8] = include_bytes!("{header_path}");"#,
            prefix = prefix, header_path = header_path.display())?;
        writeln!(f)?;

        writeln!(f, r#"pub static {prefix}_LIB_NAME: &str = "{lib_name}";"#,
            prefix = prefix, lib_name = lib_name)?;
        writeln!(f, r#"pub static {prefix}_LIB_FILENAME: &str = "{lib_filename}";"#,
            prefix = prefix, lib_filename = lib_filename)?;
        writeln!(f, r#"pub static {prefix}_LIB_CONTENTS: &[u8] = include_bytes!("{lib_path}");"#,
            prefix = prefix, lib_path = lib_path.display())?;

        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let manifest_path = cargo::manifest::dir();
    // Requires that the dino-runtime crate already be built
    generate_static_lib_mod(
        &manifest_path,
        "dino-runtime",
        "dino-runtime.h",
        "dino_runtime",
        "RUNTIME",
        "runtime.rs",
    )?;
    generate_static_lib_mod(
        &manifest_path,
        "dino-std",
        "dino-std.h",
        "dino_std",
        "DINO_STD",
        "dino_std.rs",
    )?;

    Ok(())
}

fn generate_static_lib_mod(
    manifest_path: &Path,
    project_dir: &str,
    header_filename: &str,
    lib_name: &str,
    prefix: &str,
    module_filename: &str,
) -> Result<(), Box<dyn Error>> {
    let profile = profile().to_string();
    let header_path = &manifest_path
        .join(project_dir)
        .join("target")
        .join(&profile)
        .join(header_filename);
    if !header_path.exists() {
        panic!("{} header has not been generated yet. Run `cargo build --manifest-path {}/Cargo.toml`", header_filename, project_dir);
    }

    let lib_filename = &if windows() {
        format!("{}.lib", lib_name)
    } else if unix() {
        format!("lib{}.a", lib_name)
    } else {
        panic!("Unsupported platform. Unable to determine {} library filename.", lib_name);
    };

    let lib_path = &manifest_path
        .join(project_dir)
        .join("target")
        .join(&profile)
        .join(&lib_filename);

    if !lib_path.exists() {
        panic!("{} library has not been generated yet. Run `cargo build --manifest-path {}/Cargo.toml`", lib_name, project_dir);
    }

    let module = StaticLibraryModule {
        prefix,
        header_filename,
        header_path,
        lib_name,
        lib_filename,
        lib_path,
    };

    let mut file = File::create(out_dir().join(module_filename))?;
    writeln!(file, "{}", module)?;

    Ok(())
}
