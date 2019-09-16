use std::fs;
use std::error::Error;

use build_helper::{profile, cargo, windows, unix, out_dir};

const RUNTIME_LIB_MOD_SOURCE_CODE: &str = r#"
pub const RUNTIME_HEADER_FILENAME: &str = "{runtime_header_filename}";
pub const RUNTIME_HEADER_CONTENTS: &[u8] = include_bytes!("{runtime_header_path}");

pub const RUNTIME_LIB_NAME: &str = "{runtime_lib_name}";
pub const RUNTIME_LIB_FILENAME: &str = "{runtime_lib_filename}";
pub const RUNTIME_LIB_CONTENTS: &[u8] = include_bytes!("{runtime_lib_path}");
"#;

fn main() -> Result<(), Box<dyn Error>> {
    let manifest_path = cargo::manifest::dir();

    // Requires that the disco-runtime crate already be built
    let runtime_header_filename = "disco-runtime.h";
    let runtime_header_path = manifest_path.join("disco-runtime").join(runtime_header_filename);

    let runtime_lib_name = "disco_runtime";
    let runtime_lib_filename = if windows() {
        "disco_runtime.lib"
    } else if unix() {
        "libdisco_runtime.a"
    } else {
        panic!("Unsupported platform. Unable to determine runtime library filename.");
    };

    let runtime_lib_path = manifest_path
        .join("disco-runtime")
        .join("target")
        .join(profile().to_string())
        .join(&runtime_lib_filename);

    if !runtime_lib_path.exists() {
        panic!("Runtime library has not been generated yet. Run `cargo build --manifest-path disco-runtime/Cargo.toml`");
    }

    // Generate a module that embeds the entire runtime
    let code = RUNTIME_LIB_MOD_SOURCE_CODE
        .replace("{runtime_header_filename}", runtime_header_filename)
        .replace("{runtime_header_path}", runtime_header_path.to_str().unwrap())
        .replace("{runtime_lib_name}", runtime_lib_name)
        .replace("{runtime_lib_filename}", runtime_lib_filename)
        .replace("{runtime_lib_path}", runtime_lib_path.to_str().unwrap());

    fs::write(out_dir().join("runtime.rs"), code)?;

    Ok(())
}
