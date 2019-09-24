use std::error::Error;

use build_helper::{cargo::manifest, profile};

fn main() -> Result<(), Box<dyn Error>> {
    let crate_dir = manifest::dir();

    let header_path = crate_dir
        .join("target")
        .join(profile().to_string())
        .join("dino-std.h");

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()?
        .write_to_file(header_path);

    Ok(())
}
