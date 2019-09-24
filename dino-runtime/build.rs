use std::error::Error;

use build_helper::cargo::manifest;

fn main() -> Result<(), Box<dyn Error>> {
    let crate_dir = manifest::dir();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()?
        .write_to_file("dino-runtime.h");

    Ok(())
}
