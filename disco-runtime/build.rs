use std::env;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let crate_dir = env::var("CARGO_MANIFEST_DIR")?;

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()?
        .write_to_file("disco-runtime.h");

    Ok(())
}
