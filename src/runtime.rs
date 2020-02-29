//! Information about the language runtime

include!(concat!(env!("OUT_DIR"), "/runtime.rs"));

use std::{fs, io};
use std::path::Path;

/// The name of the allocation function that takes a size and returns a pointer
pub const ALLOCATE: &str = "__dino__alloc";

/// Places the language runtime in the given directory.
///
/// This files must be present in the same directory as the generated code file.
pub fn write_runtime_files<P: AsRef<Path>>(dir_path: P) -> io::Result<()> {
    let dir_path = dir_path.as_ref();
    fs::write(dir_path.join(RUNTIME_HEADER_FILENAME), RUNTIME_HEADER_CONTENTS)?;
    fs::write(dir_path.join(RUNTIME_LIB_FILENAME), RUNTIME_LIB_CONTENTS)?;
    Ok(())
}
