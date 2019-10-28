//! Information about the Garbage Collection (GC) library
//!
//! See instructions in the README for how to setup the necessary dependency.

use std::{fs, io};
use std::path::Path;

pub static GC_LIB_HEADER_FILENAME: &str = "gc.h";
pub static GC_LIB_HEADER_CONTENTS: &[u8] = include_bytes!("../bdwgc/include/gc.h");
pub static GC_LIB_HEADER_2_FILENAME: &str = "gc_version.h";
pub static GC_LIB_HEADER_2_CONTENTS: &[u8] = include_bytes!("../bdwgc/include/gc_version.h");
pub static GC_LIB_HEADER_3_FILENAME: &str = "gc_config_macros.h";
pub static GC_LIB_HEADER_3_CONTENTS: &[u8] = include_bytes!("../bdwgc/include/gc_config_macros.h");

pub static GC_LIB_LIB_NAME: &str = "gc";
pub static GC_LIB_LIB_FILENAME: &str = "libgc.a";
pub static GC_LIB_LIB_CONTENTS: &[u8] = include_bytes!("../bdwgc/.libs/libgc.a");

/// Places the garbage collection library in the given directory.
///
/// This files must be present in the same directory as the generated code file.
pub fn write_gc_lib_files<P: AsRef<Path>>(dir_path: P) -> io::Result<()> {
    let dir_path = dir_path.as_ref();
    fs::write(dir_path.join(GC_LIB_HEADER_FILENAME), GC_LIB_HEADER_CONTENTS)?;
    fs::write(dir_path.join(GC_LIB_HEADER_2_FILENAME), GC_LIB_HEADER_2_CONTENTS)?;
    fs::write(dir_path.join(GC_LIB_HEADER_3_FILENAME), GC_LIB_HEADER_3_CONTENTS)?;
    fs::write(dir_path.join(GC_LIB_LIB_FILENAME), GC_LIB_LIB_CONTENTS)?;
    Ok(())
}
