//! Information about the language standard library

mod int;

include!(concat!(env!("OUT_DIR"), "/dino_std.rs"));

use std::{fs, io};
use std::path::Path;

use crate::primitives::Primitives;
use crate::package::Packages;

/// Places the language standard library in the given directory.
///
/// This files must be present in the same directory as the generated code file.
pub fn write_std_files<P: AsRef<Path>>(dir_path: P) -> io::Result<()> {
    let dir_path = dir_path.as_ref();
    fs::write(dir_path.join(DINO_STD_HEADER_FILENAME), DINO_STD_HEADER_CONTENTS)?;
    fs::write(dir_path.join(DINO_STD_LIB_FILENAME), DINO_STD_LIB_CONTENTS)?;
    Ok(())
}

pub fn define_std(packages: &mut Packages, prims: &Primitives) {
    let std_id = packages.insert("std".into());
    let std_pkg = packages.package_mut(std_id);

    int::define_module(std_pkg.root_mut(), prims);
}
