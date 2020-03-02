use std::collections::HashMap;

use crate::def_id::DefId;

/// The registery of all packages in the root scope
pub type Packages = HashMap<String, Package>;

/// Represents a complete compilation unit, all of its declarations, and information about its
/// generated code
#[derive(Debug)]
pub struct Package {
    name: String,
    /// Map of module DefId to the declarations in that module
    modules: HashMap<DefId, ()>,
}
