use std::collections::HashMap;

use rand::{Rng, SeedableRng, rngs::SmallRng};

/// Represents a single level of local scope and maps the names of variables to their mangled
/// equivalent
pub struct NameMangler {
    rng: SmallRng,
    mangled_names: HashMap<String, String>,
    next_fresh_name: u64,
}

impl NameMangler {
    pub fn new() -> Self {
        Self {
            // Want names to be deterministic across builds
            rng: SmallRng::seed_from_u64(2194920),
            mangled_names: HashMap::new(),
            next_fresh_name: 0,
        }
    }

    /// Mangles the given name, overwriting any mangled name previously stored for the same name
    pub fn mangle_name(&mut self, name: &str) -> &str {
        // Append some random bytes to the end of the name to differentiate this name from any
        // other shadowed variables with the same name
        //TODO: Base the random characters off of the name so they aren't the same in every function
        //TODO: Make it impossible for there to be collisions (currently collisions are unlikely,
        // but still entirely possible)
        let mut mangled_name = name.to_string();
        mangled_name.reserve_exact(9);
        mangled_name.push('_');
        for _ in 0..8 {
            mangled_name.push(self.rng.gen_range(b'a', b'z') as char);
        }

        self.mangled_names.insert(name.to_string(), mangled_name);
        self.get(name)
    }

    /// Generates a fresh mangled name that is not associated with any program variable name
    pub fn fresh_mangled_name(&mut self) -> String {
        //TODO: Actually ensure that this name is unique
        let mut mangled_name = format!("var{}_", self.next_fresh_name);
        self.next_fresh_name += 1;
        mangled_name.reserve_exact(8);
        for _ in 0..8 {
            mangled_name.push(self.rng.gen_range(b'a', b'z') as char);
        }

        mangled_name
    }

    /// Returns the mangled name of the given name or panics
    pub fn get(&self, name: &str) -> &str {
        self.mangled_names.get(name).expect("bug: unresolved name was allowed to get to codegen")
    }
}
