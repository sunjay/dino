use std::fmt;
use std::collections::{HashSet, HashMap};
use std::borrow::Borrow;
use std::error::Error;
use std::hash::{Hash, Hasher};

use crate::ast::{Function, FuncSig, Ident, IdentPath};

use super::ExternType;

// Allows functions to be looked up by name without requiring us to use a HashMap and duplicating
// the name in the key.
#[derive(Debug)]
struct FunctionEntry<'a> {
    func: Function<'a>,
}

impl<'a> Hash for FunctionEntry<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.name.hash(state)
    }
}

impl<'a> Borrow<Ident<'a>> for FunctionEntry<'a> {
    fn borrow(&self) -> &Ident<'a> {
        &self.func.name
    }
}

// Don't want all of Function to need to implement PartialEq
impl<'a> PartialEq for FunctionEntry<'a> {
    fn eq(&self, other: &FunctionEntry<'a>) -> bool {
        self.func.name == other.func.name
    }
}

impl<'a> PartialEq<str> for FunctionEntry<'a> {
    fn eq(&self, other: &str) -> bool {
        self.func.name == other
    }
}

impl<'a> Eq for FunctionEntry<'a> {}

/// Stores information about a type
#[derive(Debug)]
struct TypeEntry<'a> {
    /// The name of the type to be used in the AST, etc.
    ty_name: Ident<'a>,
    /// Information about code generation for this type
    //TODO: Not all types will have an extern name once we support structs/enums
    extern_type: ExternType,
    /// The methods provided by this type.
    ///
    /// The keys of the map are the method names (e.g. `add`) whereas the `name` field of
    /// the `Function` can be anything. If the Function is extern, this `name` field will
    /// be used in the generated code.
    methods: HashMap<Ident<'a>, Function<'a>>,
}

#[derive(Debug)]
pub struct DuplicateDecl {
    duplicate: String,
}

impl fmt::Display for DuplicateDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the name '{}' is defined multiple times", self.duplicate)
    }
}

impl Error for DuplicateDecl {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(usize);

/// The declarations in a module, indexed by name
#[derive(Debug, Default)]
pub struct DeclMap<'a> {
    functions: HashSet<FunctionEntry<'a>>,
    types: Vec<TypeEntry<'a>>,
    type_ids: HashMap<Ident<'a>, TyId>,
}

impl<'a> DeclMap<'a> {
    /// Inserts a new function declaration
    pub fn insert_func(&mut self, func: Function<'a>) -> Result<(), DuplicateDecl> {
        let name = func.name;
        let func_entry = FunctionEntry {func};

        //TODO: Disallow duplicate parameter names (either here or somewhere else in the code)
        if !self.functions.insert(func_entry) {
            return Err(DuplicateDecl {
                duplicate: name.to_string(),
            });
        }

        Ok(())
    }

    /// Inserts a new type and returns its type ID
    pub fn insert_type(
        &mut self,
        ty_name: Ident<'a>,
        extern_type: ExternType,
    ) -> Result<TyId, DuplicateDecl> {
        let id = TyId(self.types.len());
        if self.type_ids.insert(ty_name, id).is_some() {
            return Err(DuplicateDecl {
                duplicate: ty_name.to_string(),
            });
        }

        self.types.push(TypeEntry {ty_name, extern_type, methods: HashMap::new()});
        Ok(id)
    }

    /// Inserts a new method for the given type
    pub fn insert_method(
        &mut self,
        id: TyId,
        method_name: Ident<'a>,
        method: Function<'a>,
    ) -> Result<(), DuplicateDecl> {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        let methods = &mut self.types.get_mut(id).unwrap().methods;

        //TODO: Disallow duplicate parameter names (either here or somewhere else in the code)
        if methods.insert(method_name, method).is_some() {
            return Err(DuplicateDecl {
                duplicate: method_name.to_string(),
            });
        }

        Ok(())
    }

    /// Returns the ID of the given type name
    pub fn type_id(&self, ty: &Ident<'a>) -> Option<TyId> {
        self.type_ids.get(ty).copied()
    }

    /// Returns the name of the given type ID
    pub fn type_name(&self, id: TyId) -> &Ident<'a> {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        &self.types.get(id).unwrap().ty_name
    }

    /// Returns the extern type information of the given type ID
    pub fn type_extern_info(&self, id: TyId) -> &ExternType {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        &self.types.get(id).unwrap().extern_type
    }

    /// Returns the method function decl corresponding to the given name, if any
    pub fn method(&self, id: TyId, name: &IdentPath<'a>) -> Option<&Function<'a>> {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        unimplemented!()
        //self.types.get(id).unwrap().methods.get(name)
    }

    /// Returns the function signature corresponding to the given name, if any
    pub fn func_sig(&self, name: &IdentPath<'a>) -> Option<&FuncSig<'a>> {
        unimplemented!()
        //self.functions.get(name).map(|entry| &entry.func.sig)
    }

    /// Returns an iterator that goes through each declaration in the map
    pub fn functions(&self) -> impl Iterator<Item=&Function<'a>> {
        self.functions.iter().map(|entry| &entry.func)
    }
}
