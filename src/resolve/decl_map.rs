use std::fmt;
use std::collections::{HashSet, HashMap};
use std::borrow::Borrow;
use std::error::Error;
use std::hash::{Hash, Hasher};

use crate::ast::{Function, FuncSig, Ident, Ty};

use super::{TypeInfo, LiteralConstructors};

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
    types: Vec<TypeInfo<'a>>,
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
    ///
    /// Generally, the ty_name should be the same as `type_info.name`. That being said, sometimes
    /// it can be useful for extern types to have different values for each.
    pub fn insert_type(
        &mut self,
        ty_name: Ident<'a>,
        type_info: TypeInfo<'a>,
    ) -> Result<TyId, DuplicateDecl> {
        let id = TyId(self.types.len());
        if self.type_ids.insert(ty_name, id).is_some() {
            return Err(DuplicateDecl {
                duplicate: ty_name.to_string(),
            });
        }

        self.types.push(type_info);
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
        &self.types.get(id).unwrap().name
    }

    /// Returns true if this type is extern
    pub fn type_is_extern(&self, id: TyId) -> bool {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        self.types.get(id).unwrap().is_extern
    }

    /// Returns the literal constructors of the given type ID
    pub fn type_lit_constructors(&self, id: TyId) -> &LiteralConstructors {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        &self.types.get(id).unwrap().constructors
    }

    /// Returns the field type corresponding to the given name, if any
    pub fn field_type(&self, id: TyId, field_name: &Ident<'a>) -> Option<&Ty<'a>> {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        self.types.get(id).unwrap().fields.get(field_name)
    }

    /// Returns the method function decl corresponding to the given name, if any
    pub fn method(&self, id: TyId, method_name: &Ident<'a>) -> Option<&Function<'a>> {
        let TyId(id) = id;
        // unwrap() is safe because it should be impossible to create an invalid TyId
        self.types.get(id).unwrap().methods.get(method_name)
    }

    /// Returns the method signature corresponding to the given type and name, if any
    pub fn method_sig(&self, id: TyId, method_name: &Ident<'a>) -> Option<&FuncSig<'a>> {
        self.method(id, method_name).map(|func| &func.sig)
    }

    /// Returns the function signature corresponding to the given name, if any
    pub fn func_sig(&self, func_name: &Ident<'a>) -> Option<&FuncSig<'a>> {
        self.functions.get(func_name).map(|entry| &entry.func.sig)
    }

    /// Returns an iterator that goes through each function in the map
    pub fn types(&self) -> impl Iterator<Item=&TypeInfo<'a>> {
        self.types.iter()
    }

    /// Returns an iterator that goes through each function in the map
    pub fn functions(&self) -> impl Iterator<Item=&Function<'a>> {
        self.functions.iter().map(|ty_info| &ty_info.func)
    }
}
