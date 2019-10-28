use std::collections::HashMap;

use crate::ir;

use super::{TypeInfo, FunctionInfo, LiteralConstructors, Error};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(usize);

/// The declarations in a module, indexed by name
#[derive(Debug, Default)]
pub struct DeclMap<'a> {
    functions: HashMap<ir::Ident<'a>, FunctionInfo<'a>>,
    /// A mapping from type ID (index) to the type info.
    /// This is None for each user defined type during the first pass of name resolution.
    /// Lookups may rely on this being Some(TypeInfo).
    types: Vec<Option<TypeInfo<'a>>>,
    type_ids: HashMap<ir::Ident<'a>, TyId>,
}

impl<'a> DeclMap<'a> {
    /// Reserves a type ID for the given type name without inserting any type info for it
    pub fn reserve_type(&mut self, ty_name: ir::Ident<'a>) -> Result<TyId, Error> {
        if let Some(_) = self.type_id(&ty_name) {
            return Err(Error::DuplicateDecl {
                duplicate: ty_name.to_string(),
            });
        }

        let id = TyId(self.types.len());
        self.types.push(None);
        Ok(id)
    }

    /// Inserts a new type and returns its type ID
    ///
    /// Generally, the ty_name should be the same as `type_info.name`. That being said, sometimes
    /// it can be useful for extern types to have different values for each.
    pub fn insert_type(
        &mut self,
        ty_name: ir::Ident<'a>,
        type_info: TypeInfo<'a>,
    ) -> Result<TyId, Error> {
        let id = match self.type_id(&ty_name) {
            // Type was probably previously reserved
            Some(id) => id,
            None => TyId(self.types.len()),
        };

        // It's an error to overwrite type info that was already previously present
        // However, if the type was reserved (i.e. self.types[id.0] == None), this is fine.
        if self.type_ids.insert(ty_name, id).is_some() && self.types[id.0].is_some() {
            return Err(Error::DuplicateDecl {
                duplicate: ty_name.to_string(),
            });
        }

        let TyId(idx) = id;
        if idx == self.types.len() {
            self.types.push(Some(type_info));
        } else if idx < self.types.len() {
            self.types[idx] = Some(type_info);
        } else { // idx > self.types.len()
            unreachable!();
        }
        Ok(id)
    }

    /// Inserts a new function declaration
    pub fn insert_func(&mut self, func_info: FunctionInfo<'a>) -> Result<(), Error> {
        let func_name = func_info.name;
        if self.functions.insert(func_name, func_info).is_some() {
            return Err(Error::DuplicateDecl {
                duplicate: func_name.to_string(),
            });
        }

        Ok(())
    }

    /// Inserts a new method for the given type
    pub fn insert_method(
        &mut self,
        id: TyId,
        method_name: ir::Ident<'a>,
        method_info: FunctionInfo<'a>,
    ) -> Result<(), Error> {
        let methods = &mut self.type_info_mut(id).methods;

        if methods.insert(method_name, method_info).is_some() {
            return Err(Error::DuplicateDecl {
                duplicate: method_name.to_string(),
            });
        }

        Ok(())
    }

    /// Returns the ID of the given type name
    pub fn type_id(&self, ty: &ir::Ident<'a>) -> Option<TyId> {
        self.type_ids.get(ty).copied()
    }

    /// Returns the name of the given type ID
    pub fn type_name(&self, id: TyId) -> &ir::Ident {
        &self.type_info(id).name
    }

    /// Returns true if this type is extern
    pub fn type_is_extern(&self, id: TyId) -> bool {
        self.type_info(id).is_extern
    }

    /// Returns the literal constructors of the given type ID
    pub fn type_lit_constructors(&self, id: TyId) -> &LiteralConstructors {
        &self.type_info(id).constructors
    }

    /// Returns the field type corresponding to the given name, if any
    pub fn field_type(&self, ty_id: TyId, field_name: &ir::Ident<'a>) -> Option<TyId> {
        self.type_info(ty_id).fields.get(field_name).copied()
    }

    /// Returns the method function decl corresponding to the given name, if any
    pub fn method(&self, id: TyId, method_name: &ir::Ident<'a>) -> Option<&FunctionInfo<'a>> {
        self.type_info(id).methods.get(method_name)
    }

    /// Returns the method signature corresponding to the given type and name, if any
    pub fn method_sig(&self, id: TyId, method_name: &ir::Ident<'a>) -> Option<&ir::FuncSig<'a>> {
        self.method(id, method_name).map(|func| &func.sig)
    }

    /// Returns the function signature corresponding to the given name, if any
    pub fn func_sig(&self, func_name: &ir::Ident<'a>) -> Option<&ir::FuncSig<'a>> {
        self.functions.get(func_name).map(|entry| &entry.sig)
    }

    /// Gets the type info for the given ID
    fn type_info(&self, id: TyId) -> &TypeInfo<'a> {
        let TyId(id) = id;
        self.types.get(id).unwrap().as_ref()
            .expect("bug: not all reserved type IDs were initialized to type info")
    }

    /// Gets the type info for the given ID
    fn type_info_mut(&mut self, id: TyId) -> &mut TypeInfo<'a> {
        let TyId(id) = id;
        self.types.get_mut(id).unwrap().as_mut()
            .expect("bug: not all reserved type IDs were initialized with type info")
    }
}
