use std::collections::HashMap;

use crate::resolve::TyId;

use super::constraints::TyVar;

#[derive(Debug)]
pub struct TypeMismatch {
    /// The variable that experienced the mismatch
    pub ty_var: TyVar,
    /// The current type ID that was already stored for this variable
    pub expected: TyId,
    /// The type ID that we attempted to overwrite the current type with
    pub actual: TyId,
}

/// A type substitution that maps type variables to their concrete type
#[derive(Debug, Default)]
pub struct TypeSubst {
    assignments: HashMap<TyVar, TyId>,
}

impl TypeSubst {
    /// Inserts the given type into the substitution, returning an error if the variable
    /// had already been inserted with a different type ID
    pub fn insert(&mut self, ty_var: TyVar, ty: TyId) -> Result<(), TypeMismatch> {
        match self.get(ty_var) {
            Some(current) if current == ty => Ok(()),
            Some(current) => Err(TypeMismatch {ty_var, expected: current, actual: ty}),
            None => {
                self.assignments.insert(ty_var, ty);
                Ok(())
            },
        }
    }

    /// Returns the type ID associated with the given type variable, if any
    pub fn get(&self, ty_var: TyVar) -> Option<TyId> {
        self.assignments.get(&ty_var).copied()
    }
}
