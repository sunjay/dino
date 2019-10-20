use std::collections::HashMap;
use std::iter::FromIterator;

use rayon::prelude::*;

use crate::resolve::TyId;

use super::constraints::TyVar;

/// A type substitution that maps type variables to their concrete type
///
/// Each type variable can only be assigned to a single type
#[derive(Debug, Default)]
pub struct TypeSubst {
    assignments: HashMap<TyVar, TyId>,
}

impl FromIterator<(TyVar, TyId)> for TypeSubst {
    fn from_iter<T: IntoIterator<Item = (TyVar, TyId)>>(iter: T) -> Self {
        Self {
            assignments: iter.into_iter().collect(),
        }
    }
}

impl FromParallelIterator<(TyVar, TyId)> for TypeSubst {
    fn from_par_iter<I: IntoParallelIterator<Item = (TyVar, TyId)>>(par_iter: I) -> Self {
        Self {
            assignments: par_iter.into_par_iter().collect(),
        }
    }
}

impl TypeSubst {
    /// Returns the type ID associated with the given type variable, if any
    pub fn get(&self, ty_var: TyVar) -> Option<TyId> {
        self.assignments.get(&ty_var).copied()
    }
}
