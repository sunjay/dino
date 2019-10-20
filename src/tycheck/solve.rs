use std::collections::HashSet;

use snafu::OptionExt;
use rayon::prelude::*;
use ena::unify::InPlaceUnificationTable;

use crate::resolve::TyId;

use super::subst::TypeSubst;
use super::constraints::TyVar;
use super::{Error, AmbiguousType};

/// Builds the substitution and verifies that it contains all of the generated type variables
///
/// Any missing type variables were underconstrained and therefore ambiguous
pub fn build_substitution(
    // The TyVars provided by the iterator are assumed to be unique
    ty_vars: impl Iterator<Item = (TyVar, Option<TyId>)> + Send,
) -> Result<TypeSubst, Error> {
    ty_vars.par_bridge().map(|(ty_var, ty)| {
        Ok((ty_var, ty.with_context(|| AmbiguousType {})?))
    }).collect()
}

/// Verifies that all of the given type variables have been assigned a type within the
/// given set of valid types. If a type variable has no type assigned so far (ambiguous),
/// the ambiguity is resolved using the default type given.
pub fn verify_valid_tys_or_default(
    ty_vars: &HashSet<TyVar>,
    valid_tys: &HashSet<TyId>,
    default_ty: TyId,
    subst: &mut InPlaceUnificationTable<TyVar>,
) -> Result<(), TyId> {
    debug_assert!(valid_tys.contains(&default_ty));

    for &ty_var in ty_vars {
        match subst.probe_value(ty_var) {
            Some(ty_id) if valid_tys.contains(&ty_id) => {},
            // Type was not one of the valid types
            Some(ty_id) => return Err(ty_id),
            // If the type was deemed ambiguous, assign it to the default (assumed) value
            None => {
                // unwrap() is safe because the type hasn't been inserted yet
                subst.unify_var_value(ty_var, Some(default_ty)).unwrap();
            },
        }
    }

    Ok(())
}
