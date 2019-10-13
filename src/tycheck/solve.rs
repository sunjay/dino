use std::collections::{HashSet, HashMap};

use snafu::OptionExt;
use maplit::hashset;
use rayon::prelude::*;

use crate::resolve::TyId;

use super::subst::TypeSubst;
use super::constraints::{TyVar, TyVarEquals};
use super::{Error, AmbiguousType};

/// Verifies that all of the given type variables have been assigned a type within the
/// given set of valid types. If a type variable has no type assigned so far (ambiguous),
/// the ambiguity is resolved using the default type given.
pub fn verify_valid_tys_or_default(
    ty_vars: &HashSet<TyVar>,
    valid_tys: &HashSet<TyId>,
    default_ty: TyId,
    subst: &mut TypeSubst,
) -> Result<(), TyId> {
    debug_assert!(valid_tys.contains(&default_ty));

    for &ty_var in ty_vars {
        match subst.get(ty_var) {
            Some(ty_id) if valid_tys.contains(&ty_id) => {},
            // Type was not one of the valid types
            Some(ty_id) => return Err(ty_id),
            // If the type was deemed ambiguous, assign it to the default (assumed) value
            None => {
                // unwrap() is safe because the type hasn't been inserted yet
                subst.insert(ty_var, default_ty).unwrap();
            },
        }
    }

    Ok(())
}

/// Applies the type equality constraints and updates the substitution accordingly
pub fn apply_eq_constraints(
    ty_var_eq_constraints: &[TyVarEquals],
    subst: &mut TypeSubst,
) -> Result<(), Error> {
    // Want to avoid any errors caused by the ordering of the equality constraints, so it's
    // best to group all the equal variables together and then apply the equality all at once

    //TODO: Test constraint cycles: (A = B, B = C, C = A) => (A = A)
    // e.g. let x = 2; // x: C, C in {int, real}
    //      let y = x; // y: B, B = C
    //      let z = y; // z: A, A = B
    //      x = z;     //       C = A

    // The unwrap() uses in the code below are safe because this ID is meant to be guaranteed as a
    // valid value in the maps below
    let mut next_group_id = 0;
    // Usually a hash map from usize -> T is actually just a Vec<T>, but in this case we want
    // fast random access and random removals, so a hash map works better
    let mut groups: HashMap<usize, HashSet<TyVar>> = HashMap::new();
    // Contains which variable is associated with which group
    let mut var_groups: HashMap<TyVar, usize> = HashMap::new();

    for TyVarEquals(left, right) in ty_var_eq_constraints {
        match (var_groups.get(left).copied(), var_groups.get(right).copied()) {
            (Some(left_group_id), Some(right_group_id)) => {
                // Merge both groups since the variables are equal and all the variables in their
                // groups are transitively equal
                let right_group = groups.remove(&right_group_id).unwrap();
                let left_group = groups.get_mut(&left_group_id).unwrap();
                // All the variables in the right group are going in the left group
                for &ty_var in &right_group {
                    var_groups.insert(ty_var, left_group_id);
                }
                // Perform the merge, consuming the right group
                left_group.extend(right_group);
            },
            (Some(left_group_id), None) => {
                // Right variable is not in a group yet
                groups.get_mut(&left_group_id).unwrap().insert(*right);
                var_groups.insert(*right, left_group_id);
            },
            (None, Some(right_group_id)) => {
                // Left variable is not in a group yet
                groups.get_mut(&right_group_id).unwrap().insert(*left);
                var_groups.insert(*left, right_group_id);
            },
            (None, None) => {
                // Neither variable is in a group, add them both to the same one
                let group_id = next_group_id;
                next_group_id += 1;

                groups.insert(group_id, hashset!{*left, *right});
                var_groups.insert(*left, group_id);
                var_groups.insert(*right, group_id);
            },
        }
    }

    // Ensure that groups are disjoint
    #[cfg(debug_assertions)]
    {
        for (gid, group) in &groups {
            for (gid2, group2) in &groups {
                if gid == gid2 { continue; }
                assert!(group.is_disjoint(group2),
                    "bug: the type variable groups had some overlap");
            }
        }
    }

    // Go through and find the common type in each group, then update the substitution to make all
    // the variables in each group equal
    for (_, group) in groups {
        // Some of the variables in the group won't have a type assigned yet in the substitution.
        // This is fine as long as at least one variable has a type assigned.
        let first_type = group.iter().find_map(|&ty_var| subst.get(ty_var));

        let common_type = match first_type {
            Some(ty_var) => ty_var,
            None => {
                // None of the variables in this group were assigned to a concrete type. That means
                // that they are all ambiguous. This will be discovered later in the constraint
                // solving process, so we just continue for now. The equality constraints were
                // still technically applied, it just turns out that there is nothing to do for
                // this particular group of variables.
                continue;
            },
        };

        for ty_var in group {
            match subst.get(ty_var) {
                // Type was already assigned to the right type
                Some(ty_id) if ty_id == common_type => {},
                // Type was not the common type
                Some(ty_id) => return Err(Error::MismatchedTypes {
                    expected: common_type,
                    actual: ty_id,
                }),
                // No substitution yet for that type, so insert the common type
                None => {
                    // unwrap() is safe because the type hasn't been inserted yet
                    subst.insert(ty_var, common_type).unwrap();
                },
            }
        }
    }

    Ok(())
}

/// Verifies that the substitution contains all of the generated type variables
///
/// Any missing type variables were underconstrained and therefore ambiguous
pub fn verify_substitution(
    subst: &TypeSubst,
    ty_vars: impl Iterator<Item = TyVar> + Send,
) -> Result<(), Error> {
    ty_vars.par_bridge().map(|ty_var| {
        subst.get(ty_var).map(|_| ()).with_context(|| AmbiguousType {})
    }).collect()
}
