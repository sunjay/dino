use std::collections::{HashMap, HashSet};

use snafu::OptionExt;

use crate::resolve::TyId;

use super::{
    Error,
    AmbiguousType,
    constraints::{TyVar, TyVarValidTypes, TyVarEquals},
};

/// A type substitution that maps type variables to their concrete type
pub type TypeSubst = HashMap<TyVar, TyId>;

/// A map from type variables to a set of types valid for that variable
///
/// Any type variable without an entry in this map is completely unconstrained (can be any type)
pub type TyVarValidTypesMap = HashMap<TyVar, HashSet<TyId>>;

/// Collects the valid types for each type variable by processing pairs of type variables and
/// their set of valid types
///
/// Goes through and narrows the set of valid types for each type variable (TyVarOneOf). The
/// results are inserted into a map of valid types for each type variable.
///
/// Returns the unprocessed TyVarEquals constraints
pub fn collect_valid_types(ty_var_valid_types: Vec<TyVarValidTypes>) -> TyVarValidTypesMap {
    let mut valid_types = HashMap::new();

    for TyVarValidTypes {ty_var, valid_tys} in ty_var_valid_types {
        match valid_types.get_mut(&ty_var) {
            None => {
                valid_types.insert(ty_var, valid_tys);
            },
            Some(ty_var_types) => {
                *ty_var_types = valid_tys.intersection(ty_var_types).copied().collect();
            },
        }
    }

    valid_types
}

/// Apply "equality" constraints continuously until either there are no more updates to make or
/// until an infinite loop is detected.
pub fn apply_eq_constraints(
    ty_var_eq_constraints: &[TyVarEquals],
    valid_types: &mut TyVarValidTypesMap,
) {
    //TODO: Test constraint cycles: (A = B, B = C, C = A) => (A = A)
    // e.g. let x = 2; // x: C, C in {int, real}
    //      let y = x; // y: B, B = C
    //      let z = y; // z: A, A = B
    //      x = z;     //       C = A

    // This needs to be a separate pass because if we were to simply process the constrains in
    // order we may end up adding types to a set where those types would have ended up filtered
    // out by an earlier intersection operation.

    // Avoid infinite loops by stopping once we've only got unconstrained pairs of variables
    let mut stop_next_unconstrained = false;
    //TODO: Is there a more efficient way to this?
    // Continuously evaluate type variable equality constraints until none of them apply
    loop {
        // Track whether we have updated any of the sets during this pass through the constraints
        let mut updated = false;
        // Track whether we ran into an unconstrained pair of type variables
        let mut unconstrained = false;

        for &TyVarEquals(left, right) in ty_var_eq_constraints {
            // We can immediately ignore the case where the left and right are equal since we
            // know that constraint to be trivially true by the reflexive property
            if left == right {
                continue;
            }

            // Try to find the intersection between the sets of types for left and right
            let common_types = match (valid_types.get(&left), valid_types.get(&right)) {
                // Either the left or the right is completely unconstrained
                (Some(common_types), None) | (None, Some(common_types)) => {
                    common_types.clone()
                },

                // Both the left and the right is constrained, so we take the common types
                // between them
                (Some(left_types), Some(right_types)) => {
                    left_types.intersection(right_types).copied().collect()
                },

                // Neither is constrained, so we have nothing to do
                (None, None) => {
                    unconstrained = true;
                    continue;
                },
            };

            // Either there are no types for this variable yet or the types have to be updated
            if valid_types.get(&left).map(|tys| *tys != common_types).unwrap_or(true) {
                valid_types.insert(left, common_types.clone());
                updated = true;
            }
            if valid_types.get(&right).map(|tys| *tys != common_types).unwrap_or(true) {
                valid_types.insert(right, common_types);
                updated = true;
            }
        }

        if updated {
            // Found some updates, so we no longer need to stop
            stop_next_unconstrained = false;
            // Keep going in case we get an update next time
            continue;

        } else if unconstrained && !stop_next_unconstrained {
            // Try one more time to see if the next round ends up changing anything
            // After all, just because the variables were unconstrained this round, doesn't
            // mean that hasn't changed after the unconstrained pair was processed
            stop_next_unconstrained = true;
            continue;
        }
        // Stop processing. Either we are done or we have detected a potential infinite loop.
        break;
    }
}

/// Attempt to create a solution (a substitution) from the given map of variables to valid types.
///
/// The solution must contain *every* type variable in `ty_vars` to be valid.
///
/// Some type variables may not have any valid substitution (invalid types) whereas others may have
/// more than one (ambiguous). Both of these cases will result in an error being returned.
///
/// In the ambiguous case, special effort can be made to artificially "solve" the ambiguity. The
/// `resolve_ambiguity` callback is used to reduce a set of more than one type into just a single
/// type. It returns Some(...) if it was able to resolve the ambiguity and None otherwise.
pub fn build_substitution<I: Iterator<Item=TyVar>>(
    ty_var_valid_types: TyVarValidTypesMap,
    ty_vars: I,
    resolve_ambiguity: impl Fn(&HashSet<TyId>) -> Option<TyId>,
) -> Result<TypeSubst, Error> {
    // Attempt to construct a valid substitution for *every* type variable
    let mut subst = TypeSubst::default();

    for ty_var in ty_vars {
        // Any type variable not in the map is immediately ambiguous
        let valid_types = ty_var_valid_types.get(&ty_var)
            .with_context(|| AmbiguousType {})?;

        // Attempt to resolve a single type for this type variable
        let single_type = match valid_types.len() {
            0 => return Err(Error::MismatchedTypes {}),
            1 => valid_types.iter().next().copied().unwrap(),
            _ => resolve_ambiguity(valid_types).with_context(|| AmbiguousType {})?,
        };

        debug_assert!(subst.insert(ty_var, single_type).is_none(),
            "bug: overwrote type variable in substitution");
    }

    Ok(subst)
}
