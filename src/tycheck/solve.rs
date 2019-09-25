use std::collections::{HashMap, HashSet};

use crate::resolve::TyId;

use super::{Error, constraints::{TyVar, Constraint}};

/// Represents the fact that a substitution may directly map to a type or otherwise redirect to
/// another type variable
#[derive(Debug)]
enum Subst {
    Ty(TyId),
    Var(TyVar),
}

/// A type substitution that maps type variables to their concrete type
#[derive(Debug, Default)]
pub struct TypeSubst {
    subst: HashMap<TyVar, Subst>,
}

impl TypeSubst {
    /// Attempt to solve a constraint set and return the solution as a substitution. The solution
    /// must contain *every* type variable in ty_vars to be valid.
    pub fn solve<I: Iterator<Item=TyVar>>(
        constraints: Vec<Constraint>,
        ty_vars: I,
    ) -> Result<Self, Error> {
        //TODO: Test constraint cycles: (A = B, B = C, C = A) => (A = A)
        // e.g. let x = 2; // x: C, C in {int, real}
        //      let y = x; // y: B, B = C
        //      let z = y; // z: A, A = B
        //      x = z;     //       C = A

        // A map from type variable to the set of possible types it can be
        //
        // Any type variable without an entry in this map is completely unconstrained
        let mut possible_types = HashMap::new();

        // This needs to be a separate pass because if we were to simply process the constrains in
        // order we may end up adding types to a set where those types would have ended up filtered
        // out by an earlier intersection operation.
        let ty_var_eq_constraints = collect_possible_types(constraints, &mut possible_types);

        // Avoid infinite loops by stopping once we've only got unconstrained pairs of variables
        let mut stop_next_unconstrained = false;
        //TODO: Is there a more efficient way to this?
        // Continuously evaluate type variable equality constraints until none of them apply
        loop {
            // Track whether we have updated any of the sets during this pass through the constraints
            let mut updated = false;
            // Track whether we ran into an unconstrained pair of type variables
            let mut unconstrained = false;

            for &(left, right) in &ty_var_eq_constraints {
                let left_types = possible_types.get(&left);
                let right_types = possible_types.get(&right);
                // Since left and right are supposed to be equal, their sets of valid types must
                // intersect
                let common_types = match (left_types, right_types) {
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
                if left_types.map(|tys| *tys != common_types).unwrap_or(true) {
                    possible_types.insert(left, common_types.clone());
                    updated = true;
                }
                if right_types.map(|tys| *tys != common_types).unwrap_or(true) {
                    possible_types.insert(right, common_types);
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

        // Attempt to construct a valid substitution for *every* type variable
        let subst = TypeSubst::default();

        for ty_var in ty_vars {
            //TODO
        }

        Ok(subst)
    }
}

/// Goes through and narrows the set of valid types for each type variable (TyVarOneOf). The
/// results are inserted into a map of possible types for each type variable.
///
/// Returns the unprocessed TyVarEquals constraints
fn collect_possible_types(
    constraints: Vec<Constraint>,
    possible_types: &mut HashMap<TyVar, HashSet<TyId>>,
) -> Vec<(TyVar, TyVar)> {
    constraints.into_iter().filter_map(|constraint| {
        match constraint {
            Constraint::TyVarOneOf {ty_var, valid_tys} => {
                match possible_types.get_mut(&ty_var) {
                    None => {
                        //TODO: This clone can be removed if we can take the constraints by value
                        possible_types.insert(ty_var, valid_tys.clone());
                    },
                    Some(ty_var_types) => {
                        *ty_var_types = valid_tys.intersection(ty_var_types).copied().collect();
                    },
                }

                None
            },

            // We can immediately ignore the case where the left and right are equal since we
            // know that constraint to be trivially true by the reflexive property
            Constraint::TyVarEquals(left, right) => if left != right {
                Some((left, right))
            } else { None },
        }
    }).collect()
}
