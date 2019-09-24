use std::collections::{HashSet, HashMap};

use snafu::Snafu;

use crate::resolve::{DeclMap, Primitives, TyId};
use crate::ast::Function;

use super::tyir;

/// A type substitution that maps type variables to their concrete type
pub type TypeSubst = HashMap<TyVar, TyId>;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

/// A constraint that dictates the set of types that are valid for a given type variable
#[derive(Debug)]
pub struct Constraint {
    /// The type variable being constrained
    pub ty_var: TyVar,
    /// The set of types valid for this variable
    pub valid_tys: HashSet<TyId>,
}

#[derive(Debug)]
pub struct ConstraintSet {
    constraints: Vec<Constraint>,
    next_var: usize,
}

impl ConstraintSet {
    /// Generates a constraint set for the given function declaration. Any fresh type variables
    /// created are annotated inline into the returned `tyir::Function`
    pub fn generate<'a>(
        func: &'a Function<'a>,
        mod_decls: &DeclMap,
        prims: &Primitives,
    ) -> (Self, tyir::Function<'a>) {
        unimplemented!()
    }

    /// Attempts to solve the constraint set and return the solution as a substitution map
    pub fn solve(self) -> Result<TypeSubst, Error> {
        let subst = HashMap::new();
        //TODO
        Ok(subst)
    }
}
