use std::collections::HashMap;

use crate::ast;

use super::constraints::TyVar;

/// A map of variable names to their type variables
type LocalScope<'a> = HashMap<ast::Ident<'a>, TyVar>;

/// A linked list of scopes, used to create an ad-hoc stack where only the "top" is mutable
///
/// `'a` is the lifetime of the things stored in the scope. `'s` is the lifetime of the parent scope.
#[derive(Debug, Default)]
pub struct Scope<'a, 's> {
    /// The current scope, it can be modified freely
    pub current: LocalScope<'a>,
    /// The next upper level of scope (if any)
    pub parent: Option<&'s Scope<'a, 's>>,
}

impl<'a, 's> Scope<'a, 's> {
    /// Create a new mutable child scope
    pub fn child_scope(&'s self) -> Self {
        Self {
            current: LocalScope::default(),
            parent: Some(self),
        }
    }

    /// Returns true if the given variable name is in scope at the current level, or in any parent
    /// scope.
    pub fn contains(&self, name: &ast::Ident<'a>) -> bool {
        self.get(name).is_some()
    }

    /// Returns the type variable associated with a given variable name
    pub fn get(&self, name: ast::Ident<'a>) -> Option<TyVar> {
        self.current.get(name).copied().or_else(|| match self.parent {
            Some(parent) => parent.get(name),
            None => None,
        })
    }

    /// Adds a new variable *only* to the current scope and associates it with the given type
    /// variable
    pub fn add_variable(&mut self, name: ast::Ident<'a>, ty_var: TyVar) {
        self.current.insert(name, ty_var);
    }
}
