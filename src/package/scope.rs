use std::sync::Arc;
use std::collections::HashMap;

use parking_lot::RwLock;

use super::{PkgId, DefId};

pub type ScopeTreeSync = Arc<RwLock<ScopeTree>>;

/// An ID for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId {
    pkg: PkgId,
    scope_index: usize,
}

#[derive(Debug)]
pub enum ScopeKind {
    Module {
        /// A submodule is simply a subscope
        modules: HashMap<Arc<str>, ScopeId>,
        /// A list of scopes from which any name may be used
        wildcard_imports: Vec<ScopeId>,
        /// Names visible in a type context
        types: HashMap<Arc<str>, DefId>,
        /// Names visible in a calling context and an expression context
        functions: HashMap<Arc<str>, DefId>,
    },

    Impl {
        /// Names visible in a type context
        ///
        /// Any type names (i.e. from generics) introduced by the impl
        types: HashMap<Arc<str>, DefId>,
        /// The `Self` type which this impl applies to
        self_ty: DefId,
    },

    Function {
        /// Names visible in a type context
        ///
        /// Any type names (i.e. from generics) introduced by the function signature
        types: HashMap<Arc<str>, DefId>,
        /// Any variables (i.e from parameters) introduced by the function signature
        ///
        /// The same name may NOT be present multiple times since parameter names must be unique
        variables: HashMap<Arc<str>, DefId>,
    },

    /// The start of a block
    ///
    /// All declarations are hoisted to the top of a block. That makes them available for
    /// everything else in the block.
    Block {
        /// A submodule is simply a subscope
        modules: HashMap<Arc<str>, ScopeId>,
        /// A list of scopes from which any name may be used
        wildcard_imports: Vec<ScopeId>,
        /// Names visible in a type context
        types: HashMap<Arc<str>, DefId>,
        /// Names visible in a calling context and an expression context
        functions: HashMap<Arc<str>, DefId>,
        /// Names visible in an expression context, inserted in declaration order
        ///
        /// The same name may be present multiple times if it was shadowed
        variables: Vec<(Arc<str>, DefId)>,
    },

    /// The continuation of a block after a subscope
    ///
    /// The parent of a scope with this kind may either be the same kind (`BlockVars`) or `Block`.
    BlockVars {
        /// Names visible in an expression context, inserted in declaration order
        ///
        /// The same name may be present multiple times if it was shadowed
        variables: Vec<(Arc<str>, DefId)>,
    },
}

impl ScopeKind {
    pub fn new_module() -> Self {
        ScopeKind::Module {
            modules: HashMap::new(),
            wildcard_imports: Vec::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn new_impl(self_ty: DefId) -> Self {
        ScopeKind::Impl {
            types: HashMap::new(),
            self_ty,
        }
    }

    pub fn new_function() -> Self {
        ScopeKind::Function {
            types: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn new_block() -> Self {
        ScopeKind::Block {
            modules: HashMap::new(),
            wildcard_imports: Vec::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
            variables: Vec::new(),
        }
    }

    pub fn new_block_vars() -> Self {
        ScopeKind::BlockVars {
            variables: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    // Root scope/module has id == parent
    parent: ScopeId,
    /// The kind of scope this is (used during traversal)
    kind: ScopeKind,
}

impl Scope {
    pub fn parent(&self) -> ScopeId {
        self.parent
    }

    pub fn kind(&self) -> &ScopeKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut ScopeKind {
        &mut self.kind
    }
}

#[derive(Debug)]
pub struct ScopeTree {
    pkg_id: PkgId,
    /// Each `ScopeId` corresponds to an index into this list of scopes
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn new(pkg_id: PkgId) -> Self {
        Self {
            pkg_id,
            scopes: vec![Scope {
                // Root scope has parent ID equal to its scope ID
                parent: ScopeId {pkg: pkg_id, scope_index: 0},
                kind: ScopeKind::new_module(),
            }],
        }
    }

    /// Returns the ID of the root scope
    pub fn root(&self) -> ScopeId {
        ScopeId {pkg: self.pkg_id, scope_index: 0}
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        let ScopeId {pkg, scope_index} = id;
        assert_eq!(self.pkg_id, pkg);

        &self.scopes[scope_index]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        let ScopeId {pkg, scope_index} = id;
        assert_eq!(self.pkg_id, pkg);

        &mut self.scopes[scope_index]
    }
}
