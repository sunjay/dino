//! Name resolution - translates High-level IR (HIR) to Nameless IR (NIR)

use std::collections::VecDeque;

use crate::hir;
use crate::nir;
use crate::def_id::DefId;
use crate::diagnostics::Diagnostics;
use crate::package::Packages;

pub fn resolve_names(
    pkg: &hir::Package,
    packages: &Packages,
    diag: &mut Diagnostics,
) -> nir::Package {
    let mut walker = HIRWalker {
        scope_stack: VecDeque::new(),
        packages,
        diag,
    };

    walker.resolve_package(pkg)
}

/// The different kinds of scopes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ScopeKind {
    /// The top-level scope for an entire package
    Package,
    /// Module scope
    ///
    /// Most name lookups stop here.
    Module,
    /// Function scope
    ///
    /// Variable lookups stop here. This is to prevent inner functions from accessing the variables
    /// of outer functions.
    Function,
    /// Block scope
    ///
    /// Blocks can access all upper level scopes.
    Block,
}

/// Represents a single level of scope
///
/// This is used during name resolution to figure out what is currently in scope as we walk through
/// the program. Non-wildcard imported names must be unique throughout the entire stack. Most decls
/// can be referenced out of order, so they are grouped together on the stack. Variable names must
/// be defined before they are used.
///
/// Thus, name resolution proceeds in two steps:
/// 1. At each level of scope, collect all imports and decls
/// 2. Walk through the scope, adding one variable at a time, ensuring that variables are only used
///    after their declarations
#[derive(Debug)]
struct Scope {
    /// The kind of scope this is
    kind: ScopeKind,
    /// All symbols from all of these modules are available in the scope
    /// The name is looked up
    wildcard_imports: Vec<DefId>,
    /// All the types in the current scope
    types: nir::DefTable,
    /// All the functions (*not* methods) in the current scope
    functions: nir::DefTable,
    /// All the variables in the current scope
    ///
    /// To support shadowing, variables can be arbitrarily overwritten, at which point they will be
    /// given a fresh `DefId`.
    variables: nir::DefTable,
}

impl Scope {
    /// The top-level scope for the entire package
    pub fn package() -> Self {
        Self {
            kind: ScopeKind::Package,
            wildcard_imports: Vec::new(),
            types: nir::DefTable::default(),
            functions: nir::DefTable::default(),
            variables: nir::DefTable::default(),
        }
    }

    pub fn child(&self, kind: ScopeKind) -> Self {
        Self {
            kind,
            wildcard_imports: Vec::new(),
            types: self.types.new_shared(),
            functions: self.functions.new_shared(),
            variables: self.variables.new_shared(),
        }
    }
}

struct HIRWalker<'a> {
    /// The back of the `VecDeque` is the top of the stack
    scope_stack: VecDeque<Scope>,
    /// The packages available in the root scope
    packages: &'a Packages,
    diag: &'a mut Diagnostics,
}

impl<'a> HIRWalker<'a> {
    fn top_scope(&mut self) -> &mut Scope {
        self.scope_stack.back_mut()
            .expect("bug: scope stack should not be empty")
    }

    /// Pushes a new level onto the scope stack
    fn push_scope(&mut self, kind: ScopeKind) -> &mut Scope {
        let new_scope = self.top_scope().child(kind);
        self.scope_stack.push_back(new_scope);
        self.top_scope()
    }

    /// Removes the upper-most level of the scope stack
    fn pop_scope(&mut self) {
        todo!()
    }

    fn resolve_package(&mut self, pkg: &hir::Package) -> nir::Package {
        let hir::Package {root} = pkg;

        self.scope_stack.push_back(Scope::package());
        let pkg = nir::Package {
            root: self.resolve_module(root),
        };
        self.pop_scope();
        assert!(self.scope_stack.is_empty(), "bug: mismatched push and pop calls");

        pkg
    }

    fn resolve_module(&mut self, module: &hir::Module) -> nir::Module {
        let hir::Module {decls} = module;

        self.push_scope(ScopeKind::Module);


        todo!()
    }

    fn resolve_function(&mut self, func: &hir::Function) -> nir::Function {
        //TODO: Function parameters need to be resolved BEFORE we push a scope for the block of the
        // function since imports inside the function do not affect the name resolution of the params
        todo!()
    }
}
