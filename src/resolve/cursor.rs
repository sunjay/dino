use std::sync::Arc;

use parking_lot::RwLock;

use super::scope::{ScopeId, ScopeTree, ScopeKind};

/// A mutable pointer to a given scope in the scope tree
#[derive(Debug)]
pub struct Cursor {
    tree: Arc<RwLock<ScopeTree>>,
    /// The current position in the scope tree represented by this cursor
    current: ScopeId,
}

impl Cursor {
    pub fn new(tree: Arc<RwLock<ScopeTree>>, current: ScopeId) -> Self {
        Self {tree, current}
    }

    /// Go up one level of scope
    pub fn go_to_prev_level(&mut self) {
        let tree = self.tree.read();

        loop {
            let scope = tree.scope(self.current);

            let id = self.current;
            self.current = scope.parent();
            assert_ne!(id, self.current, "bug: attempt to go up one level of scope at root scope");

            match scope.kind() {
                // If we're current in the continuation of a block, we need to find the block head
                // before we can actually go up a level
                ScopeKind::BlockVars {..} => {},
                _ => break,
            }
        }
    }
}
