use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::symbol_table::{GenIdSync, SymId};

/// An ID for any module, type, function, variable, etc.
///
/// Note that method names in method calls do NOT get a DefId, but once the method name is
/// resolved, it gets resolved to the DefId of the method declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(usize);

impl SymId for DefId {
    type Gen = Arc<DefIdGen>;
}

/// Generates DefIds
///
/// This generator uses atomics so it can be used through an `Arc`.
#[derive(Debug, Default)]
pub struct DefIdGen(AtomicUsize);

impl GenIdSync<DefId> for DefIdGen {
    fn next_id(&self) -> DefId {
        DefId(self.0.fetch_add(1, Ordering::SeqCst))
    }
}
