//! Reverse dependency index: maps input path IDs to dependent entity IDs.
//!
//! Generic data structure used by both BoolLogicRegistry and ValidatorRegistry
//! to track which entities depend on which paths, enabling O(1) average-case
//! lookup of affected entities when a path changes.

use std::sync::Arc;

use crate::prelude::{HashMap, HashSet};
use smallvec::SmallVec;

/// Most paths have 1–4 dependent entities; inline storage avoids heap allocation.
pub(crate) type SmallU32Vec = SmallVec<[u32; 4]>;

/// Maps interned path IDs to the set of entity IDs that depend on them,
/// enabling O(1) average-case lookup of affected entities when a path changes.
pub(crate) struct ReverseDependencyIndex {
    /// path_id -> set of entity_ids
    path_to_entity: HashMap<u32, HashSet<u32>>,
    /// entity_id -> set of path_ids (for cleanup).
    /// Arc shared with the caller so registration requires no deep clone.
    entity_to_paths: HashMap<u32, Arc<HashSet<u32>>>,
}

impl ReverseDependencyIndex {
    pub(crate) fn new() -> Self {
        Self {
            path_to_entity: HashMap::new(),
            entity_to_paths: HashMap::new(),
        }
    }

    /// Add an entity_id with its set of interned input path IDs.
    /// Takes shared ownership via Arc — no deep clone at registration.
    pub(crate) fn add(&mut self, entity_id: u32, path_ids: Arc<HashSet<u32>>) {
        for &path_id in path_ids.as_ref() {
            self.path_to_entity
                .entry(path_id)
                .or_default()
                .insert(entity_id);
        }
        self.entity_to_paths.insert(entity_id, path_ids);
    }

    /// Remove an entity_id and all its reverse index entries.
    pub(crate) fn remove(&mut self, entity_id: u32) {
        if let Some(path_ids) = self.entity_to_paths.remove(&entity_id) {
            for path_id in path_ids.iter() {
                if let Some(set) = self.path_to_entity.get_mut(path_id) {
                    set.remove(&entity_id);
                    if set.is_empty() {
                        self.path_to_entity.remove(path_id);
                    }
                }
            }
        }
    }

    /// Return entity IDs affected by a given interned path ID.
    /// Uses SmallVec to avoid heap allocation in the common 1–4 item case.
    pub(crate) fn affected_by_path(&self, path_id: u32) -> SmallU32Vec {
        self.path_to_entity
            .get(&path_id)
            .map(|set| set.iter().copied().collect())
            .unwrap_or_default()
    }

    /// Number of tracked path entries.
    #[cfg(test)]
    pub(crate) fn path_count(&self) -> usize {
        self.path_to_entity.len()
    }
}
