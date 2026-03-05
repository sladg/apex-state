//! Shared infrastructure for string-path-indexed registries (aggregation, computation).
//!
//! Provides:
//! - `RegistrySource`: Unified source type (path + optional exclude_when condition)
//! - `HasRegistrySources` trait: Allows generic indexing over entry types
//! - `PathIndexedRegistry<T>`: Generic struct with shared register/unregister/query logic
//! - Free helper functions for index maintenance

use crate::bool_logic::BoolLogicNode;
use crate::prelude::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use ts_rs::TS;

// ---------------------------------------------------------------------------
// Unified source type
// ---------------------------------------------------------------------------

/// A single source path with an optional BoolLogic exclude condition.
/// Shared by both aggregation and computation registries.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
pub struct RegistrySource {
    pub path: String,
    pub exclude_when: Option<BoolLogicNode>,
}

// ---------------------------------------------------------------------------
// Trait for entry types
// ---------------------------------------------------------------------------

/// Trait for registry entry types that hold a target path and a list of sources.
/// Enables `PathIndexedRegistry<T>` to generically index any entry type.
pub(crate) trait HasRegistrySources {
    fn target(&self) -> &str;
    fn sources(&self) -> &[RegistrySource];
}

// ---------------------------------------------------------------------------
// Generic path-indexed registry
// ---------------------------------------------------------------------------

/// Generic registry that maintains reverse indices from source/condition paths to targets.
///
/// Wraps a `HashMap<String, T>` of entries keyed by target path, plus two reverse indices
/// for reactive lookups. Shared by `AggregationRegistry` and `ComputationRegistry`.
#[derive(Debug)]
pub(crate) struct PathIndexedRegistry<T: HasRegistrySources> {
    pub(crate) entries: HashMap<String, T>,
    source_to_targets: HashMap<String, Vec<String>>,
    condition_path_to_targets: HashMap<String, Vec<String>>,
}

impl<T: HasRegistrySources> PathIndexedRegistry<T> {
    pub(crate) fn new() -> Self {
        Self {
            entries: HashMap::new(),
            source_to_targets: HashMap::new(),
            condition_path_to_targets: HashMap::new(),
        }
    }

    /// Register an entry, updating reverse indices for all its sources.
    pub(crate) fn register(&mut self, entry: T) {
        let target = entry.target().to_owned();

        for source in entry.sources() {
            self.source_to_targets
                .entry(source.path.clone())
                .or_default()
                .push(target.clone());

            if let Some(ref condition) = source.exclude_when {
                add_condition_paths(condition, &target, &mut self.condition_path_to_targets);
            }
        }

        self.entries.insert(target, entry);
    }

    /// Unregister an entry by target path, cleaning up all reverse indices.
    pub(crate) fn unregister(&mut self, target: &str) {
        if let Some(entry) = self.entries.get(target) {
            for source in entry.sources() {
                remove_from_index(&source.path, target, &mut self.source_to_targets);

                if let Some(ref condition) = source.exclude_when {
                    remove_condition_paths(condition, target, &mut self.condition_path_to_targets);
                }
            }
        }
        self.entries.remove(target);
    }

    /// Find all unique targets affected by a set of changed paths.
    pub(crate) fn get_affected_targets(&self, changed_paths: &[String]) -> Vec<String> {
        get_affected_targets(
            changed_paths,
            &self.source_to_targets,
            &self.condition_path_to_targets,
        )
    }
}

// ---------------------------------------------------------------------------
// Ancestor path iterator
// ---------------------------------------------------------------------------

/// Yields ancestor paths from most-specific to least-specific.
/// e.g. "user.profile.name" -> ["user.profile", "user"]
fn ancestors_of(path: &str) -> impl Iterator<Item = &str> {
    let mut end = path.len();
    std::iter::from_fn(move || {
        let sub = &path[..end];
        sub.rfind('.').map(|pos| {
            end = pos;
            &path[..pos]
        })
    })
}

// ---------------------------------------------------------------------------
// Free helper functions (still useful independently)
// ---------------------------------------------------------------------------

/// Add condition paths from a BoolLogic expression to the condition index.
pub(crate) fn add_condition_paths(
    condition: &BoolLogicNode,
    target: &str,
    index: &mut HashMap<String, Vec<String>>,
) {
    for cond_path in condition.extract_paths() {
        index.entry(cond_path).or_default().push(target.to_owned());
    }
}

/// Remove condition paths from the condition index during unregistration.
pub(crate) fn remove_condition_paths(
    condition: &BoolLogicNode,
    target: &str,
    index: &mut HashMap<String, Vec<String>>,
) {
    for cond_path in condition.extract_paths() {
        if let Some(targets) = index.get_mut(&cond_path) {
            targets.retain(|t| t != target);
            if targets.is_empty() {
                index.remove(&cond_path);
            }
        }
    }
}

/// Remove a target from a source index entry and clean up if empty.
pub(crate) fn remove_from_index(
    source_path: &str,
    target: &str,
    index: &mut HashMap<String, Vec<String>>,
) {
    if let Some(targets) = index.get_mut(source_path) {
        targets.retain(|t| t != target);
        if targets.is_empty() {
            index.remove(source_path);
        }
    }
}

/// Find all unique targets affected by a set of changed paths.
///
/// Uses the same 4-step algorithm for both aggregation and computation:
/// 1. Direct match on source path
/// 2. Direct match on condition path
/// 3. Child-of-source match (changed path is a child of a registered source)
/// 4. Child-of-condition match
pub(crate) fn get_affected_targets(
    changed_paths: &[String],
    source_to_targets: &HashMap<String, Vec<String>>,
    condition_path_to_targets: &HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut targets = HashSet::default();

    for path in changed_paths {
        if let Some(target_list) = source_to_targets.get(path.as_str()) {
            targets.extend(target_list.iter().cloned());
        }

        if let Some(target_list) = condition_path_to_targets.get(path.as_str()) {
            targets.extend(target_list.iter().cloned());
        }

        // Child-of-source: walk ancestors of changed path, O(D) where D = path depth
        for ancestor in ancestors_of(path) {
            if let Some(target_list) = source_to_targets.get(ancestor) {
                targets.extend(target_list.iter().cloned());
            }
        }

        // Child-of-condition: walk ancestors of changed path, O(D)
        for ancestor in ancestors_of(path) {
            if let Some(target_list) = condition_path_to_targets.get(ancestor) {
                targets.extend(target_list.iter().cloned());
            }
        }
    }

    targets.into_iter().collect()
}
