//! Generic function registry for path-based function dispatch.
//!
//! Unified registry for all JS functions that should be called when paths change:
//! - Custom concerns (validationState, custom evaluate())
//! - Validators (Zod schemas)
//! - Listeners (reactive side effects)
//!
//! WASM doesn't distinguish between these - they're all just functions that:
//! - Watch a set of dependency paths
//! - Get called with scoped state when those paths change
//! - Return changes to apply
//!
//! BoolLogic concerns are NOT in this registry - they're statically evaluated in WASM.

use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// Data structures
// ---------------------------------------------------------------------------

/// Metadata stored per registered function.
#[allow(dead_code)]
pub(crate) struct FunctionMetadata {
    /// JS-assigned function ID.
    pub function_id: u32,
    /// Interned path IDs that this function depends on.
    pub dependency_path_ids: HashSet<u32>,
    /// Original path strings (for debugging/logging).
    pub dependency_paths: Vec<String>,
    /// Scope for state presentation ("" = full state).
    pub scope: String,
    /// Optional output path (for concerns/validators that write to specific paths).
    pub output_path: Option<String>,
}

/// Registry of functions keyed by JS-assigned u32 IDs.
///
/// Functions can be:
/// - Custom concerns: watch paths, return changes for _concerns proxy
/// - Validators: watch paths, return validation results
/// - Listeners: watch paths, return changes for state proxy
///
/// The registry doesn't care - it just tracks dependency paths and enables
/// O(1) lookup of which functions to call when paths change.
pub(crate) struct FunctionRegistry {
    functions: HashMap<u32, FunctionMetadata>,
}

impl FunctionRegistry {
    pub(crate) fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Register a new function.
    ///
    /// Updates the reverse dependency index with dependency paths.
    /// function_id is JS-assigned (not auto-generated).
    pub(crate) fn register(
        &mut self,
        function_id: u32,
        dependency_paths: Vec<String>,
        scope: String,
        output_path: Option<String>,
        intern: &mut InternTable,
        rev_index: &mut ReverseDependencyIndex,
    ) {
        // Intern dependency paths and build set for reverse index
        let mut interned_ids = HashSet::with_capacity(dependency_paths.len());
        for path in &dependency_paths {
            let path_id = intern.intern(path);
            interned_ids.insert(path_id);
        }

        // Update reverse index
        rev_index.add(function_id, &interned_ids);

        // Store metadata
        self.functions.insert(
            function_id,
            FunctionMetadata {
                function_id,
                dependency_path_ids: interned_ids,
                dependency_paths,
                scope,
                output_path,
            },
        );
    }

    /// Unregister a function and clean up reverse index entries.
    pub(crate) fn unregister(&mut self, function_id: u32, rev_index: &mut ReverseDependencyIndex) {
        if self.functions.remove(&function_id).is_some() {
            rev_index.remove(function_id);
        }
    }

    /// Get metadata for a given function_id.
    #[allow(dead_code)]
    pub(crate) fn get(&self, function_id: u32) -> Option<&FunctionMetadata> {
        self.functions.get(&function_id)
    }

    /// Number of registered functions.
    #[allow(dead_code)]
    pub(crate) fn len(&self) -> usize {
        self.functions.len()
    }

    /// Check if registry is empty.
    #[allow(dead_code)]
    pub(crate) fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

// ---------------------------------------------------------------------------
// Batch registration input format
// ---------------------------------------------------------------------------

/// Input format for batch function registration.
#[derive(Deserialize, Debug)]
pub(crate) struct FunctionInput {
    pub function_id: u32,
    pub dependency_paths: Vec<String>,
    pub scope: String,
    pub output_path: Option<String>,
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_registration() {
        let mut registry = FunctionRegistry::new();
        let mut intern = InternTable::new();
        let mut rev_index = ReverseDependencyIndex::new();

        // Register a function
        registry.register(
            1,
            vec!["user.email".to_string(), "user.name".to_string()],
            "user".to_string(),
            Some("_concerns.user.email.validationState".to_string()),
            &mut intern,
            &mut rev_index,
        );

        assert_eq!(registry.len(), 1);

        // Get metadata
        let meta = registry.get(1).unwrap();
        assert_eq!(meta.function_id, 1);
        assert_eq!(meta.scope, "user");
        assert_eq!(meta.dependency_paths.len(), 2);
        assert_eq!(
            meta.output_path,
            Some("_concerns.user.email.validationState".to_string())
        );

        // Verify reverse index
        let email_id = intern.get_id("user.email").unwrap();
        let affected = rev_index.affected_by_path(email_id);
        assert_eq!(affected, vec![1]);

        // Unregister
        registry.unregister(1, &mut rev_index);
        assert_eq!(registry.len(), 0);

        // Verify reverse index cleaned up
        let affected = rev_index.affected_by_path(email_id);
        assert!(affected.is_empty());
    }

    #[test]
    fn test_multiple_functions() {
        let mut registry = FunctionRegistry::new();
        let mut intern = InternTable::new();
        let mut rev_index = ReverseDependencyIndex::new();

        // Register multiple functions watching the same path
        registry.register(
            1,
            vec!["user.email".to_string()],
            "".to_string(),
            None,
            &mut intern,
            &mut rev_index,
        );

        registry.register(
            2,
            vec!["user.email".to_string(), "user.name".to_string()],
            "user".to_string(),
            None,
            &mut intern,
            &mut rev_index,
        );

        // Check reverse index
        let email_id = intern.get_id("user.email").unwrap();
        let mut affected = rev_index.affected_by_path(email_id);
        affected.sort();
        assert_eq!(affected, vec![1, 2]);

        let name_id = intern.get_id("user.name").unwrap();
        let affected = rev_index.affected_by_path(name_id);
        assert_eq!(affected, vec![2]);
    }

    #[test]
    fn test_function_scope_variations() {
        let mut registry = FunctionRegistry::new();
        let mut intern = InternTable::new();
        let mut rev_index = ReverseDependencyIndex::new();

        // Empty scope = full state
        registry.register(
            1,
            vec!["user.email".to_string()],
            "".to_string(),
            None,
            &mut intern,
            &mut rev_index,
        );

        // Scoped state
        registry.register(
            2,
            vec!["user.profile.name".to_string()],
            "user.profile".to_string(),
            None,
            &mut intern,
            &mut rev_index,
        );

        let meta1 = registry.get(1).unwrap();
        assert_eq!(meta1.scope, "");

        let meta2 = registry.get(2).unwrap();
        assert_eq!(meta2.scope, "user.profile");
    }
}
