//! Validator registry with reverse dependency tracking.
//!
//! Tracks which validators need to run when paths change. Unlike BoolLogic,
//! validators don't have a tree to evaluate in WASM — WASM only decides which
//! validators to run. Zod schemas execute in JS.
//!
//! The registry stores metadata only (output path + dependency paths).

use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};

/// Metadata stored per registered validator.
pub(crate) struct ValidatorMetadata {
    pub output_path: String,
    pub dependency_paths: Vec<String>,
}

/// Registry of validators keyed by JS-assigned u32 IDs.
///
/// Unlike BoolLogic (where WASM assigns IDs), validator IDs are JS-assigned
/// because JS needs to keep a Map<id, ZodSchema> and controls the ID space.
pub(crate) struct ValidatorRegistry {
    validators: HashMap<u32, ValidatorMetadata>,
}

impl ValidatorRegistry {
    pub(crate) fn new() -> Self {
        Self {
            validators: HashMap::new(),
        }
    }

    /// Register a new validator.
    ///
    /// Also updates the reverse dependency index with the dependency paths.
    /// validator_id is JS-assigned (not auto-generated).
    pub(crate) fn register(
        &mut self,
        validator_id: u32,
        output_path: String,
        dependency_paths: Vec<String>,
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
        rev_index.add(validator_id, &interned_ids);

        // Store metadata
        self.validators.insert(
            validator_id,
            ValidatorMetadata {
                output_path,
                dependency_paths,
            },
        );
    }

    /// Unregister a validator and clean up reverse index entries.
    pub(crate) fn unregister(&mut self, validator_id: u32, rev_index: &mut ReverseDependencyIndex) {
        if self.validators.remove(&validator_id).is_some() {
            rev_index.remove(validator_id);
        }
    }

    /// Get metadata for a given validator_id.
    pub(crate) fn get(&self, validator_id: u32) -> Option<&ValidatorMetadata> {
        self.validators.get(&validator_id)
    }

    /// Number of registered validators.
    #[allow(dead_code)]
    pub(crate) fn len(&self) -> usize {
        self.validators.len()
    }
}

// ---------------------------------------------------------------------------
// Batch registration input format
// ---------------------------------------------------------------------------

/// Input format for batch validator registration.
#[derive(Deserialize, Debug)]
pub(crate) struct ValidatorInput {
    pub validator_id: u32,
    pub output_path: String,
    pub dependency_paths: Vec<String>,
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intern::InternTable;
    use crate::rev_index::ReverseDependencyIndex;

    #[test]
    fn register_single_validator() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        registry.register(
            1,
            "_concerns.user.email.validationState".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        assert_eq!(registry.len(), 1);

        let meta = registry.get(1).unwrap();
        assert_eq!(meta.output_path, "_concerns.user.email.validationState");
        assert_eq!(meta.dependency_paths, vec!["user.email"]);

        // Verify reverse index lookup
        let path_id = intern.intern("user.email");
        let affected = rev.affected_by_path(path_id);
        assert_eq!(affected, vec![1]);
    }

    #[test]
    fn register_validator_with_multiple_dependencies() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        registry.register(
            2,
            "_concerns.order.total.validationState".into(),
            vec!["order.amount".into(), "order.currency".into()],
            &mut intern,
            &mut rev,
        );

        assert_eq!(registry.len(), 1);

        // Lookup by first dependency path
        let path_id1 = intern.intern("order.amount");
        let affected1 = rev.affected_by_path(path_id1);
        assert_eq!(affected1, vec![2]);

        // Lookup by second dependency path
        let path_id2 = intern.intern("order.currency");
        let affected2 = rev.affected_by_path(path_id2);
        assert_eq!(affected2, vec![2]);
    }

    #[test]
    fn register_multiple_validators_same_path() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        registry.register(
            1,
            "_concerns.user.email.validationState".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            2,
            "_concerns.user.email.required".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        assert_eq!(registry.len(), 2);

        // Lookup by shared path should return both validators
        let path_id = intern.intern("user.email");
        let mut affected = rev.affected_by_path(path_id);
        affected.sort();
        assert_eq!(affected, vec![1, 2]);
    }

    #[test]
    fn unregister_removes_from_reverse_index() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        registry.register(
            1,
            "_concerns.user.email.validationState".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        let path_id = intern.intern("user.email");

        // Before unregister: should find validator
        let affected = rev.affected_by_path(path_id);
        assert_eq!(affected, vec![1]);

        // Unregister
        registry.unregister(1, &mut rev);

        // After unregister: should be empty
        let affected = rev.affected_by_path(path_id);
        assert!(affected.is_empty());
        assert_eq!(registry.len(), 0);
        assert_eq!(rev.path_count(), 0);
    }

    #[test]
    fn unregister_nonexistent_is_noop() {
        let mut registry = ValidatorRegistry::new();
        let mut rev = ReverseDependencyIndex::new();

        // Should not panic
        registry.unregister(999, &mut rev);
        assert_eq!(registry.len(), 0);
    }

    #[test]
    fn batch_register_multiple_validators() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        // Register 3 validators
        registry.register(
            1,
            "_concerns.user.email.validationState".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            2,
            "_concerns.user.name.validationState".into(),
            vec!["user.name".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            3,
            "_concerns.order.total.validationState".into(),
            vec!["order.amount".into(), "order.currency".into()],
            &mut intern,
            &mut rev,
        );

        assert_eq!(registry.len(), 3);

        // Verify all are accessible
        assert!(registry.get(1).is_some());
        assert!(registry.get(2).is_some());
        assert!(registry.get(3).is_some());

        // Verify reverse index lookups
        let email_id = intern.intern("user.email");
        assert_eq!(rev.affected_by_path(email_id), vec![1]);

        let name_id = intern.intern("user.name");
        assert_eq!(rev.affected_by_path(name_id), vec![2]);

        let amount_id = intern.intern("order.amount");
        assert_eq!(rev.affected_by_path(amount_id), vec![3]);

        let currency_id = intern.intern("order.currency");
        assert_eq!(rev.affected_by_path(currency_id), vec![3]);
    }

    #[test]
    fn batch_unregister_multiple_validators() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        // Register 3 validators
        registry.register(
            1,
            "_concerns.user.email.validationState".into(),
            vec!["user.email".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            2,
            "_concerns.user.name.validationState".into(),
            vec!["user.name".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            3,
            "_concerns.user.age.validationState".into(),
            vec!["user.age".into()],
            &mut intern,
            &mut rev,
        );

        assert_eq!(registry.len(), 3);

        // Unregister all 3
        registry.unregister(1, &mut rev);
        registry.unregister(2, &mut rev);
        registry.unregister(3, &mut rev);

        assert_eq!(registry.len(), 0);

        // Verify reverse index is cleaned up
        let email_id = intern.intern("user.email");
        assert!(rev.affected_by_path(email_id).is_empty());

        let name_id = intern.intern("user.name");
        assert!(rev.affected_by_path(name_id).is_empty());

        let age_id = intern.intern("user.age");
        assert!(rev.affected_by_path(age_id).is_empty());

        assert_eq!(rev.path_count(), 0);
    }

    #[test]
    fn validator_deduplication() {
        let mut registry = ValidatorRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        // Register two validators with overlapping dependencies
        registry.register(
            1,
            "_concerns.order.total.validationState".into(),
            vec!["order.amount".into(), "order.currency".into()],
            &mut intern,
            &mut rev,
        );

        registry.register(
            2,
            "_concerns.order.discount.validationState".into(),
            vec!["order.amount".into(), "order.discount".into()],
            &mut intern,
            &mut rev,
        );

        // Change the shared path "order.amount"
        let amount_id = intern.intern("order.amount");
        let mut affected = rev.affected_by_path(amount_id);
        affected.sort();

        // Both validators should appear exactly once (deduped)
        assert_eq!(affected.len(), 2);
        assert_eq!(affected, vec![1, 2]);

        // Verify no duplicates in the result
        let unique_count = affected
            .iter()
            .collect::<std::collections::HashSet<_>>()
            .len();
        assert_eq!(unique_count, 2);
    }
}
