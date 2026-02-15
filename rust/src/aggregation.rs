//! Aggregation writes processor.
//!
//! When a change targets an aggregation path (e.g., "allUsers"), it gets
//! distributed to all source paths instead (e.g., ["user1", "user2", "user3"]).
//! The original target change is removed from the output.
//!
//! Algorithm:
//! 1. Iterate changes in reverse order
//! 2. Check if path matches aggregation target (exact match or child path)
//! 3. If yes: generate changes for all source paths with same value
//! 4. Remove original target change from output

use crate::pipeline::Change;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single aggregation: target path maps to multiple source paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Aggregation {
    pub target: String,
    pub sources: Vec<String>,
}

/// Registry of all registered aggregations, keyed by target path.
#[derive(Debug)]
pub(crate) struct AggregationRegistry {
    aggregations: HashMap<String, Aggregation>,
}

impl AggregationRegistry {
    pub(crate) fn new() -> Self {
        Self {
            aggregations: HashMap::new(),
        }
    }

    /// Register a single aggregation.
    pub(crate) fn register(&mut self, target: String, sources: Vec<String>) {
        self.aggregations
            .insert(target.clone(), Aggregation { target, sources });
    }

    /// Unregister a single aggregation by target path.
    pub(crate) fn unregister(&mut self, target: &str) {
        self.aggregations.remove(target);
    }

    /// Get aggregation by target path.
    pub(crate) fn get(&self, target: &str) -> Option<&Aggregation> {
        self.aggregations.get(target)
    }

    /// Check if a path is an aggregation target (exact match or child).
    fn find_target(&self, path: &str) -> Option<&Aggregation> {
        // Exact match
        if let Some(agg) = self.aggregations.get(path) {
            return Some(agg);
        }

        // Check for child path (path starts with target + '.')
        for (target, agg) in &self.aggregations {
            if path.starts_with(&format!("{}.", target)) {
                return Some(agg);
            }
        }

        None
    }

    /// Clear all aggregations (for testing).
    pub(crate) fn clear(&mut self) {
        self.aggregations.clear();
    }
}

/// Process aggregation writes: distribute target writes to source paths.
///
/// Algorithm:
/// 1. Iterate changes in reverse (so we can safely remove)
/// 2. For each change, check if path matches aggregation target
/// 3. If exact match: generate changes for all sources with same value
/// 4. If child path: generate changes for all sources with child path appended
/// 5. Remove original target change
pub(crate) fn process_aggregation_writes(
    registry: &AggregationRegistry,
    changes: Vec<Change>,
) -> Vec<Change> {
    let mut output: Vec<Change> = changes.clone();

    // Process in reverse order so we can safely remove items
    let mut i = output.len();
    while i > 0 {
        i -= 1;
        let change = &output[i];

        if let Some(agg) = registry.find_target(&change.path) {
            let is_exact_match = change.path == agg.target;

            // Generate changes for all source paths
            let mut new_changes = Vec::new();
            for source_path in &agg.sources {
                let final_path = if is_exact_match {
                    source_path.clone()
                } else {
                    // Child path: append relative part to source path
                    let relative = &change.path[(agg.target.len() + 1)..];
                    format!("{}.{}", source_path, relative)
                };

                new_changes.push(Change {
                    path: final_path,
                    value_json: change.value_json.clone(),
                });
            }

            // Remove original target change
            output.remove(i);

            // Add distributed changes
            output.extend(new_changes);
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match_aggregation() {
        let mut registry = AggregationRegistry::new();
        registry.register(
            "allUsers".to_string(),
            vec![
                "user1".to_string(),
                "user2".to_string(),
                "user3".to_string(),
            ],
        );

        let changes = vec![Change {
            path: "allUsers".to_string(),
            value_json: "\"alice\"".to_string(),
        }];

        let result = process_aggregation_writes(&registry, changes);

        assert_eq!(result.len(), 3);
        assert_eq!(result[0].path, "user1");
        assert_eq!(result[0].value_json, "\"alice\"");
        assert_eq!(result[1].path, "user2");
        assert_eq!(result[1].value_json, "\"alice\"");
        assert_eq!(result[2].path, "user3");
        assert_eq!(result[2].value_json, "\"alice\"");
    }

    #[test]
    fn test_child_path_aggregation() {
        let mut registry = AggregationRegistry::new();
        registry.register(
            "form.allChecked".to_string(),
            vec!["item1".to_string(), "item2".to_string()],
        );

        let changes = vec![Change {
            path: "form.allChecked".to_string(),
            value_json: "true".to_string(),
        }];

        let result = process_aggregation_writes(&registry, changes);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].path, "item1");
        assert_eq!(result[0].value_json, "true");
        assert_eq!(result[1].path, "item2");
        assert_eq!(result[1].value_json, "true");
    }

    #[test]
    fn test_child_path_of_aggregation_target() {
        let mut registry = AggregationRegistry::new();
        registry.register(
            "allUsers".to_string(),
            vec!["user1".to_string(), "user2".to_string()],
        );

        // Writing to a child path of the aggregation target
        let changes = vec![Change {
            path: "allUsers.email".to_string(),
            value_json: "\"alice@example.com\"".to_string(),
        }];

        let result = process_aggregation_writes(&registry, changes);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].path, "user1.email");
        assert_eq!(result[0].value_json, "\"alice@example.com\"");
        assert_eq!(result[1].path, "user2.email");
        assert_eq!(result[1].value_json, "\"alice@example.com\"");
    }

    #[test]
    fn test_mixed_changes_some_aggregated() {
        let mut registry = AggregationRegistry::new();
        registry.register(
            "allUsers".to_string(),
            vec!["user1".to_string(), "user2".to_string()],
        );

        let changes = vec![
            Change {
                path: "otherField".to_string(),
                value_json: "42".to_string(),
            },
            Change {
                path: "allUsers".to_string(),
                value_json: "\"alice\"".to_string(),
            },
            Change {
                path: "anotherField".to_string(),
                value_json: "\"test\"".to_string(),
            },
        ];

        let result = process_aggregation_writes(&registry, changes);

        // Should have: otherField + 2 distributed users + anotherField
        assert_eq!(result.len(), 4);

        // Check that non-aggregated changes are preserved
        let paths: Vec<&str> = result.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"otherField"));
        assert!(paths.contains(&"anotherField"));
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
    }

    #[test]
    fn test_no_aggregation_match() {
        let registry = AggregationRegistry::new();

        let changes = vec![
            Change {
                path: "user.name".to_string(),
                value_json: "\"alice\"".to_string(),
            },
            Change {
                path: "user.email".to_string(),
                value_json: "\"alice@example.com\"".to_string(),
            },
        ];

        let result = process_aggregation_writes(&registry, changes.clone());

        // No changes should be aggregated
        assert_eq!(result.len(), 2);
        assert_eq!(result, changes);
    }
}
