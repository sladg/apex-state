//! Aggregation processor.
//!
//! Handles two directions:
//! 1. Write direction (target → sources): When a change targets an aggregation path,
//!    it gets distributed to all source paths.
//! 2. Read direction (sources → target): When registering or when sources change,
//!    compute target value using all-equal logic (all same → that value, else null).
//!
//! Write Algorithm:
//! 1. Iterate changes in reverse order
//! 2. Check if path matches aggregation target (exact match or child path)
//! 3. If yes: generate changes for all source paths with same value
//! 4. Remove original target change from output

use crate::pipeline::{Change, UNDEFINED_SENTINEL_JSON};
use crate::shadow::ShadowState;
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
    /// Reverse index: source path → target paths (for reactive updates)
    source_to_targets: HashMap<String, Vec<String>>,
}

impl AggregationRegistry {
    pub(crate) fn new() -> Self {
        Self {
            aggregations: HashMap::new(),
            source_to_targets: HashMap::new(),
        }
    }

    /// Register a single aggregation.
    pub(crate) fn register(&mut self, target: String, sources: Vec<String>) {
        // Update reverse index: each source points to this target
        for source in &sources {
            self.source_to_targets
                .entry(source.clone())
                .or_default()
                .push(target.clone());
        }

        // Register the aggregation
        self.aggregations
            .insert(target.clone(), Aggregation { target, sources });
    }

    /// Unregister a single aggregation by target path.
    #[allow(dead_code)] // Called via WASM exports (invisible to clippy)
    pub(crate) fn unregister(&mut self, target: &str) {
        // Remove from reverse index
        if let Some(agg) = self.aggregations.get(target) {
            for source in &agg.sources {
                if let Some(targets) = self.source_to_targets.get_mut(source) {
                    targets.retain(|t| t != target);
                    if targets.is_empty() {
                        self.source_to_targets.remove(source);
                    }
                }
            }
        }

        // Remove the aggregation
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

    /// Get affected aggregation targets for a set of changed paths.
    /// Returns unique target paths that need recomputation.
    pub(crate) fn get_affected_targets(&self, changed_paths: &[String]) -> Vec<String> {
        let mut targets = std::collections::HashSet::new();

        for path in changed_paths {
            // Direct match: path is a source
            if let Some(target_list) = self.source_to_targets.get(path) {
                targets.extend(target_list.iter().cloned());
            }

            // Parent match: path is a child of a source (e.g., "item1.checked" matches source "item1")
            // Check all registered sources to see if this path is a descendant
            for (source, target_list) in &self.source_to_targets {
                if path.starts_with(&format!("{}.", source)) {
                    targets.extend(target_list.iter().cloned());
                }
            }
        }

        targets.into_iter().collect()
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

/// Compute initial target values for newly registered aggregations.
///
/// Process aggregation reads: recompute target values when sources change.
///
/// This is the reactive part of aggregations - when a source path changes,
/// we need to recompute the target value using all-equal logic.
///
/// Algorithm:
/// 1. Identify affected aggregations (where any source changed)
/// 2. Check if all source values are equal (excluding null/undefined)
/// 3. If all equal → use that value, else → null
/// 4. Return changes (with no-op filtering)
pub(crate) fn process_aggregation_reads(
    registry: &AggregationRegistry,
    shadow: &ShadowState,
    changed_paths: &[String],
) -> Vec<Change> {
    // Get affected targets
    let affected_targets = registry.get_affected_targets(changed_paths);

    if affected_targets.is_empty() {
        return Vec::new();
    }

    // Recompute values for affected targets
    let mut changes = Vec::new();

    for target_path in affected_targets {
        if let Some(agg) = registry.get(&target_path) {
            // Skip empty sources
            if agg.sources.is_empty() {
                let desired_value = "null".to_string();
                let current_target = shadow.get(&agg.target).map(|v| {
                    serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_string())
                });

                if current_target.as_ref() != Some(&desired_value) {
                    changes.push(Change {
                        path: agg.target.clone(),
                        value_json: desired_value,
                    });
                }
                continue;
            }

            // Get values from all source paths
            let source_values: Vec<Option<String>> = agg
                .sources
                .iter()
                .map(|path| {
                    shadow.get(path).map(|v| {
                        serde_json::to_string(&v.to_json_value())
                            .unwrap_or_else(|_| "null".to_string())
                    })
                })
                .collect();

            // If any source doesn't exist in shadow state, clear target to undefined
            // (sources were removed, aggregation should be cleared)
            let desired_value = if source_values.iter().any(|v| v.is_none()) {
                UNDEFINED_SENTINEL_JSON.to_string()
            } else {
                // Unwrap safely (we checked all are Some)
                let values: Vec<String> = source_values.into_iter().map(|v| v.unwrap()).collect();

                // Filter out undefined sentinel (missing/blank) — null is a valid value
                let present_values: Vec<&String> = values
                    .iter()
                    .filter(|v| *v != UNDEFINED_SENTINEL_JSON)
                    .collect();

                if present_values.is_empty() {
                    // All values are undefined (missing) → clear target
                    UNDEFINED_SENTINEL_JSON.to_string()
                } else {
                    let first_value = present_values[0];
                    let all_equal = present_values.iter().all(|v| *v == first_value);

                    if all_equal {
                        // All present values agree (including null)
                        first_value.clone()
                    } else {
                        // Sources disagree → null
                        "null".to_string()
                    }
                }
            };

            // Filter no-op: only create change if target value actually differs
            let current_target = shadow.get(&agg.target).map(|v| {
                serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_string())
            });

            if current_target.as_ref() != Some(&desired_value) {
                changes.push(Change {
                    path: agg.target.clone(),
                    value_json: desired_value,
                });
            }
        }
    }

    changes
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

    // --- Tests for read direction (sources → target) ---

    #[test]
    fn test_initial_values_all_equal() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allChecked": null, "item1": true, "item2": true, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        // Use reactive logic with source paths
        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        assert_eq!(changes[0].value_json, "true");
    }

    #[test]
    fn test_initial_values_different() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allChecked": null, "item1": true, "item2": false, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        // Values differ → target stays null (no change needed)
        assert_eq!(changes.len(), 0);
    }

    #[test]
    fn test_initial_values_all_null() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allChecked": false, "item1": null, "item2": null, "item3": null}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        assert_eq!(changes[0].value_json, "null");
    }

    #[test]
    fn test_initial_values_missing_sources() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allChecked": null, "item1": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        // Missing sources → target set to undefined sentinel
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        assert_eq!(changes[0].value_json, UNDEFINED_SENTINEL_JSON);
    }

    #[test]
    fn test_initial_values_empty_sources() {
        let mut shadow = ShadowState::new();
        shadow.init(r#"{"allChecked": true}"#).unwrap();

        let mut registry = AggregationRegistry::new();
        let sources: Vec<String> = vec![];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        // Empty sources with empty changed_paths → no affected targets → no changes
        assert_eq!(changes.len(), 0);
    }

    #[test]
    fn test_initial_values_strings() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allNames": null, "name1": "alice", "name2": "alice", "name3": "alice"}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "name1".to_string(),
            "name2".to_string(),
            "name3".to_string(),
        ];
        registry.register("allNames".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allNames");
        assert_eq!(changes[0].value_json, "\"alice\"");
    }

    #[test]
    fn test_initial_values_no_op_filter() {
        let mut shadow = ShadowState::new();
        // Target already has the correct value
        shadow
            .init(r#"{"allChecked": true, "item1": true, "item2": true, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        // Should not create a change if target already has the correct value
        assert_eq!(changes.len(), 0);
    }

    #[test]
    fn test_initial_values_no_op_filter_null() {
        let mut shadow = ShadowState::new();
        // Target already null, sources differ
        shadow
            .init(r#"{"allChecked": null, "item1": true, "item2": false, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        let sources = vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ];
        registry.register("allChecked".to_string(), sources.clone());

        let changes = process_aggregation_reads(&registry, &shadow, &sources);

        // Should not create a change (target is null, sources differ → computed value is null)
        assert_eq!(changes.len(), 0);
    }

    // --- Tests for reactive reads (sources → target during runtime) ---

    #[test]
    fn test_reactive_read_source_change_makes_equal() {
        let mut shadow = ShadowState::new();
        // Start: sources differ, target is null
        shadow
            .init(r#"{"allChecked": null, "item1": true, "item2": false, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        registry.register(
            "allChecked".to_string(),
            vec![
                "item1".to_string(),
                "item2".to_string(),
                "item3".to_string(),
            ],
        );

        // Simulate item2 changing to true (now all equal)
        shadow.set("item2", "true").unwrap();
        let changed_paths = vec!["item2".to_string()];

        let changes = process_aggregation_reads(&registry, &shadow, &changed_paths);

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        assert_eq!(changes[0].value_json, "true");
    }

    #[test]
    fn test_reactive_read_source_change_makes_differ() {
        let mut shadow = ShadowState::new();
        // Start: all sources true, target is true
        shadow
            .init(r#"{"allChecked": true, "item1": true, "item2": true, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        registry.register(
            "allChecked".to_string(),
            vec![
                "item1".to_string(),
                "item2".to_string(),
                "item3".to_string(),
            ],
        );

        // Simulate item2 changing to false (now sources differ)
        shadow.set("item2", "false").unwrap();
        let changed_paths = vec!["item2".to_string()];

        let changes = process_aggregation_reads(&registry, &shadow, &changed_paths);

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        // Values differ → target becomes null
        assert_eq!(changes[0].value_json, "null");
    }

    #[test]
    fn test_reactive_read_no_affected_aggregations() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"allChecked": null, "item1": true, "unrelated": "value"}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        registry.register("allChecked".to_string(), vec!["item1".to_string()]);

        // Change unrelated path
        let changed_paths = vec!["unrelated".to_string()];

        let changes = process_aggregation_reads(&registry, &shadow, &changed_paths);

        // Should not create any changes
        assert_eq!(changes.len(), 0);
    }

    #[test]
    fn test_reactive_read_child_path_change() {
        let mut shadow = ShadowState::new();
        // Start: nested values all true
        shadow
            .init(
                r#"{
                "allChecked": true,
                "item1": {"checked": true},
                "item2": {"checked": true},
                "item3": {"checked": true}
            }"#,
            )
            .unwrap();

        let mut registry = AggregationRegistry::new();
        // Aggregation on the items (not their children)
        registry.register(
            "allChecked".to_string(),
            vec![
                "item1".to_string(),
                "item2".to_string(),
                "item3".to_string(),
            ],
        );

        // Change a child path (item2.checked)
        shadow.set("item2.checked", "false").unwrap();
        let changed_paths = vec!["item2.checked".to_string()];

        let changes = process_aggregation_reads(&registry, &shadow, &changed_paths);

        // Should detect that a child of item2 changed and recompute
        // Note: The aggregation compares item1, item2, item3 objects (not their children)
        // Since item2 object itself changed, sources now differ
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "allChecked");
        // Objects will differ, so target becomes null
        assert_eq!(changes[0].value_json, "null");
    }

    #[test]
    fn test_reactive_read_no_op_already_correct() {
        let mut shadow = ShadowState::new();
        // Target already correct
        shadow
            .init(r#"{"allChecked": true, "item1": true, "item2": true, "item3": true}"#)
            .unwrap();

        let mut registry = AggregationRegistry::new();
        registry.register(
            "allChecked".to_string(),
            vec![
                "item1".to_string(),
                "item2".to_string(),
                "item3".to_string(),
            ],
        );

        // Simulate a change that doesn't affect equality (all still true)
        let changed_paths = vec!["item1".to_string()];

        let changes = process_aggregation_reads(&registry, &shadow, &changed_paths);

        // Should not create a change (target already correct)
        assert_eq!(changes.len(), 0);
    }
}
