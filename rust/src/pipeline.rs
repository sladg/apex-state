//! Processing pipeline: single entry point for all state changes.
//!
//! Owns shadow state, intern table, BoolLogic registry, and reverse
//! dependency index. Processes a batch of changes, updates shadow state,
//! evaluates affected BoolLogic expressions, and returns all changes
//! (input + computed).

use crate::aggregation::{process_aggregation_writes, Aggregation, AggregationRegistry};
use crate::bool_logic::{BoolLogicNode, BoolLogicRegistry, ReverseDependencyIndex};
use crate::graphs::Graph;
use crate::intern::InternTable;
use crate::shadow::ShadowState;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// A single change in the input/output format.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub(crate) struct Change {
    pub path: String,
    pub value_json: String,
}

/// Output wrapper for processChanges.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct ProcessResult {
    pub changes: Vec<Change>,
}

/// Owns all WASM-internal state and orchestrates change processing.
pub(crate) struct ProcessingPipeline {
    shadow: ShadowState,
    intern: InternTable,
    registry: BoolLogicRegistry,
    rev_index: ReverseDependencyIndex,
    aggregations: AggregationRegistry,
    sync_graph: Graph,
    flip_graph: Graph,
}

impl ProcessingPipeline {
    pub(crate) fn new() -> Self {
        Self {
            shadow: ShadowState::new(),
            intern: InternTable::new(),
            registry: BoolLogicRegistry::new(),
            rev_index: ReverseDependencyIndex::new(),
            aggregations: AggregationRegistry::new(),
            sync_graph: Graph::new(),
            flip_graph: Graph::new(),
        }
    }

    /// Reset the entire pipeline to a fresh state.
    pub(crate) fn reset(&mut self) {
        *self = Self::new();
    }

    /// Initialize shadow state from a JSON string.
    pub(crate) fn shadow_init(&mut self, state_json: &str) -> Result<(), String> {
        self.shadow.init(state_json)
    }

    /// Register a BoolLogic expression. Returns logic_id for later cleanup.
    pub(crate) fn register_boollogic(
        &mut self,
        output_path: &str,
        tree_json: &str,
    ) -> Result<u32, String> {
        let tree: BoolLogicNode = serde_json::from_str(tree_json)
            .map_err(|e| format!("BoolLogic parse error: {}", e))?;
        let id = self.registry.register(
            output_path.to_owned(),
            tree,
            &mut self.intern,
            &mut self.rev_index,
        );
        Ok(id)
    }

    /// Unregister a BoolLogic expression.
    pub(crate) fn unregister_boollogic(&mut self, logic_id: u32) {
        self.registry.unregister(logic_id, &mut self.rev_index);
    }

    /// Register a batch of aggregations.
    ///
    /// Input: JSON array of `{ "target": "...", "sources": [...] }`
    pub(crate) fn register_aggregation_batch(&mut self, aggregations_json: &str) -> Result<(), String> {
        let aggs: Vec<Aggregation> = serde_json::from_str(aggregations_json)
            .map_err(|e| format!("Aggregation parse error: {}", e))?;

        for agg in aggs {
            self.aggregations.register(agg.target, agg.sources);
        }

        Ok(())
    }

    /// Unregister a batch of aggregations.
    ///
    /// Input: JSON array of target paths
    pub(crate) fn unregister_aggregation_batch(&mut self, targets_json: &str) -> Result<(), String> {
        let targets: Vec<String> = serde_json::from_str(targets_json)
            .map_err(|e| format!("Targets parse error: {}", e))?;

        for target in targets {
            self.aggregations.unregister(&target);
        }

        Ok(())
    }

    /// Register a batch of sync pairs.
    ///
    /// Input: JSON array of path pairs
    /// Example: `[["user.name", "profile.name"], ["user.email", "profile.email"]]`
    pub(crate) fn register_sync_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Sync pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.sync_graph.add_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Unregister a batch of sync pairs.
    ///
    /// Input: JSON array of path pairs to remove
    pub(crate) fn unregister_sync_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Sync pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.sync_graph.remove_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Register a batch of flip pairs.
    ///
    /// Input: JSON array of path pairs that should stay inverted
    /// Example: `[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]`
    pub(crate) fn register_flip_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Flip pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.flip_graph.add_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Unregister a batch of flip pairs.
    ///
    /// Input: JSON array of path pairs to remove from flip
    pub(crate) fn unregister_flip_batch(&mut self, pairs_json: &str) -> Result<(), String> {
        let pairs: Vec<[String; 2]> = serde_json::from_str(pairs_json)
            .map_err(|e| format!("Flip pairs parse error: {}", e))?;

        for pair in pairs {
            let id1 = self.intern.intern(&pair[0]);
            let id2 = self.intern.intern(&pair[1]);
            self.flip_graph.remove_edge_public(id1, id2);
        }

        Ok(())
    }

    /// Process sync paths: for each changed path, propagate to all peers in its component.
    fn process_sync_paths(&mut self, changes: &[Change]) -> Vec<Change> {
        let mut sync_changes = Vec::new();

        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.sync_graph.get_component_paths_public(path_id);

            // Generate changes for all peers (excluding the source path itself)
            for peer_id in peer_ids {
                if peer_id != path_id {
                    if let Some(peer_path) = self.intern.resolve(peer_id) {
                        sync_changes.push(Change {
                            path: peer_path.to_owned(),
                            value_json: change.value_json.clone(),
                        });
                    }
                }
            }
        }

        sync_changes
    }

    /// Process flip paths: for each changed path, invert value for all peers in its component.
    /// Only applies to boolean values; non-boolean values pass through unchanged.
    fn process_flip_paths(&mut self, changes: &[Change]) -> Vec<Change> {
        let mut flip_changes = Vec::new();

        for change in changes {
            let path_id = self.intern.intern(&change.path);
            let peer_ids = self.flip_graph.get_component_paths_public(path_id);

            // Only process if this path has flip peers
            if peer_ids.is_empty() {
                continue;
            }

            // Try to parse the value as a boolean
            let inverted_value = match change.value_json.trim() {
                "true" => Some("false".to_owned()),
                "false" => Some("true".to_owned()),
                _ => None, // Non-boolean values pass through unchanged
            };

            // If the value was a boolean, generate inverted changes for all peers
            if let Some(inverted) = inverted_value {
                for peer_id in peer_ids {
                    if peer_id != path_id {
                        if let Some(peer_path) = self.intern.resolve(peer_id) {
                            flip_changes.push(Change {
                                path: peer_path.to_owned(),
                                value_json: inverted.clone(),
                            });
                        }
                    }
                }
            }
        }

        flip_changes
    }

    /// Process a batch of changes.
    ///
    /// Algorithm:
    /// 1. Parse input changes
    /// 2. Process aggregation writes (distribute target → sources)
    /// 3. Update shadow state with aggregated changes
    /// 4. Process sync paths (propagate to peers)
    /// 5. Update shadow state with sync changes
    /// 6. Process flip paths (invert booleans for peers)
    /// 7. Update shadow state with flip changes
    /// 8. Find affected BoolLogic expressions via reverse index
    /// 9. Evaluate each affected expression
    /// 10. Return all changes (input/aggregated/sync/flip + BoolLogic)
    pub(crate) fn process_changes(&mut self, changes_json: &str) -> Result<String, String> {
        let input_changes: Vec<Change> = serde_json::from_str(changes_json)
            .map_err(|e| format!("Changes parse error: {}", e))?;

        // Step 1-2: Process aggregation writes (distribute target → sources)
        let mut changes = process_aggregation_writes(&self.aggregations, input_changes);

        // Collect all output changes (start with aggregated input)
        let mut output_changes: Vec<Change> = Vec::with_capacity(changes.len());

        // Track which logic IDs need re-evaluation
        let mut affected_logic_ids: HashSet<u32> = HashSet::new();

        // Step 3: Apply aggregated changes to shadow state and collect affected paths
        for change in &changes {
            // Update shadow state
            self.shadow.set(&change.path, &change.value_json)?;

            // Echo aggregated change to output
            output_changes.push(Change {
                path: change.path.clone(),
                value_json: change.value_json.clone(),
            });

            // Mark affected logic for later evaluation
            let affected_paths = self.shadow.affected_paths(&change.path);
            for affected_path in &affected_paths {
                let path_id = self.intern.intern(affected_path);
                for logic_id in self.rev_index.affected_by_path(path_id) {
                    affected_logic_ids.insert(logic_id);
                }
            }
            let path_id = self.intern.intern(&change.path);
            for logic_id in self.rev_index.affected_by_path(path_id) {
                affected_logic_ids.insert(logic_id);
            }
        }

        // Step 4: Process sync paths (propagate to all peers in component)
        let sync_changes = self.process_sync_paths(&changes);

        // Step 5: Update shadow state with sync changes
        for change in &sync_changes {
            self.shadow.set(&change.path, &change.value_json)?;

            // Add to output
            output_changes.push(Change {
                path: change.path.clone(),
                value_json: change.value_json.clone(),
            });

            // Mark affected logic
            let affected_paths = self.shadow.affected_paths(&change.path);
            for affected_path in &affected_paths {
                let path_id = self.intern.intern(affected_path);
                for logic_id in self.rev_index.affected_by_path(path_id) {
                    affected_logic_ids.insert(logic_id);
                }
            }
            let path_id = self.intern.intern(&change.path);
            for logic_id in self.rev_index.affected_by_path(path_id) {
                affected_logic_ids.insert(logic_id);
            }
        }

        // Extend changes to include sync changes for flip processing
        changes.extend(sync_changes);

        // Step 6: Process flip paths (invert booleans for peers)
        let flip_changes = self.process_flip_paths(&changes);

        // Step 7: Update shadow state with flip changes
        for change in &flip_changes {
            self.shadow.set(&change.path, &change.value_json)?;

            // Add to output
            output_changes.push(Change {
                path: change.path.clone(),
                value_json: change.value_json.clone(),
            });

            // Mark affected logic
            let affected_paths = self.shadow.affected_paths(&change.path);
            for affected_path in &affected_paths {
                let path_id = self.intern.intern(affected_path);
                for logic_id in self.rev_index.affected_by_path(path_id) {
                    affected_logic_ids.insert(logic_id);
                }
            }
            let path_id = self.intern.intern(&change.path);
            for logic_id in self.rev_index.affected_by_path(path_id) {
                affected_logic_ids.insert(logic_id);
            }
        }

        // Step 8-9: Evaluate affected BoolLogic expressions
        for logic_id in &affected_logic_ids {
            if let Some(meta) = self.registry.get(*logic_id) {
                let result = meta.tree.evaluate(&self.shadow);
                output_changes.push(Change {
                    path: meta.output_path.clone(),
                    value_json: if result { "true".to_owned() } else { "false".to_owned() },
                });
            }
        }

        // Step 10: Serialize output
        let result = ProcessResult {
            changes: output_changes,
        };
        serde_json::to_string(&result).map_err(|e| format!("Serialize error: {}", e))
    }

    /// Dump shadow state as JSON (debug/testing).
    pub(crate) fn shadow_dump(&self) -> String {
        self.shadow.dump()
    }

    /// Get a value from shadow state at path (debug/testing).
    pub(crate) fn shadow_get(&self, path: &str) -> Option<String> {
        self.shadow.get(path).map(|v| {
            serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_owned())
        })
    }

    /// Number of interned paths (debug/testing).
    pub(crate) fn intern_count(&self) -> u32 {
        self.intern.count()
    }
}

impl Default for ProcessingPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_pipeline() -> ProcessingPipeline {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"user": {"role": "guest", "age": 20, "email": "test@test.com"}}"#)
            .unwrap();
        p
    }

    // --- basic change processing ---

    #[test]
    fn process_echoes_input_changes() {
        let mut p = make_pipeline();
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.role");
        assert_eq!(parsed.changes[0].value_json, r#""admin""#);
    }

    #[test]
    fn process_updates_shadow_state() {
        let mut p = make_pipeline();
        p.process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        assert_eq!(p.shadow_get("user.role").unwrap(), r#""admin""#);
    }

    #[test]
    fn process_multiple_changes() {
        let mut p = make_pipeline();
        let result = p
            .process_changes(
                r#"[
                    {"path": "user.role", "value_json": "\"admin\""},
                    {"path": "user.age", "value_json": "30"}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(p.shadow_get("user.age").unwrap(), "30");
    }

    #[test]
    fn process_empty_changes() {
        let mut p = make_pipeline();
        let result = p.process_changes("[]").unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert!(parsed.changes.is_empty());
    }

    // --- BoolLogic evaluation ---

    #[test]
    fn process_evaluates_affected_boollogic() {
        let mut p = make_pipeline();

        // Register: disable email field when role is admin
        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change role to admin
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 changes: echoed input + computed BoolLogic
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "user.role");

        let bl_change = &parsed.changes[1];
        assert_eq!(bl_change.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl_change.value_json, "true");
    }

    #[test]
    fn process_boollogic_evaluates_false() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change role to editor (not admin)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"editor\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        assert_eq!(parsed.changes.len(), 2);
        let bl_change = &parsed.changes[1];
        assert_eq!(bl_change.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl_change.value_json, "false");
    }

    #[test]
    fn process_unrelated_change_no_boollogic() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Change age (unrelated to the BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "user.age", "value_json": "25"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Only the echoed input, no BoolLogic evaluation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "user.age");
    }

    #[test]
    fn process_multiple_concerns_same_dependency() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.user.email.readonlyWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.user.name.visibleWhen",
            r#"{"EXISTS": "user.role"}"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // 1 input + 3 BoolLogic evaluations
        assert_eq!(parsed.changes.len(), 4);

        let bl_paths: Vec<&str> = parsed.changes[1..].iter().map(|c| c.path.as_str()).collect();
        assert!(bl_paths.contains(&"_concerns.user.email.disabledWhen"));
        assert!(bl_paths.contains(&"_concerns.user.email.readonlyWhen"));
        assert!(bl_paths.contains(&"_concerns.user.name.visibleWhen"));
    }

    #[test]
    fn process_complex_boollogic() {
        let mut p = make_pipeline();

        // AND: role=admin AND age >= 18
        p.register_boollogic(
            "_concerns.user.panel.visibleWhen",
            r#"{"AND": [{"IS_EQUAL": ["user.role", "admin"]}, {"GTE": ["user.age", 18]}]}"#,
        )
        .unwrap();

        // Set role=admin (age is already 20, so both conditions met)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        let bl = parsed.changes.iter().find(|c| c.path.contains("visibleWhen")).unwrap();
        assert_eq!(bl.value_json, "true");
    }

    #[test]
    fn process_nested_object_update() {
        let mut p = make_pipeline();

        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
        )
        .unwrap();

        // Replace entire user object
        let result = p
            .process_changes(
                r#"[{"path": "user", "value_json": "{\"role\": \"admin\", \"age\": 30}"}]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should trigger BoolLogic since user.role is a descendant of user
        let bl = parsed.changes.iter().find(|c| c.path.contains("disabledWhen"));
        assert!(bl.is_some());
        assert_eq!(bl.unwrap().value_json, "true");
    }

    // --- registration lifecycle ---

    #[test]
    fn register_and_unregister() {
        let mut p = make_pipeline();

        let id = p
            .register_boollogic(
                "_concerns.x.disabledWhen",
                r#"{"IS_EQUAL": ["user.role", "admin"]}"#,
            )
            .unwrap();

        // Should produce BoolLogic output
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 2);

        // Unregister
        p.unregister_boollogic(id);

        // Now no BoolLogic output
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"editor\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.changes.len(), 1);
    }

    #[test]
    fn register_invalid_json_fails() {
        let mut p = make_pipeline();
        assert!(p.register_boollogic("out", "not json").is_err());
    }

    #[test]
    fn process_invalid_changes_json_fails() {
        let mut p = make_pipeline();
        assert!(p.process_changes("not json").is_err());
    }

    // --- shadow state init ---

    #[test]
    fn shadow_init_and_dump() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": 1, "b": "hello"}"#).unwrap();
        let dump = p.shadow_dump();
        let v: serde_json::Value = serde_json::from_str(&dump).unwrap();
        assert_eq!(v["a"], 1);
        assert_eq!(v["b"], "hello");
    }

    #[test]
    fn shadow_get_returns_value() {
        let p = make_pipeline();
        assert_eq!(p.shadow_get("user.role").unwrap(), r#""guest""#);
        assert_eq!(p.shadow_get("user.age").unwrap(), "20");
    }

    #[test]
    fn shadow_get_missing_returns_none() {
        let p = make_pipeline();
        assert!(p.shadow_get("missing.path").is_none());
    }

    // --- aggregation writes ---

    #[test]
    fn aggregation_distributes_to_sources() {
        let mut p = make_pipeline();

        // Register aggregation: allUsers -> [user1, user2, user3]
        p.register_aggregation_batch(
            r#"[{"target": "allUsers", "sources": ["user1", "user2", "user3"]}]"#,
        )
        .unwrap();

        // Write to aggregation target
        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 3 distributed changes (no original target change)
        assert_eq!(parsed.changes.len(), 3);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
        assert!(paths.contains(&"user3"));

        // All should have the same value
        for change in &parsed.changes {
            assert_eq!(change.value_json, "\"alice\"");
        }
    }

    #[test]
    fn aggregation_removes_target_change() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[{"target": "form.allChecked", "sources": ["item1", "item2"]}]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "form.allChecked", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes, NOT the original form.allChecked
        assert_eq!(parsed.changes.len(), 2);
        for change in &parsed.changes {
            assert_ne!(change.path, "form.allChecked");
        }
    }

    #[test]
    fn aggregation_with_child_path() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#,
        )
        .unwrap();

        // Write to a child path of the aggregation target
        let result = p
            .process_changes(r#"[{"path": "allUsers.email", "value_json": "\"test@example.com\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should distribute to both users with the child path appended
        assert_eq!(parsed.changes.len(), 2);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1.email"));
        assert!(paths.contains(&"user2.email"));
    }

    #[test]
    fn aggregation_with_multiple_aggregations() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[
                {"target": "allUsers", "sources": ["user1", "user2"]},
                {"target": "allItems", "sources": ["item1", "item2", "item3"]}
            ]"#,
        )
        .unwrap();

        let result = p
            .process_changes(
                r#"[
                    {"path": "allUsers", "value_json": "\"alice\""},
                    {"path": "allItems", "value_json": "42"}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 + 3 = 5 distributed changes (no original targets)
        assert_eq!(parsed.changes.len(), 5);

        // Check users
        let user_changes: Vec<_> = parsed.changes.iter().filter(|c| c.path.starts_with("user")).collect();
        assert_eq!(user_changes.len(), 2);
        for change in user_changes {
            assert_eq!(change.value_json, "\"alice\"");
        }

        // Check items
        let item_changes: Vec<_> = parsed.changes.iter().filter(|c| c.path.starts_with("item")).collect();
        assert_eq!(item_changes.len(), 3);
        for change in item_changes {
            assert_eq!(change.value_json, "42");
        }
    }

    #[test]
    fn aggregation_unregister() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#)
            .unwrap();

        // After unregister, aggregation should not apply
        p.unregister_aggregation_batch(r#"["allUsers"]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have the original change, not distributed
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "allUsers");
    }

    // --- sync graph tests ---

    #[test]
    fn register_sync_batch() {
        let mut p = make_pipeline();
        let result = p.register_sync_batch(r#"[["user.name", "profile.name"], ["user.email", "profile.email"]]"#);
        assert!(result.is_ok());
    }

    #[test]
    fn register_sync_batch_invalid_json() {
        let mut p = make_pipeline();
        let result = p.register_sync_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn unregister_sync_batch() {
        let mut p = make_pipeline();
        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#).unwrap();
        let result = p.unregister_sync_batch(r#"[["user.name", "profile.name"]]"#);
        assert!(result.is_ok());
    }

    // Note: sync propagation tests are in Phase 2 (WASM-015) when process_sync_paths
    // is integrated into process_changes. For now, we only test registration/unregistration.
    //
    // #[test]
    // fn sync_propagates_change_to_peers() {
    //     // TODO: Implement in WASM-015: processChanges() Phase 2
    // }
    //
    // #[test]
    // fn sync_with_multiple_peers() {
    //     // TODO: Implement in WASM-015: processChanges() Phase 2
    // }

    #[test]
    fn sync_isolated_paths_no_propagation() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"x": "X", "y": "Y"}"#).unwrap();

        // No sync registered
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "\"Z\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have input change, no sync propagation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "x");
    }

    // --- flip graph tests ---

    #[test]
    fn register_flip_batch() {
        let mut p = make_pipeline();
        let result = p.register_flip_batch(r#"[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]"#);
        assert!(result.is_ok());
    }

    #[test]
    fn register_flip_batch_invalid_json() {
        let mut p = make_pipeline();
        let result = p.register_flip_batch("not json");
        assert!(result.is_err());
    }

    #[test]
    fn unregister_flip_batch() {
        let mut p = make_pipeline();
        p.register_flip_batch(r#"[["checkbox1", "checkbox2"]]"#).unwrap();
        let result = p.unregister_flip_batch(r#"[["checkbox1", "checkbox2"]]"#);
        assert!(result.is_ok());
    }

    #[test]
    fn flip_inverts_true_to_false() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"isVisible": true, "isHidden": false}"#).unwrap();
        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "isVisible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + inverted flip change
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isVisible");
        assert_eq!(parsed.changes[0].value_json, "true");
        assert_eq!(parsed.changes[1].path, "isHidden");
        assert_eq!(parsed.changes[1].value_json, "false");
    }

    #[test]
    fn flip_inverts_false_to_true() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"isVisible": true, "isHidden": false}"#).unwrap();
        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "isHidden", "value_json": "false"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + inverted flip change
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isHidden");
        assert_eq!(parsed.changes[0].value_json, "false");
        assert_eq!(parsed.changes[1].path, "isVisible");
        assert_eq!(parsed.changes[1].value_json, "true");
    }

    #[test]
    fn flip_with_non_boolean_passes_through() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"enabled": true, "disabled": false}"#).unwrap();
        p.register_flip_batch(r#"[["enabled", "disabled"]]"#).unwrap();

        // Try to set a non-boolean value (should pass through unchanged)
        let result = p
            .process_changes(r#"[{"path": "enabled", "value_json": "\"string value\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have the input change, no flip
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "enabled");
        assert_eq!(parsed.changes[0].value_json, "\"string value\"");
    }

    #[test]
    fn flip_multiple_peers() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"a": true, "b": false, "c": false}"#).unwrap();
        p.register_flip_batch(r#"[["a", "b", "c"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + 2 inverted flip changes for b and c
        assert_eq!(parsed.changes.len(), 3);
        assert_eq!(parsed.changes[0].path, "a");
        assert_eq!(parsed.changes[0].value_json, "true");

        // Check that both peers got false
        let paths: Vec<&str> = parsed.changes[1..].iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"b"));
        assert!(paths.contains(&"c"));
        for change in &parsed.changes[1..] {
            assert_eq!(change.value_json, "false");
        }
    }

    #[test]
    fn flip_isolated_paths_no_propagation() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"x": true, "y": false}"#).unwrap();

        // No flip registered
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have input change, no flip propagation
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "x");
    }

    #[test]
    fn flip_with_number_value_passes_through() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"count1": 5, "count2": 10}"#).unwrap();
        p.register_flip_batch(r#"[["count1", "count2"]]"#).unwrap();

        let result = p
            .process_changes(r#"[{"path": "count1", "value_json": "42"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should only have the input change, no flip (number is not boolean)
        assert_eq!(parsed.changes.len(), 1);
        assert_eq!(parsed.changes[0].path, "count1");
        assert_eq!(parsed.changes[0].value_json, "42");
    }

    // --- Full pipeline integration tests (WASM-015) ---

    #[test]
    fn full_pipeline_aggregation_only() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "allUsers", "value_json": "\"alice\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have 2 distributed changes (no original target)
        assert_eq!(parsed.changes.len(), 2);
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"user1"));
        assert!(paths.contains(&"user2"));
    }

    #[test]
    fn full_pipeline_sync_only() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"user.name": "alice", "profile.name": "bob"}"#)
            .unwrap();

        p.register_sync_batch(r#"[["user.name", "profile.name"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "user.name", "value_json": "\"charlie\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + sync propagation
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "user.name");
        assert_eq!(parsed.changes[0].value_json, r#""charlie""#);
        assert_eq!(parsed.changes[1].path, "profile.name");
        assert_eq!(parsed.changes[1].value_json, r#""charlie""#);
    }

    #[test]
    fn full_pipeline_flip_only() {
        let mut p = make_pipeline();
        p.shadow_init(r#"{"isVisible": true, "isHidden": false}"#)
            .unwrap();

        p.register_flip_batch(r#"[["isVisible", "isHidden"]]"#)
            .unwrap();

        let result = p
            .process_changes(r#"[{"path": "isVisible", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have input change + flip inversion
        assert_eq!(parsed.changes.len(), 2);
        assert_eq!(parsed.changes[0].path, "isVisible");
        assert_eq!(parsed.changes[0].value_json, "true");
        assert_eq!(parsed.changes[1].path, "isHidden");
        assert_eq!(parsed.changes[1].value_json, "false");
    }

    #[test]
    fn full_pipeline_aggregation_sync_flip() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "allUsers": null,
                "user1": {"status": true},
                "user2": {"status": false},
                "profile1": {"status": null},
                "profile2": {"status": null}
            }"#,
        )
        .unwrap();

        // Register aggregation
        p.register_aggregation_batch(
            r#"[{"target": "allUsers", "sources": ["user1", "user2"]}]"#,
        )
        .unwrap();

        // Register sync: user.status <-> profile.status
        p.register_sync_batch(r#"[["user1.status", "profile1.status"], ["user2.status", "profile2.status"]]"#)
            .unwrap();

        // Register flip: user1.status <-> user2.status
        p.register_flip_batch(r#"[["user1.status", "user2.status"]]"#)
            .unwrap();

        // Write to aggregation target (will distribute to user1, user2)
        let result = p
            .process_changes(r#"[{"path": "allUsers.status", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Expected pipeline:
        // 1. Aggregation: allUsers.status → user1.status, user2.status
        // 2. Sync: user1.status → profile1.status, user2.status → profile2.status
        // 3. Flip: user1.status true → user2.status false (overrides aggregation)
        //
        // Detailed breakdown:
        // - Input: allUsers.status = true
        // - Agg output: user1.status = true, user2.status = true
        // - Sync propagation: profile1.status = true, profile2.status = true
        // - Flip: user2.status = false (inverted from user1.status = true)
        // Total: 5 changes (2 agg + 2 sync + 1 flip override)

        // Find paths in output
        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();

        // Check that key paths are present
        assert!(paths.contains(&"user1.status"), "Missing user1.status");
        assert!(paths.contains(&"user2.status"), "Missing user2.status");
        assert!(paths.contains(&"profile1.status"), "Missing profile1.status");
        assert!(paths.contains(&"profile2.status"), "Missing profile2.status");
    }

    #[test]
    fn full_pipeline_with_boollogic() {
        let mut p = make_pipeline();

        // Register sync
        p.register_sync_batch(r#"[["user.role", "profile.role"]]"#)
            .unwrap();

        // Register BoolLogic that depends on synced field
        p.register_boollogic(
            "_concerns.user.email.disabledWhen",
            r#"{"IS_EQUAL": ["profile.role", "admin"]}"#,
        )
        .unwrap();

        // Change user.role (will sync to profile.role, then trigger BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "user.role", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: input + sync + BoolLogic
        assert_eq!(parsed.changes.len(), 3);
        assert_eq!(parsed.changes[0].path, "user.role");
        assert_eq!(parsed.changes[1].path, "profile.role");
        let bl = &parsed.changes[2];
        assert_eq!(bl.path, "_concerns.user.email.disabledWhen");
        assert_eq!(bl.value_json, "true");
    }

    #[test]
    fn full_pipeline_multiple_boollogics_from_sync() {
        let mut p = make_pipeline();

        p.register_sync_batch(r#"[["flag1", "flag2"]]"#)
            .unwrap();

        p.register_boollogic(
            "_concerns.field1.visibleWhen",
            r#"{"IS_EQUAL": ["flag1", true]}"#,
        )
        .unwrap();
        p.register_boollogic(
            "_concerns.field2.visibleWhen",
            r#"{"IS_EQUAL": ["flag2", true]}"#,
        )
        .unwrap();

        let result = p
            .process_changes(r#"[{"path": "flag1", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Input + sync + 2 BoolLogics
        assert_eq!(parsed.changes.len(), 4);

        // Extract BoolLogic changes
        let bl_paths: Vec<&str> = parsed.changes[2..]
            .iter()
            .map(|c| c.path.as_str())
            .collect();
        assert!(bl_paths.contains(&"_concerns.field1.visibleWhen"));
        assert!(bl_paths.contains(&"_concerns.field2.visibleWhen"));

        // Both should be true
        for change in &parsed.changes[2..] {
            assert_eq!(change.value_json, "true");
        }
    }

    #[test]
    fn full_pipeline_sync_then_flip_then_boollogic() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(r#"{"a": true, "b": false, "c": true}"#)
            .unwrap();

        // a <-> b (sync)
        p.register_sync_batch(r#"[["a", "b"]]"#)
            .unwrap();

        // a <-> c (flip)
        p.register_flip_batch(r#"[["a", "c"]]"#)
            .unwrap();

        // BoolLogic on c
        p.register_boollogic(
            "_concerns.x.disabledWhen",
            r#"{"IS_EQUAL": ["c", false]}"#,
        )
        .unwrap();

        // Set a = false
        let result = p
            .process_changes(r#"[{"path": "a", "value_json": "false"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Pipeline: a=false → b=false (sync) → c=true (flip) → concern=false (BoolLogic)
        // (c flips because a changed: false -> inverted = true, but wait...)
        // Actually: a=false, sync makes b=false, flip inverts a for c: c=true becomes c=false
        // Then BoolLogic: c==false? yes → true

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"a"), "Missing a");
        assert!(paths.contains(&"b"), "Missing b (sync)");
        assert!(paths.contains(&"c"), "Missing c (flip)");
        assert!(paths.contains(&"_concerns.x.disabledWhen"), "Missing BoolLogic");
    }

    #[test]
    fn full_pipeline_shadow_state_kept_in_sync() {
        let mut p = make_pipeline();

        p.register_sync_batch(r#"[["x", "y"]]"#)
            .unwrap();

        p.process_changes(r#"[{"path": "x", "value_json": "\"updated\""}]"#)
            .unwrap();

        // Check that shadow state has both x and y updated
        assert_eq!(p.shadow_get("x").unwrap(), r#""updated""#);
        assert_eq!(p.shadow_get("y").unwrap(), r#""updated""#);
    }

    #[test]
    fn full_pipeline_many_changes_with_sync_and_flip() {
        let mut p = ProcessingPipeline::new();
        p.shadow_init(
            r#"{
                "a1": 1, "a2": 1,
                "b1": true, "b2": false,
                "c1": "x", "c2": "x"
            }"#,
        )
        .unwrap();

        // Sync pairs
        p.register_sync_batch(r#"[["a1", "a2"], ["c1", "c2"]]"#)
            .unwrap();

        // Flip pair
        p.register_flip_batch(r#"[["b1", "b2"]]"#)
            .unwrap();

        // 3 input changes
        let result = p
            .process_changes(
                r#"[
                    {"path": "a1", "value_json": "2"},
                    {"path": "b1", "value_json": "false"},
                    {"path": "c1", "value_json": "\"y\""}
                ]"#,
            )
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Expected:
        // - Input: a1=2, b1=false, c1="y"
        // - Sync: a2=2, c2="y"
        // - Flip: b2=true (inverted from b1=false)
        // Total: 6 changes

        assert_eq!(parsed.changes.len(), 6);

        let paths: Vec<&str> = parsed.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"a1"));
        assert!(paths.contains(&"a2"));
        assert!(paths.contains(&"b1"));
        assert!(paths.contains(&"b2"));
        assert!(paths.contains(&"c1"));
        assert!(paths.contains(&"c2"));

        // Verify values
        let changes_map: std::collections::HashMap<&str, &str> = parsed.changes
            .iter()
            .map(|c| (c.path.as_str(), c.value_json.as_str()))
            .collect();

        assert_eq!(changes_map["a1"], "2");
        assert_eq!(changes_map["a2"], "2");
        assert_eq!(changes_map["b1"], "false");
        assert_eq!(changes_map["b2"], "true"); // flipped
        assert_eq!(changes_map["c1"], r#""y""#);
        assert_eq!(changes_map["c2"], r#""y""#);
    }

    #[test]
    fn full_pipeline_sync_change_triggers_affected_boollogic() {
        let mut p = make_pipeline();

        // x <-> y (sync)
        p.register_sync_batch(r#"[["x", "y"]]"#)
            .unwrap();

        // BoolLogic depends on y
        p.register_boollogic(
            "_concerns.field.disabledWhen",
            r#"{"IS_EQUAL": ["y", "admin"]}"#,
        )
        .unwrap();

        // Change x (will sync to y, then trigger BoolLogic)
        let result = p
            .process_changes(r#"[{"path": "x", "value_json": "\"admin\""}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: input + sync + BoolLogic
        assert_eq!(parsed.changes.len(), 3);

        let bl_change = parsed.changes.iter().find(|c| c.path.contains("disabledWhen")).unwrap();
        assert_eq!(bl_change.value_json, "true");
    }

    #[test]
    fn full_pipeline_aggregation_with_boollogic() {
        let mut p = make_pipeline();

        p.register_aggregation_batch(
            r#"[{"target": "allFlags", "sources": ["flag1", "flag2"]}]"#,
        )
        .unwrap();

        p.register_boollogic(
            "_concerns.panel.visibleWhen",
            r#"{"OR": [{"IS_EQUAL": ["flag1", true]}, {"IS_EQUAL": ["flag2", true]}]}"#,
        )
        .unwrap();

        // Write to aggregation target
        let result = p
            .process_changes(r#"[{"path": "allFlags", "value_json": "true"}]"#)
            .unwrap();
        let parsed: ProcessResult = serde_json::from_str(&result).unwrap();

        // Should have: 2 aggregated changes + 1 BoolLogic
        assert_eq!(parsed.changes.len(), 3);

        let bl = parsed.changes.iter().find(|c| c.path.contains("visibleWhen")).unwrap();
        assert_eq!(bl.value_json, "true");
    }
}
