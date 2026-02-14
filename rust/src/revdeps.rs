use crate::boollogic::BoolLogicNode;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use wasm_bindgen::prelude::*;

/// Reverse dependency index
/// Maps path_id → set of BoolLogic IDs that depend on that path
thread_local! {
    static REV_DEPS: std::cell::RefCell<ReverseDependencyIndex> =
        std::cell::RefCell::new(ReverseDependencyIndex::new());
}

struct ReverseDependencyIndex {
    /// path_id → set of logic_ids that reference this path
    path_to_logic: HashMap<u32, HashSet<u32>>,
    /// logic_id → set of path_ids it depends on (for cleanup)
    logic_to_paths: HashMap<u32, HashSet<u32>>,
}

impl ReverseDependencyIndex {
    fn new() -> Self {
        Self {
            path_to_logic: HashMap::new(),
            logic_to_paths: HashMap::new(),
        }
    }

    /// Register a BoolLogic tree and extract its path dependencies
    fn register(&mut self, logic_id: u32, tree: &BoolLogicNode) {
        // Extract all path IDs from the tree
        let path_ids = tree.extract_path_ids();

        // Store logic → paths mapping for cleanup
        self.logic_to_paths.insert(logic_id, path_ids.iter().copied().collect());

        // Update path → logic mappings
        for path_id in path_ids {
            self.path_to_logic
                .entry(path_id)
                .or_insert_with(HashSet::new)
                .insert(logic_id);
        }
    }

    /// Unregister a BoolLogic tree and clean up all its references
    fn unregister(&mut self, logic_id: u32) {
        // Get all paths this logic depends on
        if let Some(path_ids) = self.logic_to_paths.remove(&logic_id) {
            // Remove this logic_id from all path → logic mappings
            for path_id in path_ids {
                if let Some(logic_set) = self.path_to_logic.get_mut(&path_id) {
                    logic_set.remove(&logic_id);
                    // Clean up empty sets
                    if logic_set.is_empty() {
                        self.path_to_logic.remove(&path_id);
                    }
                }
            }
        }
    }

    /// Get all BoolLogic IDs affected by a path change
    fn affected_by(&self, path_id: u32) -> Vec<u32> {
        self.path_to_logic
            .get(&path_id)
            .map(|set| {
                let mut ids: Vec<u32> = set.iter().copied().collect();
                ids.sort_unstable();
                ids
            })
            .unwrap_or_default()
    }

    /// Clear the entire index
    fn clear(&mut self) {
        self.path_to_logic.clear();
        self.logic_to_paths.clear();
    }

    /// Get statistics for debugging
    fn stats(&self) -> IndexStats {
        IndexStats {
            path_count: self.path_to_logic.len(),
            logic_count: self.logic_to_paths.len(),
            total_edges: self.logic_to_paths.values().map(|s| s.len()).sum(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IndexStats {
    pub path_count: usize,
    pub logic_count: usize,
    pub total_edges: usize,
}

/// WASM export: Register a BoolLogic tree from JSON
#[wasm_bindgen]
pub fn register_bool_logic(logic_id: u32, tree_json: &str) -> Result<(), JsValue> {
    let tree: BoolLogicNode = serde_json::from_str(tree_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse tree: {}", e)))?;

    REV_DEPS.with(|deps| {
        deps.borrow_mut().register(logic_id, &tree);
    });

    Ok(())
}

/// WASM export: Unregister a BoolLogic tree
#[wasm_bindgen]
pub fn unregister_bool_logic(logic_id: u32) {
    REV_DEPS.with(|deps| {
        deps.borrow_mut().unregister(logic_id);
    });
}

/// WASM export: Get all logic IDs affected by a path change
#[wasm_bindgen]
pub fn affected_by_change(path_id: u32) -> Vec<u32> {
    REV_DEPS.with(|deps| deps.borrow().affected_by(path_id))
}

/// WASM export: Clear the reverse dependency index
#[wasm_bindgen]
pub fn clear_rev_deps() {
    REV_DEPS.with(|deps| deps.borrow_mut().clear());
}

/// WASM export: Get index statistics as JSON
#[wasm_bindgen]
pub fn rev_deps_stats() -> String {
    REV_DEPS.with(|deps| {
        let stats = deps.borrow().stats();
        serde_json::to_string(&stats).unwrap_or_else(|_| "{}".to_string())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_single_path() {
        clear_rev_deps();

        let tree = BoolLogicNode::IsEqual {
            path_id: 5,
            expected: crate::boollogic::ValueRepr::Bool(true),
        };
        let tree_json = serde_json::to_string(&tree).unwrap();

        register_bool_logic(100, &tree_json).unwrap();

        let affected = affected_by_change(5);
        assert_eq!(affected, vec![100]);
    }

    #[test]
    fn test_register_multiple_paths() {
        clear_rev_deps();

        // AND with two different paths
        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: crate::boollogic::ValueRepr::String("admin".to_string()),
                },
                BoolLogicNode::Exists { path_id: 2 },
            ],
        };
        let tree_json = serde_json::to_string(&tree).unwrap();

        register_bool_logic(200, &tree_json).unwrap();

        // Both paths should reference the same logic
        assert_eq!(affected_by_change(1), vec![200]);
        assert_eq!(affected_by_change(2), vec![200]);
    }

    #[test]
    fn test_multiple_logics_same_path() {
        clear_rev_deps();

        let tree1 = BoolLogicNode::IsEqual {
            path_id: 10,
            expected: crate::boollogic::ValueRepr::Bool(true),
        };
        let tree2 = BoolLogicNode::Exists { path_id: 10 };

        register_bool_logic(300, &serde_json::to_string(&tree1).unwrap()).unwrap();
        register_bool_logic(301, &serde_json::to_string(&tree2).unwrap()).unwrap();

        let mut affected = affected_by_change(10);
        affected.sort_unstable();
        assert_eq!(affected, vec![300, 301]);
    }

    #[test]
    fn test_unregister() {
        clear_rev_deps();

        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::Exists { path_id: 20 },
                BoolLogicNode::Exists { path_id: 21 },
            ],
        };
        let tree_json = serde_json::to_string(&tree).unwrap();

        register_bool_logic(400, &tree_json).unwrap();

        assert_eq!(affected_by_change(20), vec![400]);
        assert_eq!(affected_by_change(21), vec![400]);

        unregister_bool_logic(400);

        assert_eq!(affected_by_change(20), Vec::<u32>::new());
        assert_eq!(affected_by_change(21), Vec::<u32>::new());
    }

    #[test]
    fn test_unregister_shared_path() {
        clear_rev_deps();

        // Two logics sharing path 30
        let tree1 = BoolLogicNode::Exists { path_id: 30 };
        let tree2 = BoolLogicNode::IsEqual {
            path_id: 30,
            expected: crate::boollogic::ValueRepr::String("test".to_string()),
        };

        register_bool_logic(500, &serde_json::to_string(&tree1).unwrap()).unwrap();
        register_bool_logic(501, &serde_json::to_string(&tree2).unwrap()).unwrap();

        assert_eq!(affected_by_change(30), vec![500, 501]);

        // Unregister one logic
        unregister_bool_logic(500);

        // The other should still be referenced
        assert_eq!(affected_by_change(30), vec![501]);
    }

    #[test]
    fn test_nested_complex_tree() {
        clear_rev_deps();

        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: crate::boollogic::ValueRepr::String("admin".to_string()),
                },
                BoolLogicNode::Or {
                    children: vec![
                        BoolLogicNode::Exists { path_id: 2 },
                        BoolLogicNode::Gt {
                            path_id: 3,
                            threshold: 0.0,
                        },
                    ],
                },
                BoolLogicNode::Not {
                    child: Box::new(BoolLogicNode::IsEmpty { path_id: 4 }),
                },
            ],
        };
        let tree_json = serde_json::to_string(&tree).unwrap();

        register_bool_logic(600, &tree_json).unwrap();

        // All paths should reference logic 600
        assert_eq!(affected_by_change(1), vec![600]);
        assert_eq!(affected_by_change(2), vec![600]);
        assert_eq!(affected_by_change(3), vec![600]);
        assert_eq!(affected_by_change(4), vec![600]);

        // Path not in tree should return empty
        assert_eq!(affected_by_change(99), Vec::<u32>::new());
    }

    #[test]
    fn test_stats() {
        clear_rev_deps();

        let tree1 = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::Exists { path_id: 1 },
                BoolLogicNode::Exists { path_id: 2 },
            ],
        };
        let tree2 = BoolLogicNode::Exists { path_id: 3 };

        register_bool_logic(700, &serde_json::to_string(&tree1).unwrap()).unwrap();
        register_bool_logic(701, &serde_json::to_string(&tree2).unwrap()).unwrap();

        let stats_json = rev_deps_stats();
        let stats: IndexStats = serde_json::from_str(&stats_json).unwrap();

        assert_eq!(stats.path_count, 3); // paths 1, 2, 3
        assert_eq!(stats.logic_count, 2); // logics 700, 701
        assert_eq!(stats.total_edges, 3); // 700→{1,2}, 701→{3}
    }

    #[test]
    fn test_clear() {
        clear_rev_deps();

        let tree = BoolLogicNode::Exists { path_id: 50 };
        register_bool_logic(800, &serde_json::to_string(&tree).unwrap()).unwrap();

        assert_eq!(affected_by_change(50), vec![800]);

        clear_rev_deps();

        assert_eq!(affected_by_change(50), Vec::<u32>::new());

        let stats_json = rev_deps_stats();
        let stats: IndexStats = serde_json::from_str(&stats_json).unwrap();
        assert_eq!(stats.path_count, 0);
        assert_eq!(stats.logic_count, 0);
    }
}
