use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

/// Value representation for types that can be stored in shadow state
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ValueRepr {
    Array(Vec<ValueRepr>),
    Object(HashMap<String, ValueRepr>),
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

impl ValueRepr {
    /// Check if value is empty (for IS_EMPTY operator)
    /// Matches JS is.empty() behavior:
    /// - null/undefined → true
    /// - string → length === 0
    /// - array → length === 0
    /// - object → has no own properties
    /// - number/boolean → false
    fn is_empty(&self) -> bool {
        match self {
            ValueRepr::Null => true,
            ValueRepr::String(s) => s.is_empty(),
            ValueRepr::Array(arr) => arr.is_empty(),
            ValueRepr::Object(obj) => obj.is_empty(),
            ValueRepr::Number(_) | ValueRepr::Bool(_) => false,
        }
    }

    /// Check if value exists (for EXISTS operator)
    fn exists(&self) -> bool {
        !matches!(self, ValueRepr::Null)
    }

    /// Try to get numeric value
    fn as_number(&self) -> Option<f64> {
        match self {
            ValueRepr::Number(n) => Some(*n),
            _ => None,
        }
    }
}

/// BoolLogic expression tree node
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum BoolLogicNode {
    /// Equality check: path_id value === expected
    IsEqual { path_id: u32, expected: ValueRepr },

    /// Existence check: path_id value is not null/undefined
    Exists { path_id: u32 },

    /// Emptiness check: path_id value is empty
    IsEmpty { path_id: u32 },

    /// Logical AND: all children must be true
    And { children: Vec<BoolLogicNode> },

    /// Logical OR: at least one child must be true
    Or { children: Vec<BoolLogicNode> },

    /// Logical NOT: negation of child
    Not { child: Box<BoolLogicNode> },

    /// Greater than: path_id value > threshold
    Gt { path_id: u32, threshold: f64 },

    /// Less than: path_id value < threshold
    Lt { path_id: u32, threshold: f64 },

    /// Greater than or equal: path_id value >= threshold
    Gte { path_id: u32, threshold: f64 },

    /// Less than or equal: path_id value <= threshold
    Lte { path_id: u32, threshold: f64 },

    /// Inclusion: path_id value in allowed list
    In {
        path_id: u32,
        allowed: Vec<ValueRepr>,
    },
}

impl BoolLogicNode {
    /// Evaluate this BoolLogic node against shadow state
    pub fn evaluate(&self, shadow_state: &HashMap<u32, ValueRepr>) -> bool {
        match self {
            BoolLogicNode::IsEqual { path_id, expected } => {
                shadow_state.get(path_id) == Some(expected)
            }

            BoolLogicNode::Exists { path_id } => shadow_state
                .get(path_id)
                .map(|v| v.exists())
                .unwrap_or(false),

            BoolLogicNode::IsEmpty { path_id } => shadow_state
                .get(path_id)
                .map(|v| v.is_empty())
                .unwrap_or(true), // Missing value is considered empty

            BoolLogicNode::And { children } => children.iter().all(|c| c.evaluate(shadow_state)),

            BoolLogicNode::Or { children } => children.iter().any(|c| c.evaluate(shadow_state)),

            BoolLogicNode::Not { child } => !child.evaluate(shadow_state),

            BoolLogicNode::Gt { path_id, threshold } => shadow_state
                .get(path_id)
                .and_then(|v| v.as_number())
                .map(|n| n > *threshold)
                .unwrap_or(false),

            BoolLogicNode::Lt { path_id, threshold } => shadow_state
                .get(path_id)
                .and_then(|v| v.as_number())
                .map(|n| n < *threshold)
                .unwrap_or(false),

            BoolLogicNode::Gte { path_id, threshold } => shadow_state
                .get(path_id)
                .and_then(|v| v.as_number())
                .map(|n| n >= *threshold)
                .unwrap_or(false),

            BoolLogicNode::Lte { path_id, threshold } => shadow_state
                .get(path_id)
                .and_then(|v| v.as_number())
                .map(|n| n <= *threshold)
                .unwrap_or(false),

            BoolLogicNode::In { path_id, allowed } => shadow_state
                .get(path_id)
                .map(|v| allowed.contains(v))
                .unwrap_or(false),
        }
    }

    /// Extract all path IDs referenced in this tree
    pub fn extract_path_ids(&self) -> Vec<u32> {
        let mut ids = Vec::new();
        self.collect_path_ids(&mut ids);
        ids.sort_unstable();
        ids.dedup();
        ids
    }

    fn collect_path_ids(&self, ids: &mut Vec<u32>) {
        match self {
            BoolLogicNode::IsEqual { path_id, .. }
            | BoolLogicNode::Exists { path_id }
            | BoolLogicNode::IsEmpty { path_id }
            | BoolLogicNode::Gt { path_id, .. }
            | BoolLogicNode::Lt { path_id, .. }
            | BoolLogicNode::Gte { path_id, .. }
            | BoolLogicNode::Lte { path_id, .. }
            | BoolLogicNode::In { path_id, .. } => {
                ids.push(*path_id);
            }
            BoolLogicNode::And { children } | BoolLogicNode::Or { children } => {
                for child in children {
                    child.collect_path_ids(ids);
                }
            }
            BoolLogicNode::Not { child } => {
                child.collect_path_ids(ids);
            }
        }
    }
}

/// WASM export: Evaluate a BoolLogic tree from JSON
#[wasm_bindgen]
pub fn evaluate_boollogic(tree_json: &str, state_json: &str) -> Result<bool, JsValue> {
    let tree: BoolLogicNode = serde_json::from_str(tree_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse tree: {}", e)))?;

    let state: HashMap<u32, ValueRepr> = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse state: {}", e)))?;

    Ok(tree.evaluate(&state))
}

/// WASM export: Extract path IDs from a BoolLogic tree
#[wasm_bindgen]
pub fn extract_path_ids(tree_json: &str) -> Result<Vec<u32>, JsValue> {
    let tree: BoolLogicNode = serde_json::from_str(tree_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse tree: {}", e)))?;

    Ok(tree.extract_path_ids())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_state(entries: Vec<(u32, ValueRepr)>) -> HashMap<u32, ValueRepr> {
        entries.into_iter().collect()
    }

    #[test]
    fn test_is_equal() {
        let state = make_state(vec![
            (0, ValueRepr::String("admin".to_string())),
            (1, ValueRepr::Number(42.0)),
        ]);

        let tree = BoolLogicNode::IsEqual {
            path_id: 0,
            expected: ValueRepr::String("admin".to_string()),
        };
        assert!(tree.evaluate(&state));

        let tree = BoolLogicNode::IsEqual {
            path_id: 0,
            expected: ValueRepr::String("user".to_string()),
        };
        assert!(!tree.evaluate(&state));

        let tree = BoolLogicNode::IsEqual {
            path_id: 1,
            expected: ValueRepr::Number(42.0),
        };
        assert!(tree.evaluate(&state));
    }

    #[test]
    fn test_exists() {
        let state = make_state(vec![
            (0, ValueRepr::String("value".to_string())),
            (1, ValueRepr::Null),
        ]);

        let tree = BoolLogicNode::Exists { path_id: 0 };
        assert!(tree.evaluate(&state));

        let tree = BoolLogicNode::Exists { path_id: 1 };
        assert!(!tree.evaluate(&state));

        let tree = BoolLogicNode::Exists { path_id: 99 };
        assert!(!tree.evaluate(&state));
    }

    #[test]
    fn test_is_empty() {
        let state = make_state(vec![
            (0, ValueRepr::String("".to_string())),
            (1, ValueRepr::String("not empty".to_string())),
            (2, ValueRepr::Null),
            (3, ValueRepr::Array(vec![])),
            (4, ValueRepr::Array(vec![ValueRepr::Number(1.0)])),
            (5, ValueRepr::Object(HashMap::new())),
            (
                6,
                ValueRepr::Object(
                    vec![("key".to_string(), ValueRepr::String("value".to_string()))]
                        .into_iter()
                        .collect(),
                ),
            ),
            (7, ValueRepr::Number(0.0)),
            (8, ValueRepr::Bool(false)),
        ]);

        // Empty string
        assert!(BoolLogicNode::IsEmpty { path_id: 0 }.evaluate(&state));

        // Non-empty string
        assert!(!BoolLogicNode::IsEmpty { path_id: 1 }.evaluate(&state));

        // Null
        assert!(BoolLogicNode::IsEmpty { path_id: 2 }.evaluate(&state));

        // Empty array
        assert!(BoolLogicNode::IsEmpty { path_id: 3 }.evaluate(&state));

        // Non-empty array
        assert!(!BoolLogicNode::IsEmpty { path_id: 4 }.evaluate(&state));

        // Empty object
        assert!(BoolLogicNode::IsEmpty { path_id: 5 }.evaluate(&state));

        // Non-empty object
        assert!(!BoolLogicNode::IsEmpty { path_id: 6 }.evaluate(&state));

        // Number (never empty)
        assert!(!BoolLogicNode::IsEmpty { path_id: 7 }.evaluate(&state));

        // Boolean (never empty)
        assert!(!BoolLogicNode::IsEmpty { path_id: 8 }.evaluate(&state));

        // Missing = empty
        assert!(BoolLogicNode::IsEmpty { path_id: 99 }.evaluate(&state));
    }

    #[test]
    fn test_and() {
        let state = make_state(vec![(0, ValueRepr::Bool(true)), (1, ValueRepr::Bool(false))]);

        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 0,
                    expected: ValueRepr::Bool(true),
                },
                BoolLogicNode::IsEqual {
                    path_id: 0,
                    expected: ValueRepr::Bool(true),
                },
            ],
        };
        assert!(tree.evaluate(&state));

        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 0,
                    expected: ValueRepr::Bool(true),
                },
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: ValueRepr::Bool(true),
                },
            ],
        };
        assert!(!tree.evaluate(&state));
    }

    #[test]
    fn test_or() {
        let state = make_state(vec![(0, ValueRepr::Bool(true)), (1, ValueRepr::Bool(false))]);

        let tree = BoolLogicNode::Or {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 0,
                    expected: ValueRepr::Bool(true),
                },
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: ValueRepr::Bool(true),
                },
            ],
        };
        assert!(tree.evaluate(&state));

        let tree = BoolLogicNode::Or {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: ValueRepr::Bool(true),
                },
                BoolLogicNode::IsEqual {
                    path_id: 1,
                    expected: ValueRepr::Bool(true),
                },
            ],
        };
        assert!(!tree.evaluate(&state));
    }

    #[test]
    fn test_not() {
        let state = make_state(vec![(0, ValueRepr::Bool(true))]);

        let tree = BoolLogicNode::Not {
            child: Box::new(BoolLogicNode::IsEqual {
                path_id: 0,
                expected: ValueRepr::Bool(true),
            }),
        };
        assert!(!tree.evaluate(&state));

        let tree = BoolLogicNode::Not {
            child: Box::new(BoolLogicNode::IsEqual {
                path_id: 0,
                expected: ValueRepr::Bool(false),
            }),
        };
        assert!(tree.evaluate(&state));
    }

    #[test]
    fn test_numeric_comparisons() {
        let state = make_state(vec![(0, ValueRepr::Number(10.0))]);

        assert!(BoolLogicNode::Gt {
            path_id: 0,
            threshold: 5.0
        }
        .evaluate(&state));
        assert!(!BoolLogicNode::Gt {
            path_id: 0,
            threshold: 10.0
        }
        .evaluate(&state));

        assert!(BoolLogicNode::Lt {
            path_id: 0,
            threshold: 15.0
        }
        .evaluate(&state));
        assert!(!BoolLogicNode::Lt {
            path_id: 0,
            threshold: 10.0
        }
        .evaluate(&state));

        assert!(BoolLogicNode::Gte {
            path_id: 0,
            threshold: 10.0
        }
        .evaluate(&state));
        assert!(!BoolLogicNode::Gte {
            path_id: 0,
            threshold: 11.0
        }
        .evaluate(&state));

        assert!(BoolLogicNode::Lte {
            path_id: 0,
            threshold: 10.0
        }
        .evaluate(&state));
        assert!(!BoolLogicNode::Lte {
            path_id: 0,
            threshold: 9.0
        }
        .evaluate(&state));
    }

    #[test]
    fn test_in() {
        let state = make_state(vec![(0, ValueRepr::String("apple".to_string()))]);

        let tree = BoolLogicNode::In {
            path_id: 0,
            allowed: vec![
                ValueRepr::String("apple".to_string()),
                ValueRepr::String("banana".to_string()),
            ],
        };
        assert!(tree.evaluate(&state));

        let tree = BoolLogicNode::In {
            path_id: 0,
            allowed: vec![
                ValueRepr::String("orange".to_string()),
                ValueRepr::String("banana".to_string()),
            ],
        };
        assert!(!tree.evaluate(&state));
    }

    #[test]
    fn test_extract_path_ids() {
        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 5,
                    expected: ValueRepr::Bool(true),
                },
                BoolLogicNode::Or {
                    children: vec![
                        BoolLogicNode::Exists { path_id: 2 },
                        BoolLogicNode::Gt {
                            path_id: 10,
                            threshold: 0.0,
                        },
                    ],
                },
                BoolLogicNode::Not {
                    child: Box::new(BoolLogicNode::IsEmpty { path_id: 2 }),
                },
            ],
        };

        let ids = tree.extract_path_ids();
        assert_eq!(ids, vec![2, 5, 10]);
    }

    #[test]
    fn test_serde_json() {
        let tree = BoolLogicNode::And {
            children: vec![
                BoolLogicNode::IsEqual {
                    path_id: 0,
                    expected: ValueRepr::String("admin".to_string()),
                },
                BoolLogicNode::Exists { path_id: 1 },
            ],
        };

        let json = serde_json::to_string(&tree).unwrap();
        let parsed: BoolLogicNode = serde_json::from_str(&json).unwrap();

        let state = make_state(vec![
            (0, ValueRepr::String("admin".to_string())),
            (1, ValueRepr::Number(42.0)),
        ]);

        assert_eq!(tree.evaluate(&state), parsed.evaluate(&state));
    }
}
