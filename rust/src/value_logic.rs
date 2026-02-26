//! Value logic DSL for conditional value selection with reverse dependency tracking
//!
//! Sibling to BoolLogic: evaluates conditions and returns arbitrary JSON values
//! (not booleans). Two variants:
//! - `IfThenElse`: nestable for elif chains, conditions reuse BoolLogicNode
//! - `Match`: multi-way switch on a state path value
//!
//! Conditions reuse `BoolLogicNode` — no new condition evaluator needed.
//! THEN/ELSE/CASES values are static JSON — not derived from state.

use crate::bool_logic::{get_path_value, BoolLogicNode};
use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use crate::shadow::ShadowState;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashMap, HashSet};

use ts_rs::TS;

// ---------------------------------------------------------------------------
// ValueLogicNode
// ---------------------------------------------------------------------------

/// Value logic DSL for conditional value selection.
///
/// Two variants:
/// - `IfThenElse`: condition (BoolLogicNode) → THEN value or ELSE (value or nested)
/// - `Match`: lookup path value in CASES map, fallback to DEFAULT
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(untagged)]
#[derive(TS)]
pub enum ValueLogicNode {
    IfThenElse {
        #[serde(rename = "IF")]
        condition: BoolLogicNode,
        #[serde(rename = "THEN")]
        then_value: Value,
        #[serde(rename = "ELSE")]
        else_value: Box<ValueLogicElse>,
    },
    Match {
        #[serde(rename = "MATCH")]
        path: String,
        #[serde(rename = "CASES")]
        cases: HashMap<String, Value>,
        #[serde(rename = "DEFAULT")]
        default: Value,
    },
}

/// The ELSE branch: either a nested ValueLogicNode (for elif chains) or a literal value.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(untagged)]
#[derive(TS)]
pub enum ValueLogicElse {
    /// Nested ValueLogicNode — tried first during deserialization
    Nested(ValueLogicNode),
    /// Literal JSON value — fallback
    Literal(Value),
}

impl ValueLogicNode {
    /// Evaluate this expression against a ShadowState.
    ///
    /// Returns the selected JSON value.
    pub(crate) fn evaluate(&self, shadow: &ShadowState) -> Value {
        self.evaluate_value(shadow.root())
    }

    /// Evaluate this expression against a raw ValueRepr tree.
    fn evaluate_value(&self, state: &crate::shadow::ValueRepr) -> Value {
        match self {
            ValueLogicNode::IfThenElse {
                condition,
                then_value,
                else_value,
            } => {
                if condition.evaluate_value(state) {
                    then_value.clone()
                } else {
                    match else_value.as_ref() {
                        ValueLogicElse::Nested(nested) => nested.evaluate_value(state),
                        ValueLogicElse::Literal(val) => val.clone(),
                    }
                }
            }
            ValueLogicNode::Match {
                path,
                cases,
                default,
            } => {
                let key = match get_path_value(state, path) {
                    Some(val) => value_repr_to_case_key(val),
                    None => None,
                };

                match key {
                    Some(k) => cases.get(&k).cloned().unwrap_or_else(|| default.clone()),
                    None => default.clone(),
                }
            }
        }
    }

    /// Extract all input path strings from the tree recursively.
    pub(crate) fn extract_paths(&self) -> Vec<String> {
        let mut paths = Vec::new();
        self.collect_paths(&mut paths);
        paths
    }

    fn collect_paths(&self, out: &mut Vec<String>) {
        match self {
            ValueLogicNode::IfThenElse {
                condition,
                else_value,
                ..
            } => {
                // Collect paths from the BoolLogic condition
                out.extend(condition.extract_paths());
                // Recurse into nested ELSE
                if let ValueLogicElse::Nested(nested) = else_value.as_ref() {
                    nested.collect_paths(out);
                }
            }
            ValueLogicNode::Match { path, .. } => {
                out.push(path.clone());
            }
        }
    }
}

/// Convert a ValueRepr to a string key for CASES lookup.
///
/// Handles string, number (integer representation), and boolean values.
/// Returns None for null, arrays, objects.
fn value_repr_to_case_key(value: &crate::shadow::ValueRepr) -> Option<String> {
    use crate::shadow::ValueRepr;
    match value {
        ValueRepr::String(s) => Some(s.clone()),
        ValueRepr::Number(n) => {
            // Use integer representation when possible (2.0 → "2")
            if n.fract() == 0.0 && *n >= (i64::MIN as f64) && *n <= (i64::MAX as f64) {
                Some((*n as i64).to_string())
            } else {
                Some(n.to_string())
            }
        }
        ValueRepr::Bool(b) => Some(b.to_string()),
        ValueRepr::Null | ValueRepr::Array(_) | ValueRepr::Object(_) => None,
    }
}

// ---------------------------------------------------------------------------
// ValueLogicRegistry
// ---------------------------------------------------------------------------

/// Metadata stored per registered ValueLogic expression.
pub(crate) struct ValueLogicMetadata {
    pub output_path: String,
    pub tree: ValueLogicNode,
}

/// Registry of ValueLogic expressions keyed by sequential u32 IDs.
pub(crate) struct ValueLogicRegistry {
    logics: HashMap<u32, ValueLogicMetadata>,
    next_id: u32,
}

impl ValueLogicRegistry {
    pub(crate) fn new() -> Self {
        Self {
            logics: HashMap::new(),
            next_id: 0,
        }
    }

    /// Register a new ValueLogic expression.
    ///
    /// Also updates the reverse dependency index with the extracted input paths.
    /// Returns the assigned logic_id.
    pub(crate) fn register(
        &mut self,
        output_path: String,
        tree: ValueLogicNode,
        intern: &mut InternTable,
        rev_index: &mut ReverseDependencyIndex,
    ) -> u32 {
        let logic_id = self.next_id;
        self.next_id += 1;

        // Extract input paths and intern them for the reverse index
        let input_paths = tree.extract_paths();
        let mut interned_ids = HashSet::with_capacity(input_paths.len());
        for path in &input_paths {
            let path_id = intern.intern(path);
            interned_ids.insert(path_id);
        }

        // Update reverse index
        rev_index.add(logic_id, &interned_ids);

        // Store metadata
        self.logics
            .insert(logic_id, ValueLogicMetadata { output_path, tree });

        logic_id
    }

    /// Unregister a ValueLogic expression and clean up reverse index entries.
    #[allow(dead_code)]
    pub(crate) fn unregister(&mut self, logic_id: u32, rev_index: &mut ReverseDependencyIndex) {
        if self.logics.remove(&logic_id).is_some() {
            rev_index.remove(logic_id);
        }
    }

    /// Get metadata for a given logic_id.
    pub(crate) fn get(&self, logic_id: u32) -> Option<&ValueLogicMetadata> {
        self.logics.get(&logic_id)
    }

    /// Number of registered expressions.
    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.logics.len()
    }

    /// Dump all registered entries as (id, output_path) pairs (debug only).
    pub(crate) fn dump_infos(&self) -> Vec<(u32, String)> {
        self.logics
            .iter()
            .map(|(&id, meta)| (id, meta.output_path.clone()))
            .collect()
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shadow::ValueRepr;
    use serde_json::json;
    use std::collections::HashMap;

    // ── Helpers ──────────────────────────────────────────────

    fn make_user_state(role: &str, age: f64) -> ValueRepr {
        let mut user = HashMap::new();
        user.insert("role".to_string(), ValueRepr::String(role.to_string()));
        user.insert("age".to_string(), ValueRepr::Number(age));
        user.insert(
            "email".to_string(),
            ValueRepr::String("test@example.com".to_string()),
        );

        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));
        ValueRepr::Object(root)
    }

    fn make_priority_state(priority: f64) -> ValueRepr {
        let mut item = HashMap::new();
        item.insert("priority".to_string(), ValueRepr::Number(priority));

        let mut root = HashMap::new();
        root.insert("item".to_string(), ValueRepr::Object(item));
        ValueRepr::Object(root)
    }

    fn make_bool_state(flag: bool) -> ValueRepr {
        let mut settings = HashMap::new();
        settings.insert("active".to_string(), ValueRepr::Bool(flag));

        let mut root = HashMap::new();
        root.insert("settings".to_string(), ValueRepr::Object(settings));
        ValueRepr::Object(root)
    }

    fn make_null_role_state() -> ValueRepr {
        let mut user = HashMap::new();
        user.insert("role".to_string(), ValueRepr::Null);

        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));
        ValueRepr::Object(root)
    }

    // ── Serde round-trip ────────────────────────────────────

    #[test]
    fn test_serde_if_then_else_simple() {
        let json_str =
            r#"{"IF":{"IS_EQUAL":["user.role","admin"]},"THEN":["create","read"],"ELSE":["read"]}"#;
        let node: ValueLogicNode = serde_json::from_str(json_str).unwrap();
        let reserialized = serde_json::to_string(&node).unwrap();
        let reparsed: ValueLogicNode = serde_json::from_str(&reserialized).unwrap();
        assert_eq!(node, reparsed);
    }

    #[test]
    fn test_serde_nested_else() {
        let json_str = r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": "Full Access",
            "ELSE": {
                "IF": {"IS_EQUAL": ["user.role", "editor"]},
                "THEN": "Edit Access",
                "ELSE": "Read Only"
            }
        }"#;
        let node: ValueLogicNode = serde_json::from_str(json_str).unwrap();
        // Verify it parsed as IfThenElse with nested ELSE
        match &node {
            ValueLogicNode::IfThenElse { else_value, .. } => {
                assert!(matches!(else_value.as_ref(), ValueLogicElse::Nested(_)));
            }
            _ => panic!("Expected IfThenElse"),
        }
    }

    #[test]
    fn test_serde_match() {
        let json_str = r#"{
            "MATCH": "user.role",
            "CASES": {"admin": [1,2,3], "editor": [1,2]},
            "DEFAULT": [1]
        }"#;
        let node: ValueLogicNode = serde_json::from_str(json_str).unwrap();
        match &node {
            ValueLogicNode::Match {
                path,
                cases,
                default,
            } => {
                assert_eq!(path, "user.role");
                assert_eq!(cases.len(), 2);
                assert_eq!(default, &json!([1]));
            }
            _ => panic!("Expected Match"),
        }
    }

    // ── Evaluation: IF/THEN/ELSE ────────────────────────────

    #[test]
    fn test_eval_if_true_returns_then() {
        let state = make_user_state("admin", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": ["create", "read", "update", "delete"],
            "ELSE": ["read"]
        }"#,
        )
        .unwrap();
        assert_eq!(
            node.evaluate_value(&state),
            json!(["create", "read", "update", "delete"])
        );
    }

    #[test]
    fn test_eval_if_false_returns_else() {
        let state = make_user_state("viewer", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": ["create", "read", "update", "delete"],
            "ELSE": ["read"]
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!(["read"]));
    }

    #[test]
    fn test_eval_elif_chain_middle_match() {
        // admin → Full, editor → Edit, else → Read
        let state = make_user_state("editor", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": "Full Access",
            "ELSE": {
                "IF": {"IS_EQUAL": ["user.role", "editor"]},
                "THEN": "Edit Access",
                "ELSE": "Read Only"
            }
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("Edit Access"));
    }

    #[test]
    fn test_eval_elif_chain_fallthrough() {
        let state = make_user_state("intern", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": "Full Access",
            "ELSE": {
                "IF": {"IS_EQUAL": ["user.role", "editor"]},
                "THEN": "Edit Access",
                "ELSE": "Read Only"
            }
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("Read Only"));
    }

    #[test]
    fn test_eval_complex_condition_with_and_or() {
        // IF (role == admin AND age > 21) THEN "senior_admin" ELSE "regular"
        let state = make_user_state("admin", 25.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"AND": [
                {"IS_EQUAL": ["user.role", "admin"]},
                {"GT": ["user.age", 21]}
            ]},
            "THEN": "senior_admin",
            "ELSE": "regular"
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("senior_admin"));

        // Same condition, age = 18 → false
        let state2 = make_user_state("admin", 18.0);
        assert_eq!(node.evaluate_value(&state2), json!("regular"));
    }

    #[test]
    fn test_eval_object_values() {
        // THEN/ELSE return full objects, not just primitives
        let state = make_user_state("admin", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": {"label": "Admin Panel", "icon": "shield", "permissions": ["all"]},
            "ELSE": {"label": "Dashboard", "icon": "home", "permissions": ["read"]}
        }"#,
        )
        .unwrap();
        assert_eq!(
            node.evaluate_value(&state),
            json!({"label": "Admin Panel", "icon": "shield", "permissions": ["all"]})
        );
    }

    #[test]
    fn test_eval_deeply_nested_elif() {
        // 4-level chain: admin → Full, editor → Edit, moderator → Moderate, else → Read
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": "Full",
            "ELSE": {
                "IF": {"IS_EQUAL": ["user.role", "editor"]},
                "THEN": "Edit",
                "ELSE": {
                    "IF": {"IS_EQUAL": ["user.role", "moderator"]},
                    "THEN": "Moderate",
                    "ELSE": "Read"
                }
            }
        }"#,
        )
        .unwrap();

        assert_eq!(
            node.evaluate_value(&make_user_state("admin", 30.0)),
            json!("Full")
        );
        assert_eq!(
            node.evaluate_value(&make_user_state("editor", 30.0)),
            json!("Edit")
        );
        assert_eq!(
            node.evaluate_value(&make_user_state("moderator", 30.0)),
            json!("Moderate")
        );
        assert_eq!(
            node.evaluate_value(&make_user_state("viewer", 30.0)),
            json!("Read")
        );
    }

    // ── Evaluation: MATCH ───────────────────────────────────

    #[test]
    fn test_eval_match_hit() {
        let state = make_user_state("editor", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "user.role",
            "CASES": {
                "admin": ["create", "read", "update", "delete"],
                "editor": ["read", "update"],
                "viewer": ["read"]
            },
            "DEFAULT": []
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!(["read", "update"]));
    }

    #[test]
    fn test_eval_match_miss_returns_default() {
        let state = make_user_state("intern", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "user.role",
            "CASES": {
                "admin": ["create", "read", "update", "delete"],
                "editor": ["read", "update"]
            },
            "DEFAULT": []
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!([]));
    }

    #[test]
    fn test_eval_match_null_path_returns_default() {
        let state = make_null_role_state();
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "user.role",
            "CASES": {"admin": "yes"},
            "DEFAULT": "no"
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("no"));
    }

    #[test]
    fn test_eval_match_numeric_key() {
        let state = make_priority_state(2.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "item.priority",
            "CASES": {"1": "low", "2": "medium", "3": "high"},
            "DEFAULT": "unknown"
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("medium"));
    }

    #[test]
    fn test_eval_match_boolean_key() {
        let state = make_bool_state(true);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "settings.active",
            "CASES": {"true": "enabled", "false": "disabled"},
            "DEFAULT": "unknown"
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("enabled"));
    }

    #[test]
    fn test_eval_match_missing_path_returns_default() {
        let state = make_user_state("admin", 30.0);
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "user.nonexistent",
            "CASES": {"x": "found"},
            "DEFAULT": "not_found"
        }"#,
        )
        .unwrap();
        assert_eq!(node.evaluate_value(&state), json!("not_found"));
    }

    // ── Path extraction ─────────────────────────────────────

    #[test]
    fn test_extract_paths_if_then_else() {
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"IS_EQUAL": ["user.role", "admin"]},
            "THEN": "yes",
            "ELSE": "no"
        }"#,
        )
        .unwrap();
        assert_eq!(node.extract_paths(), vec!["user.role".to_string()]);
    }

    #[test]
    fn test_extract_paths_nested_elif() {
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"AND": [{"IS_EQUAL": ["a.b", "x"]}, {"EXISTS": "c.d"}]},
            "THEN": "yes",
            "ELSE": {
                "IF": {"GT": ["e.f", 5]},
                "THEN": "maybe",
                "ELSE": "no"
            }
        }"#,
        )
        .unwrap();
        let paths = node.extract_paths();
        assert_eq!(
            paths,
            vec!["a.b".to_string(), "c.d".to_string(), "e.f".to_string(),]
        );
    }

    #[test]
    fn test_extract_paths_match() {
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "MATCH": "user.role",
            "CASES": {"admin": "yes"},
            "DEFAULT": "no"
        }"#,
        )
        .unwrap();
        assert_eq!(node.extract_paths(), vec!["user.role".to_string()]);
    }

    #[test]
    fn test_extract_paths_complex_condition() {
        // IF { AND: [IS_EQUAL a.x, EXISTS b.y, GT c.z 5] } THEN ... ELSE { MATCH d.w ... }
        let node: ValueLogicNode = serde_json::from_str(
            r#"{
            "IF": {"AND": [
                {"IS_EQUAL": ["a.x", "v"]},
                {"EXISTS": "b.y"},
                {"GT": ["c.z", 5]}
            ]},
            "THEN": "if_branch",
            "ELSE": {
                "MATCH": "d.w",
                "CASES": {"k": "v"},
                "DEFAULT": "def"
            }
        }"#,
        )
        .unwrap();
        let paths = node.extract_paths();
        assert_eq!(
            paths,
            vec![
                "a.x".to_string(),
                "b.y".to_string(),
                "c.z".to_string(),
                "d.w".to_string(),
            ]
        );
    }

    // ── Registry ────────────────────────────────────────────

    #[test]
    fn test_registry_register_and_get() {
        let mut registry = ValueLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree: ValueLogicNode = serde_json::from_str(
            r#"{"IF":{"IS_EQUAL":["user.role","admin"]},"THEN":"yes","ELSE":"no"}"#,
        )
        .unwrap();
        let id = registry.register("out.options".into(), tree, &mut intern, &mut rev);

        assert_eq!(id, 0);
        assert_eq!(registry.len(), 1);

        let meta = registry.get(id).unwrap();
        assert_eq!(meta.output_path, "out.options");
    }

    #[test]
    fn test_registry_sequential_ids() {
        let mut registry = ValueLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree = |path: &str| -> ValueLogicNode {
            serde_json::from_str(&format!(
                r#"{{"MATCH":"{}","CASES":{{"x":"y"}},"DEFAULT":"z"}}"#,
                path
            ))
            .unwrap()
        };

        let id0 = registry.register("out0".into(), tree("a.b"), &mut intern, &mut rev);
        let id1 = registry.register("out1".into(), tree("c.d"), &mut intern, &mut rev);
        let id2 = registry.register("out2".into(), tree("e.f"), &mut intern, &mut rev);

        assert_eq!(id0, 0);
        assert_eq!(id1, 1);
        assert_eq!(id2, 2);
        assert_eq!(registry.len(), 3);
    }

    #[test]
    fn test_registry_unregister_cleans_rev_index() {
        let mut registry = ValueLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree: ValueLogicNode = serde_json::from_str(
            r#"{"IF":{"IS_EQUAL":["user.role","admin"]},"THEN":"yes","ELSE":"no"}"#,
        )
        .unwrap();
        let id = registry.register("out".into(), tree, &mut intern, &mut rev);

        let path_id = intern.intern("user.role");
        assert_eq!(rev.affected_by_path(path_id), vec![id]);

        registry.unregister(id, &mut rev);
        assert_eq!(registry.len(), 0);
        assert!(rev.affected_by_path(path_id).is_empty());
    }

    #[test]
    fn test_registry_reverse_index_affected_by_path() {
        let mut registry = ValueLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        // Logic 0 depends on user.role
        let tree0: ValueLogicNode = serde_json::from_str(
            r#"{"IF":{"IS_EQUAL":["user.role","admin"]},"THEN":"a","ELSE":"b"}"#,
        )
        .unwrap();
        let id0 = registry.register("out0".into(), tree0, &mut intern, &mut rev);

        // Logic 1 also depends on user.role (MATCH)
        let tree1: ValueLogicNode =
            serde_json::from_str(r#"{"MATCH":"user.role","CASES":{"admin":"x"},"DEFAULT":"y"}"#)
                .unwrap();
        let id1 = registry.register("out1".into(), tree1, &mut intern, &mut rev);

        let path_id = intern.intern("user.role");
        let mut affected = rev.affected_by_path(path_id);
        affected.sort();
        assert_eq!(affected, vec![id0, id1]);
    }

    #[test]
    fn test_unregister_nonexistent_is_noop() {
        let mut registry = ValueLogicRegistry::new();
        let mut rev = ReverseDependencyIndex::new();
        registry.unregister(999, &mut rev); // should not panic
        assert_eq!(registry.len(), 0);
    }
}
