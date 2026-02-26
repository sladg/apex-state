//! Boolean logic DSL for conditional expressions with reverse dependency tracking
//!
//! This module provides:
//! - `BoolLogicNode`: An enum that deserializes JS boolean logic expressions directly
//!   using serde tuple variants, enabling high-performance WASM-based condition checking.
//! - `BoolLogicRegistry`: Stores registered BoolLogic expressions with metadata.
//!
//! Two JSON formats are accepted:
//! - Named operators: `{ "IS_EQUAL": ["path", value] }` (externally tagged)
//! - Shorthand (IS_EQUAL only): `["path", value]` — 2-element array, normalized to IsEqual

use crate::intern::InternTable;
use crate::rev_index::ReverseDependencyIndex;
use crate::shadow::{ShadowState, ValueRepr};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashMap, HashSet};

use ts_rs::TS;

// ---------------------------------------------------------------------------
// BoolLogicNode
// ---------------------------------------------------------------------------

/// Boolean logic DSL for conditional expressions.
///
/// Supports declarative condition checking against state paths.
/// Accepts two JSON formats (see module-level docs for details).
#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
#[derive(TS)]
pub enum BoolLogicNode {
    IsEqual(String, Value),
    Exists(String),
    IsEmpty(String),
    And(Vec<BoolLogicNode>),
    Or(Vec<BoolLogicNode>),
    Not(Box<BoolLogicNode>),
    Gt(String, f64),
    Lt(String, f64),
    Gte(String, f64),
    Lte(String, f64),
    In(String, Vec<Value>),
    ContainsAny(String, Vec<Value>),
    ContainsAll(String, Vec<Value>),
}

/// Private helper that derives Deserialize for the named-operator format.
/// Used by BoolLogicNode's custom Deserialize to handle the object branch.
#[derive(Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum BoolLogicNodeHelper {
    IsEqual(String, Value),
    Exists(String),
    IsEmpty(String),
    And(Vec<BoolLogicNode>),
    Or(Vec<BoolLogicNode>),
    Not(Box<BoolLogicNode>),
    Gt(String, f64),
    Lt(String, f64),
    Gte(String, f64),
    Lte(String, f64),
    In(String, Vec<Value>),
    ContainsAny(String, Vec<Value>),
    ContainsAll(String, Vec<Value>),
}

impl BoolLogicNodeHelper {
    fn into_node(self) -> BoolLogicNode {
        match self {
            Self::IsEqual(p, v) => BoolLogicNode::IsEqual(p, v),
            Self::Exists(p) => BoolLogicNode::Exists(p),
            Self::IsEmpty(p) => BoolLogicNode::IsEmpty(p),
            Self::And(c) => BoolLogicNode::And(c),
            Self::Or(c) => BoolLogicNode::Or(c),
            Self::Not(c) => BoolLogicNode::Not(c),
            Self::Gt(p, t) => BoolLogicNode::Gt(p, t),
            Self::Lt(p, t) => BoolLogicNode::Lt(p, t),
            Self::Gte(p, t) => BoolLogicNode::Gte(p, t),
            Self::Lte(p, t) => BoolLogicNode::Lte(p, t),
            Self::In(p, a) => BoolLogicNode::In(p, a),
            Self::ContainsAny(p, a) => BoolLogicNode::ContainsAny(p, a),
            Self::ContainsAll(p, a) => BoolLogicNode::ContainsAll(p, a),
        }
    }
}

impl<'de> Deserialize<'de> for BoolLogicNode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = Value::deserialize(deserializer)?;
        match raw {
            // Shorthand: ["path", value] — normalized to IsEqual
            Value::Array(ref arr) if arr.len() == 2 => {
                if let Value::String(ref path) = arr[0] {
                    Ok(BoolLogicNode::IsEqual(path.clone(), arr[1].clone()))
                } else {
                    Err(serde::de::Error::custom(
                        "shorthand first element must be a string path",
                    ))
                }
            }
            // Named operators: {"IS_EQUAL": [...], ...}
            Value::Object(_) => {
                let helper: BoolLogicNodeHelper =
                    serde_json::from_value(raw).map_err(serde::de::Error::custom)?;
                Ok(helper.into_node())
            }
            _ => Err(serde::de::Error::custom(
                "BoolLogicNode must be an object or a 2-element array shorthand",
            )),
        }
    }
}

impl BoolLogicNode {
    /// Evaluate this expression against a ShadowState.
    ///
    /// Returns `true` if the condition is satisfied.
    pub(crate) fn evaluate(&self, shadow: &ShadowState) -> bool {
        self.evaluate_value(shadow.root())
    }

    /// Evaluate this expression against a raw ValueRepr tree.
    ///
    /// Used internally and for testing without a full ShadowState.
    pub(crate) fn evaluate_value(&self, state: &ValueRepr) -> bool {
        match self {
            BoolLogicNode::IsEqual(path, expected) => match get_path_value(state, path) {
                Some(value) => value_repr_to_json(value) == *expected,
                None => expected.is_null(),
            },

            BoolLogicNode::Exists(path) => match get_path_value(state, path) {
                Some(value) => !matches!(value, ValueRepr::Null),
                None => false,
            },

            BoolLogicNode::IsEmpty(path) => match get_path_value(state, path) {
                Some(value) => is_empty(value),
                None => true,
            },

            BoolLogicNode::And(conditions) => conditions.iter().all(|c| c.evaluate_value(state)),
            BoolLogicNode::Or(conditions) => conditions.iter().any(|c| c.evaluate_value(state)),
            BoolLogicNode::Not(condition) => !condition.evaluate_value(state),

            BoolLogicNode::Gt(path, threshold) => {
                resolve_numeric(state, path).is_some_and(|n| n > *threshold)
            }
            BoolLogicNode::Lt(path, threshold) => {
                resolve_numeric(state, path).is_some_and(|n| n < *threshold)
            }
            BoolLogicNode::Gte(path, threshold) => {
                resolve_numeric(state, path).is_some_and(|n| n >= *threshold)
            }
            BoolLogicNode::Lte(path, threshold) => {
                resolve_numeric(state, path).is_some_and(|n| n <= *threshold)
            }

            BoolLogicNode::In(path, allowed) => match get_path_value(state, path) {
                Some(value) => allowed.contains(&value_repr_to_json(value)),
                None => false,
            },

            BoolLogicNode::ContainsAny(path, elements) => match get_path_value(state, path) {
                Some(ValueRepr::Array(arr)) => elements
                    .iter()
                    .any(|el| arr.iter().any(|item| value_repr_to_json(item) == *el)),
                _ => false,
            },

            BoolLogicNode::ContainsAll(path, elements) => match get_path_value(state, path) {
                Some(ValueRepr::Array(arr)) => elements
                    .iter()
                    .all(|el| arr.iter().any(|item| value_repr_to_json(item) == *el)),
                _ => false,
            },
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
            BoolLogicNode::IsEqual(path, _)
            | BoolLogicNode::Exists(path)
            | BoolLogicNode::IsEmpty(path)
            | BoolLogicNode::Gt(path, _)
            | BoolLogicNode::Lt(path, _)
            | BoolLogicNode::Gte(path, _)
            | BoolLogicNode::Lte(path, _) => {
                out.push(path.clone());
            }
            BoolLogicNode::In(path, _)
            | BoolLogicNode::ContainsAny(path, _)
            | BoolLogicNode::ContainsAll(path, _) => {
                out.push(path.clone());
            }
            BoolLogicNode::And(children) | BoolLogicNode::Or(children) => {
                for child in children {
                    child.collect_paths(out);
                }
            }
            BoolLogicNode::Not(child) => {
                child.collect_paths(out);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Path resolution helpers
// ---------------------------------------------------------------------------

/// Resolve a dot-notation path in a ValueRepr tree.
pub(crate) fn get_path_value<'a>(state: &'a ValueRepr, path: &str) -> Option<&'a ValueRepr> {
    let mut current = state;
    for seg in path.split('.') {
        match current {
            ValueRepr::Object(map) => {
                current = map.get(seg)?;
            }
            ValueRepr::Array(arr) => {
                let idx: usize = seg.parse().ok()?;
                current = arr.get(idx)?;
            }
            _ => return None,
        }
    }
    Some(current)
}

/// Resolve a path to a numeric value, supporting virtual `.length` on arrays.
///
/// Handles two cases:
/// - Regular number paths: `user.age` → the f64 value
/// - Array length paths: `items.length` → array length as f64
fn resolve_numeric(state: &ValueRepr, path: &str) -> Option<f64> {
    if let Some(parent_path) = path.strip_suffix(".length") {
        let parent = get_path_value(state, parent_path)?;
        if let ValueRepr::Array(arr) = parent {
            return Some(arr.len() as f64);
        }
        return None;
    }
    match get_path_value(state, path)? {
        ValueRepr::Number(n) => Some(*n),
        _ => None,
    }
}

/// Check if a ValueRepr is considered "empty".
///
/// Empty: Null, empty string, empty array, empty object.
/// Non-empty: everything else (including 0, false).
fn is_empty(value: &ValueRepr) -> bool {
    match value {
        ValueRepr::Null => true,
        ValueRepr::String(s) => s.is_empty(),
        ValueRepr::Array(arr) => arr.is_empty(),
        ValueRepr::Object(obj) => obj.is_empty(),
        _ => false,
    }
}

/// Convert ValueRepr to serde_json::Value for comparison.
///
/// For numbers, prefers integer representation when the f64 is a whole number.
/// This ensures `ValueRepr::Number(25.0)` equals `json!(25)`.
fn value_repr_to_json(value: &ValueRepr) -> Value {
    match value {
        ValueRepr::Null => Value::Null,
        ValueRepr::Bool(b) => Value::Bool(*b),
        ValueRepr::Number(n) => {
            let n = *n;
            // If it's a whole number that fits in i64, use integer representation
            // so that comparisons with json!(25) work correctly.
            if n.fract() == 0.0 && n >= (i64::MIN as f64) && n <= (i64::MAX as f64) {
                Value::Number(serde_json::Number::from(n as i64))
            } else {
                serde_json::Number::from_f64(n)
                    .map(Value::Number)
                    .unwrap_or(Value::Null)
            }
        }
        ValueRepr::String(s) => Value::String(s.clone()),
        ValueRepr::Array(arr) => Value::Array(arr.iter().map(value_repr_to_json).collect()),
        ValueRepr::Object(obj) => {
            let map = obj
                .iter()
                .map(|(k, v)| (k.clone(), value_repr_to_json(v)))
                .collect();
            Value::Object(map)
        }
    }
}

// ---------------------------------------------------------------------------
// BoolLogicRegistry
// ---------------------------------------------------------------------------

/// Metadata stored per registered BoolLogic expression.
pub(crate) struct BoolLogicMetadata {
    pub output_path: String,
    pub tree: BoolLogicNode,
    pub anchor_path_id: Option<u32>,
    pub registration_id: Option<String>,
}

/// Registry of BoolLogic expressions keyed by sequential u32 IDs.
pub(crate) struct BoolLogicRegistry {
    logics: HashMap<u32, BoolLogicMetadata>,
    next_id: u32,
}

impl BoolLogicRegistry {
    pub(crate) fn new() -> Self {
        Self {
            logics: HashMap::new(),
            next_id: 0,
        }
    }

    /// Register a new BoolLogic expression.
    ///
    /// Also updates the reverse dependency index with the extracted input paths.
    /// Returns the assigned logic_id.
    pub(crate) fn register(
        &mut self,
        output_path: String,
        tree: BoolLogicNode,
        intern: &mut InternTable,
        rev_index: &mut ReverseDependencyIndex,
        anchor_path_id: Option<u32>,
        registration_id: Option<String>,
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
        self.logics.insert(
            logic_id,
            BoolLogicMetadata {
                output_path,
                tree,
                anchor_path_id,
                registration_id,
            },
        );

        logic_id
    }

    /// Unregister a BoolLogic expression and clean up reverse index entries.
    pub(crate) fn unregister(&mut self, logic_id: u32, rev_index: &mut ReverseDependencyIndex) {
        if self.logics.remove(&logic_id).is_some() {
            rev_index.remove(logic_id);
        }
    }

    /// Get metadata for a given logic_id.
    pub(crate) fn get(&self, logic_id: u32) -> Option<&BoolLogicMetadata> {
        self.logics.get(&logic_id)
    }

    /// Number of registered expressions.
    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.logics.len()
    }

    /// Dump all registered entries as (id, output_path, tree) triples (debug only).
    pub(crate) fn dump_infos(&self) -> Vec<(u32, String, BoolLogicNode)> {
        self.logics
            .iter()
            .map(|(&id, meta)| (id, meta.output_path.clone(), meta.tree.clone()))
            .collect()
    }

    /// Return IDs of all entries with a matching anchor_path_id.
    pub(crate) fn ids_for_anchor(&self, anchor_id: u32) -> Vec<u32> {
        self.logics
            .iter()
            .filter(|(_, meta)| meta.anchor_path_id == Some(anchor_id))
            .map(|(&id, _)| id)
            .collect()
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::collections::HashMap;

    // -----------------------------------------------------------------------
    // Helper: build a simple shadow state for testing
    // -----------------------------------------------------------------------

    fn make_state() -> ValueRepr {
        let mut user = HashMap::new();
        user.insert("role".to_string(), ValueRepr::String("admin".to_string()));
        user.insert("age".to_string(), ValueRepr::Number(25.0));
        user.insert("active".to_string(), ValueRepr::Bool(true));
        user.insert(
            "email".to_string(),
            ValueRepr::String("alice@example.com".to_string()),
        );
        user.insert("score".to_string(), ValueRepr::Number(150.0));
        user.insert(
            "tags".to_string(),
            ValueRepr::Array(vec![ValueRepr::String("premium".to_string())]),
        );
        user.insert("bio".to_string(), ValueRepr::String("".to_string()));
        user.insert("deleted".to_string(), ValueRepr::Null);

        let mut profile = HashMap::new();
        profile.insert("verified".to_string(), ValueRepr::Bool(true));
        profile.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
        user.insert("profile".to_string(), ValueRepr::Object(profile));

        let mut document = HashMap::new();
        document.insert("id".to_string(), ValueRepr::String("doc-456".to_string()));
        document.insert("status".to_string(), ValueRepr::String("draft".to_string()));

        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));
        root.insert("document".to_string(), ValueRepr::Object(document));
        ValueRepr::Object(root)
    }

    // -----------------------------------------------------------------------
    // Serde deserialization tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_deserialize_is_equal_string() {
        let json = json!({"IS_EQUAL": ["user.role", "admin"]});
        let logic: BoolLogicNode = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.role".into(), json!("admin"))
        );
    }

    #[test]
    fn test_deserialize_is_equal_number() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"IS_EQUAL": ["user.age", 25]})).unwrap();
        assert_eq!(logic, BoolLogicNode::IsEqual("user.age".into(), json!(25)));
    }

    #[test]
    fn test_deserialize_is_equal_boolean() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"IS_EQUAL": ["user.active", true]})).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.active".into(), json!(true))
        );
    }

    #[test]
    fn test_deserialize_is_equal_null() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"IS_EQUAL": ["user.deleted", null]})).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.deleted".into(), json!(null))
        );
    }

    #[test]
    fn test_deserialize_exists() {
        let logic: BoolLogicNode = serde_json::from_value(json!({"EXISTS": "user.email"})).unwrap();
        assert_eq!(logic, BoolLogicNode::Exists("user.email".into()));
    }

    #[test]
    fn test_deserialize_is_empty() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"IS_EMPTY": "user.tags"})).unwrap();
        assert_eq!(logic, BoolLogicNode::IsEmpty("user.tags".into()));
    }

    #[test]
    fn test_deserialize_and() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "AND": [
                {"EXISTS": "user.email"},
                {"IS_EQUAL": ["user.role", "admin"]}
            ]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::And(vec![
                BoolLogicNode::Exists("user.email".into()),
                BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            ])
        );
    }

    #[test]
    fn test_deserialize_or() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "OR": [
                {"IS_EQUAL": ["user.role", "admin"]},
                {"IS_EQUAL": ["user.role", "editor"]}
            ]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::Or(vec![
                BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
                BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
            ])
        );
    }

    #[test]
    fn test_deserialize_not() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "NOT": {"IS_EQUAL": ["user.role", "guest"]}
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::Not(Box::new(BoolLogicNode::IsEqual(
                "user.role".into(),
                json!("guest")
            )))
        );
    }

    #[test]
    fn test_deserialize_gt() {
        let logic: BoolLogicNode = serde_json::from_value(json!({"GT": ["user.age", 18]})).unwrap();
        assert_eq!(logic, BoolLogicNode::Gt("user.age".into(), 18.0));
    }

    #[test]
    fn test_deserialize_lt() {
        let logic: BoolLogicNode = serde_json::from_value(json!({"LT": ["user.age", 65]})).unwrap();
        assert_eq!(logic, BoolLogicNode::Lt("user.age".into(), 65.0));
    }

    #[test]
    fn test_deserialize_gte() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"GTE": ["user.score", 100]})).unwrap();
        assert_eq!(logic, BoolLogicNode::Gte("user.score".into(), 100.0));
    }

    #[test]
    fn test_deserialize_lte() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"LTE": ["user.score", 999]})).unwrap();
        assert_eq!(logic, BoolLogicNode::Lte("user.score".into(), 999.0));
    }

    #[test]
    fn test_deserialize_in() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "IN": ["user.role", ["admin", "editor", "moderator"]]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::In(
                "user.role".into(),
                vec![json!("admin"), json!("editor"), json!("moderator")],
            )
        );
    }

    #[test]
    fn test_deserialize_nested_and() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "AND": [
                {"EXISTS": "user.id"},
                {"AND": [
                    {"IS_EQUAL": ["user.role", "admin"]},
                    {"IS_EQUAL": ["user.active", true]}
                ]}
            ]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::And(vec![
                BoolLogicNode::Exists("user.id".into()),
                BoolLogicNode::And(vec![
                    BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
                    BoolLogicNode::IsEqual("user.active".into(), json!(true)),
                ]),
            ])
        );
    }

    #[test]
    fn test_invalid_operator_returns_error() {
        let result: Result<BoolLogicNode, _> =
            serde_json::from_value(json!({"INVALID": ["x", "y"]}));
        assert!(result.is_err());
    }

    #[test]
    fn test_is_equal_wrong_args_returns_error() {
        let result: Result<BoolLogicNode, _> =
            serde_json::from_value(json!({"IS_EQUAL": ["only_one"]}));
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Serialization roundtrip tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_serialize_is_equal() {
        let logic = BoolLogicNode::IsEqual("user.role".into(), json!("admin"));
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"IS_EQUAL": ["user.role", "admin"]})
        );
    }

    #[test]
    fn test_serialize_exists() {
        let logic = BoolLogicNode::Exists("user.email".into());
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"EXISTS": "user.email"})
        );
    }

    #[test]
    fn test_serialize_and() {
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::Exists("user.email".into()),
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
        ]);
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({
                "AND": [
                    {"EXISTS": "user.email"},
                    {"IS_EQUAL": ["user.role", "admin"]}
                ]
            })
        );
    }

    #[test]
    fn test_serialize_or() {
        let logic = BoolLogicNode::Or(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
        ]);
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({
                "OR": [
                    {"IS_EQUAL": ["user.role", "admin"]},
                    {"IS_EQUAL": ["user.role", "editor"]}
                ]
            })
        );
    }

    #[test]
    fn test_serialize_not() {
        let logic = BoolLogicNode::Not(Box::new(BoolLogicNode::IsEqual(
            "user.role".into(),
            json!("guest"),
        )));
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"NOT": {"IS_EQUAL": ["user.role", "guest"]}})
        );
    }

    #[test]
    fn test_serialize_gt() {
        let logic = BoolLogicNode::Gt("user.age".into(), 18.0);
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"GT": ["user.age", 18.0]})
        );
    }

    #[test]
    fn test_serialize_is_empty() {
        let logic = BoolLogicNode::IsEmpty("user.tags".into());
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"IS_EMPTY": "user.tags"})
        );
    }

    #[test]
    fn test_serialize_in() {
        let logic = BoolLogicNode::In("user.role".into(), vec![json!("admin"), json!("editor")]);
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"IN": ["user.role", ["admin", "editor"]]})
        );
    }

    #[test]
    fn test_roundtrip_complex() {
        let original = json!({
            "AND": [
                {"IS_EQUAL": ["user.role", "admin"]},
                {"EXISTS": "user.email"},
                {"AND": [
                    {"IS_EQUAL": ["user.active", true]},
                    {"IS_EQUAL": ["user.verified", true]}
                ]}
            ]
        });
        let logic: BoolLogicNode = serde_json::from_value(original.clone()).unwrap();
        assert_eq!(serde_json::to_value(&logic).unwrap(), original);
    }

    // -----------------------------------------------------------------------
    // Evaluation tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_eval_is_equal_match() {
        let state = make_state();
        assert!(BoolLogicNode::IsEqual("user.role".into(), json!("admin")).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_equal_no_match() {
        let state = make_state();
        assert!(!BoolLogicNode::IsEqual("user.role".into(), json!("editor")).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_equal_number() {
        let state = make_state();
        assert!(BoolLogicNode::IsEqual("user.age".into(), json!(25)).evaluate_value(&state));
        assert!(!BoolLogicNode::IsEqual("user.age".into(), json!(30)).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_equal_bool() {
        let state = make_state();
        assert!(BoolLogicNode::IsEqual("user.active".into(), json!(true)).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_equal_null() {
        let state = make_state();
        assert!(BoolLogicNode::IsEqual("user.deleted".into(), json!(null)).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_equal_missing_path_equals_null() {
        let state = make_state();
        assert!(BoolLogicNode::IsEqual("user.missing".into(), json!(null)).evaluate_value(&state));
        assert!(!BoolLogicNode::IsEqual("user.missing".into(), json!("x")).evaluate_value(&state));
    }

    #[test]
    fn test_eval_exists_present() {
        let state = make_state();
        assert!(BoolLogicNode::Exists("user.email".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_exists_null() {
        let state = make_state();
        assert!(!BoolLogicNode::Exists("user.deleted".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_exists_missing() {
        let state = make_state();
        assert!(!BoolLogicNode::Exists("user.nonexistent".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_exists_empty_string_is_true() {
        let state = make_state();
        assert!(BoolLogicNode::Exists("user.bio".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_null() {
        let state = make_state();
        assert!(BoolLogicNode::IsEmpty("user.deleted".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_empty_string() {
        let state = make_state();
        assert!(BoolLogicNode::IsEmpty("user.bio".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_non_empty_string() {
        let state = make_state();
        assert!(!BoolLogicNode::IsEmpty("user.email".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_missing_path() {
        let state = make_state();
        assert!(BoolLogicNode::IsEmpty("user.nonexistent".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_number_not_empty() {
        let state = make_state();
        assert!(!BoolLogicNode::IsEmpty("user.age".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_is_empty_bool_not_empty() {
        let state = make_state();
        assert!(!BoolLogicNode::IsEmpty("user.active".into()).evaluate_value(&state));
    }

    #[test]
    fn test_eval_and_all_true() {
        let state = make_state();
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            BoolLogicNode::Exists("user.email".into()),
        ]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_and_one_false() {
        let state = make_state();
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
            BoolLogicNode::Exists("user.email".into()),
        ]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_and_empty_is_true() {
        let state = make_state();
        assert!(BoolLogicNode::And(vec![]).evaluate_value(&state));
    }

    #[test]
    fn test_eval_or_first_true() {
        let state = make_state();
        let logic = BoolLogicNode::Or(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
        ]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_or_all_false() {
        let state = make_state();
        let logic = BoolLogicNode::Or(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
            BoolLogicNode::IsEqual("user.role".into(), json!("viewer")),
        ]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_or_empty_is_false() {
        let state = make_state();
        assert!(!BoolLogicNode::Or(vec![]).evaluate_value(&state));
    }

    #[test]
    fn test_eval_not() {
        let state = make_state();
        let logic = BoolLogicNode::Not(Box::new(BoolLogicNode::IsEqual(
            "user.role".into(),
            json!("guest"),
        )));
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_not_double_negation() {
        let state = make_state();
        let logic = BoolLogicNode::Not(Box::new(BoolLogicNode::Not(Box::new(
            BoolLogicNode::IsEqual("user.active".into(), json!(true)),
        ))));
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_gt() {
        let state = make_state();
        assert!(BoolLogicNode::Gt("user.age".into(), 18.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Gt("user.age".into(), 25.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Gt("user.age".into(), 30.0).evaluate_value(&state));
    }

    #[test]
    fn test_eval_lt() {
        let state = make_state();
        assert!(BoolLogicNode::Lt("user.age".into(), 30.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Lt("user.age".into(), 25.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Lt("user.age".into(), 20.0).evaluate_value(&state));
    }

    #[test]
    fn test_eval_gte() {
        let state = make_state();
        assert!(BoolLogicNode::Gte("user.age".into(), 25.0).evaluate_value(&state));
        assert!(BoolLogicNode::Gte("user.age".into(), 24.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Gte("user.age".into(), 26.0).evaluate_value(&state));
    }

    #[test]
    fn test_eval_lte() {
        let state = make_state();
        assert!(BoolLogicNode::Lte("user.age".into(), 25.0).evaluate_value(&state));
        assert!(BoolLogicNode::Lte("user.age".into(), 26.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Lte("user.age".into(), 24.0).evaluate_value(&state));
    }

    #[test]
    fn test_eval_numeric_non_number_returns_false() {
        let state = make_state();
        assert!(!BoolLogicNode::Gt("user.role".into(), 0.0).evaluate_value(&state));
        assert!(!BoolLogicNode::Lt("user.missing".into(), 0.0).evaluate_value(&state));
    }

    #[test]
    fn test_eval_in_match() {
        let state = make_state();
        let logic = BoolLogicNode::In("user.role".into(), vec![json!("admin"), json!("editor")]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_in_no_match() {
        let state = make_state();
        let logic = BoolLogicNode::In("user.role".into(), vec![json!("editor"), json!("viewer")]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_in_empty_list() {
        let state = make_state();
        assert!(!BoolLogicNode::In("user.role".into(), vec![]).evaluate_value(&state));
    }

    #[test]
    fn test_eval_in_missing_path() {
        let state = make_state();
        assert!(!BoolLogicNode::In("user.missing".into(), vec![json!("x")]).evaluate_value(&state));
    }

    #[test]
    fn test_eval_in_number() {
        let state = make_state();
        let logic = BoolLogicNode::In("user.age".into(), vec![json!(25), json!(30)]);
        assert!(logic.evaluate_value(&state));
    }

    // -----------------------------------------------------------------------
    // CONTAINS_ANY tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_deserialize_contains_any() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "CONTAINS_ANY": ["user.tags", ["premium", "vip"]]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::ContainsAny("user.tags".into(), vec![json!("premium"), json!("vip")])
        );
    }

    #[test]
    fn test_eval_contains_any_match_first() {
        let state = make_state();
        // user.tags = ["premium"] — "premium" is in candidates
        let logic =
            BoolLogicNode::ContainsAny("user.tags".into(), vec![json!("premium"), json!("vip")]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_any_match_second() {
        let state = make_state();
        // user.tags = ["premium"] — only second candidate matches, first doesn't
        let logic =
            BoolLogicNode::ContainsAny("user.tags".into(), vec![json!("vip"), json!("premium")]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_any_no_match() {
        let state = make_state();
        let logic =
            BoolLogicNode::ContainsAny("user.tags".into(), vec![json!("vip"), json!("free")]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_any_empty_candidates_returns_false() {
        let state = make_state();
        let logic = BoolLogicNode::ContainsAny("user.tags".into(), vec![]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_any_non_array_path_returns_false() {
        let state = make_state();
        // user.role is a string, not an array
        let logic =
            BoolLogicNode::ContainsAny("user.role".into(), vec![json!("admin"), json!("editor")]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_extract_paths_contains_any() {
        let logic =
            BoolLogicNode::ContainsAny("user.tags".into(), vec![json!("premium"), json!("vip")]);
        assert_eq!(logic.extract_paths(), vec!["user.tags".to_string()]);
    }

    // -----------------------------------------------------------------------
    // CONTAINS_ALL tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_deserialize_contains_all() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "CONTAINS_ALL": ["user.tags", ["premium", "verified"]]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::ContainsAll(
                "user.tags".into(),
                vec![json!("premium"), json!("verified")]
            )
        );
    }

    #[test]
    fn test_eval_contains_all_all_present() {
        // Build state with two tags
        let mut user = HashMap::new();
        user.insert(
            "tags".to_string(),
            ValueRepr::Array(vec![
                ValueRepr::String("premium".to_string()),
                ValueRepr::String("verified".to_string()),
            ]),
        );
        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));
        let state = ValueRepr::Object(root);

        let logic = BoolLogicNode::ContainsAll(
            "user.tags".into(),
            vec![json!("premium"), json!("verified")],
        );
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_all_partial_match_returns_false() {
        let state = make_state();
        // user.tags = ["premium"] only — "verified" is missing
        let logic = BoolLogicNode::ContainsAll(
            "user.tags".into(),
            vec![json!("premium"), json!("verified")],
        );
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_all_empty_candidates_returns_true() {
        // vacuously true — all elements of an empty set are present
        let state = make_state();
        let logic = BoolLogicNode::ContainsAll("user.tags".into(), vec![]);
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_contains_all_non_array_path_returns_false() {
        let state = make_state();
        let logic =
            BoolLogicNode::ContainsAll("user.role".into(), vec![json!("admin"), json!("editor")]);
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_extract_paths_contains_all() {
        let logic = BoolLogicNode::ContainsAll(
            "user.tags".into(),
            vec![json!("premium"), json!("verified")],
        );
        assert_eq!(logic.extract_paths(), vec!["user.tags".to_string()]);
    }

    #[test]
    fn test_eval_complex_real_world() {
        let state = make_state();
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::Or(vec![
                BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
                BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
            ]),
            BoolLogicNode::Gte("user.age".into(), 18.0),
            BoolLogicNode::Gt("user.score".into(), 100.0),
            BoolLogicNode::Not(Box::new(BoolLogicNode::IsEmpty("user.tags".into()))),
            BoolLogicNode::In(
                "user.role".into(),
                vec![json!("admin"), json!("editor"), json!("mod")],
            ),
            BoolLogicNode::Exists("document.id".into()),
            BoolLogicNode::IsEqual("user.profile.verified".into(), json!(true)),
        ]);
        assert!(logic.evaluate_value(&state));
    }

    // -----------------------------------------------------------------------
    // extract_paths tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_extract_paths_leaf() {
        let logic = BoolLogicNode::IsEqual("user.role".into(), json!("admin"));
        assert_eq!(logic.extract_paths(), vec!["user.role".to_string()]);
    }

    #[test]
    fn test_extract_paths_exists() {
        assert_eq!(
            BoolLogicNode::Exists("a.b".into()).extract_paths(),
            vec!["a.b".to_string()]
        );
    }

    #[test]
    fn test_extract_paths_and() {
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            BoolLogicNode::Exists("user.email".into()),
        ]);
        let paths = logic.extract_paths();
        assert_eq!(
            paths,
            vec!["user.role".to_string(), "user.email".to_string()]
        );
    }

    #[test]
    fn test_extract_paths_nested_complex() {
        let logic = BoolLogicNode::And(vec![
            BoolLogicNode::Or(vec![
                BoolLogicNode::Gt("user.age".into(), 18.0),
                BoolLogicNode::In("user.role".into(), vec![json!("admin")]),
            ]),
            BoolLogicNode::Not(Box::new(BoolLogicNode::IsEmpty("user.bio".into()))),
            BoolLogicNode::Lte("user.score".into(), 999.0),
        ]);
        let paths = logic.extract_paths();
        assert_eq!(
            paths,
            vec![
                "user.age".to_string(),
                "user.role".to_string(),
                "user.bio".to_string(),
                "user.score".to_string(),
            ]
        );
    }

    #[test]
    fn test_extract_paths_empty_and() {
        assert!(BoolLogicNode::And(vec![]).extract_paths().is_empty());
    }

    // -----------------------------------------------------------------------
    // BoolLogicRegistry + ReverseDependencyIndex tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_registry_register_and_get() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree = BoolLogicNode::IsEqual("user.role".into(), json!("admin"));
        let id = registry.register(
            "_concerns.user.email.disabledWhen".into(),
            tree,
            &mut intern,
            &mut rev,
            None,
            None,
        );

        assert_eq!(id, 0);
        assert_eq!(registry.len(), 1);

        let meta = registry.get(id).unwrap();
        assert_eq!(meta.output_path, "_concerns.user.email.disabledWhen");
    }

    #[test]
    fn test_registry_sequential_ids() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let id0 = registry.register(
            "out0".into(),
            BoolLogicNode::Exists("a".into()),
            &mut intern,
            &mut rev,
            None,
            None,
        );
        let id1 = registry.register(
            "out1".into(),
            BoolLogicNode::Exists("b".into()),
            &mut intern,
            &mut rev,
            None,
            None,
        );
        let id2 = registry.register(
            "out2".into(),
            BoolLogicNode::Exists("c".into()),
            &mut intern,
            &mut rev,
            None,
            None,
        );

        assert_eq!(id0, 0);
        assert_eq!(id1, 1);
        assert_eq!(id2, 2);
        assert_eq!(registry.len(), 3);
    }

    #[test]
    fn test_registry_unregister() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let id = registry.register(
            "out".into(),
            BoolLogicNode::Exists("a.b".into()),
            &mut intern,
            &mut rev,
            None,
            None,
        );
        assert_eq!(registry.len(), 1);

        registry.unregister(id, &mut rev);
        assert_eq!(registry.len(), 0);
        assert!(registry.get(id).is_none());
    }

    #[test]
    fn test_reverse_index_single_path() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree = BoolLogicNode::IsEqual("user.role".into(), json!("admin"));
        let logic_id = registry.register("out".into(), tree, &mut intern, &mut rev, None, None);

        let path_id = intern.intern("user.role");
        let affected = rev.affected_by_path(path_id);
        assert_eq!(affected, vec![logic_id]);
    }

    #[test]
    fn test_reverse_index_multi_path() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree = BoolLogicNode::And(vec![
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            BoolLogicNode::Exists("user.email".into()),
            BoolLogicNode::Gt("user.age".into(), 18.0),
        ]);
        let logic_id = registry.register("out".into(), tree, &mut intern, &mut rev, None, None);

        // All three paths should map to the same logic_id
        for path in &["user.role", "user.email", "user.age"] {
            let pid = intern.intern(path);
            let affected = rev.affected_by_path(pid);
            assert_eq!(affected, vec![logic_id]);
        }
    }

    #[test]
    fn test_reverse_index_multiple_logics_same_path() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let id0 = registry.register(
            "out0".into(),
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            &mut intern,
            &mut rev,
            None,
            None,
        );
        let id1 = registry.register(
            "out1".into(),
            BoolLogicNode::In("user.role".into(), vec![json!("editor")]),
            &mut intern,
            &mut rev,
            None,
            None,
        );

        let path_id = intern.intern("user.role");
        let mut affected = rev.affected_by_path(path_id);
        affected.sort();
        assert_eq!(affected, vec![id0, id1]);
    }

    #[test]
    fn test_reverse_index_unregister_cleanup() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let id0 = registry.register(
            "out0".into(),
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            &mut intern,
            &mut rev,
            None,
            None,
        );
        let id1 = registry.register(
            "out1".into(),
            BoolLogicNode::Exists("user.role".into()),
            &mut intern,
            &mut rev,
            None,
            None,
        );

        let path_id = intern.intern("user.role");

        // Both should be affected
        assert_eq!(rev.affected_by_path(path_id).len(), 2);

        // Remove one
        registry.unregister(id0, &mut rev);
        let affected = rev.affected_by_path(path_id);
        assert_eq!(affected, vec![id1]);

        // Remove the other
        registry.unregister(id1, &mut rev);
        assert!(rev.affected_by_path(path_id).is_empty());
        assert_eq!(rev.path_count(), 0);
    }

    #[test]
    fn test_reverse_index_unaffected_path() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        registry.register(
            "out".into(),
            BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
            &mut intern,
            &mut rev,
            None,
            None,
        );

        let unrelated_pid = intern.intern("document.title");
        assert!(rev.affected_by_path(unrelated_pid).is_empty());
    }

    #[test]
    fn test_reverse_index_nested_tree_paths() {
        let mut registry = BoolLogicRegistry::new();
        let mut intern = InternTable::new();
        let mut rev = ReverseDependencyIndex::new();

        let tree = BoolLogicNode::And(vec![
            BoolLogicNode::Or(vec![
                BoolLogicNode::Gt("stats.views".into(), 100.0),
                BoolLogicNode::IsEqual("user.premium".into(), json!(true)),
            ]),
            BoolLogicNode::Not(Box::new(BoolLogicNode::IsEmpty("user.bio".into()))),
        ]);
        let logic_id = registry.register("out".into(), tree, &mut intern, &mut rev, None, None);

        // Three distinct paths
        for path in &["stats.views", "user.premium", "user.bio"] {
            let pid = intern.intern(path);
            assert_eq!(rev.affected_by_path(pid), vec![logic_id]);
        }
    }

    #[test]
    fn test_unregister_nonexistent_is_noop() {
        let mut registry = BoolLogicRegistry::new();
        let mut rev = ReverseDependencyIndex::new();
        registry.unregister(999, &mut rev); // should not panic
        assert_eq!(registry.len(), 0);
    }

    // -----------------------------------------------------------------------
    // Path resolution helpers
    // -----------------------------------------------------------------------

    #[test]
    fn test_get_path_value_simple() {
        let mut root = HashMap::new();
        root.insert("role".to_string(), ValueRepr::String("admin".to_string()));
        let state = ValueRepr::Object(root);
        let value = get_path_value(&state, "role");
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "admin"));
    }

    #[test]
    fn test_get_path_value_nested() {
        let state = make_state();
        let value = get_path_value(&state, "user.profile.name");
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "Alice"));
    }

    #[test]
    fn test_get_path_value_missing() {
        let state = make_state();
        assert!(get_path_value(&state, "user.nonexistent").is_none());
        assert!(get_path_value(&state, "missing.path.here").is_none());
    }

    // -----------------------------------------------------------------------
    // Shorthand ["path", value] deserialization
    // -----------------------------------------------------------------------

    #[test]
    fn test_deserialize_shorthand_string() {
        let logic: BoolLogicNode = serde_json::from_value(json!(["user.role", "admin"])).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.role".into(), json!("admin"))
        );
    }

    #[test]
    fn test_deserialize_shorthand_number() {
        let logic: BoolLogicNode = serde_json::from_value(json!(["user.age", 25])).unwrap();
        assert_eq!(logic, BoolLogicNode::IsEqual("user.age".into(), json!(25)));
    }

    #[test]
    fn test_deserialize_shorthand_boolean() {
        let logic: BoolLogicNode = serde_json::from_value(json!(["user.active", true])).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.active".into(), json!(true))
        );
    }

    #[test]
    fn test_deserialize_shorthand_null() {
        let logic: BoolLogicNode = serde_json::from_value(json!(["user.deleted", null])).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::IsEqual("user.deleted".into(), json!(null))
        );
    }

    #[test]
    fn test_deserialize_shorthand_normalizes_to_is_equal_on_serialize() {
        // Shorthand deserializes to IsEqual — serializes back as IS_EQUAL (one-way normalization)
        let logic: BoolLogicNode = serde_json::from_value(json!(["user.role", "admin"])).unwrap();
        assert_eq!(
            serde_json::to_value(&logic).unwrap(),
            json!({"IS_EQUAL": ["user.role", "admin"]})
        );
    }

    #[test]
    fn test_deserialize_shorthand_non_string_path_returns_error() {
        let result: Result<BoolLogicNode, _> = serde_json::from_value(json!([42, "admin"]));
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_shorthand_wrong_length_returns_error() {
        let result: Result<BoolLogicNode, _> = serde_json::from_value(json!(["user.role"]));
        assert!(result.is_err());
    }

    #[test]
    fn test_deserialize_shorthand_as_and_child() {
        // AND children can mix shorthand and named operators
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "AND": [
                ["user.role", "admin"],
                {"EXISTS": "user.email"}
            ]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::And(vec![
                BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
                BoolLogicNode::Exists("user.email".into()),
            ])
        );
    }

    #[test]
    fn test_deserialize_shorthand_as_or_child() {
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "OR": [
                ["user.role", "admin"],
                ["user.role", "editor"]
            ]
        }))
        .unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::Or(vec![
                BoolLogicNode::IsEqual("user.role".into(), json!("admin")),
                BoolLogicNode::IsEqual("user.role".into(), json!("editor")),
            ])
        );
    }

    #[test]
    fn test_deserialize_shorthand_as_not_child() {
        let logic: BoolLogicNode =
            serde_json::from_value(json!({"NOT": ["user.role", "guest"]})).unwrap();
        assert_eq!(
            logic,
            BoolLogicNode::Not(Box::new(BoolLogicNode::IsEqual(
                "user.role".into(),
                json!("guest")
            )))
        );
    }

    // -----------------------------------------------------------------------
    // Shorthand evaluation (mirrors TypeScript evaluateBoolLogic shorthand tests)
    // -----------------------------------------------------------------------

    #[test]
    fn test_eval_shorthand_string_match() {
        let state = make_state();
        let logic = BoolLogicNode::IsEqual("user.role".into(), json!("admin")); // deserialized from shorthand
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_shorthand_string_no_match() {
        let state = make_state();
        let logic = BoolLogicNode::IsEqual("user.role".into(), json!("editor"));
        assert!(!logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_shorthand_nested_path() {
        let state = make_state();
        let logic = BoolLogicNode::IsEqual("user.profile.verified".into(), json!(true));
        assert!(logic.evaluate_value(&state));
    }

    #[test]
    fn test_eval_complex_with_shorthand_children() {
        // Mirrors TypeScript "complex AND using shorthand for equality checks"
        // Shorthand children are deserialized from ["path", value] and evaluated as IsEqual
        let state = make_state();
        let logic: BoolLogicNode = serde_json::from_value(json!({
            "AND": [
                {"OR": [["user.role", "admin"], ["user.role", "editor"]]},
                {"GTE": ["user.age", 18]},
                {"GT": ["user.score", 100]},
                {"NOT": {"IS_EMPTY": "user.tags"}},
                {"IN": ["user.role", ["admin", "editor", "mod"]]},
                {"EXISTS": "document.id"},
                ["user.profile.verified", true]
            ]
        }))
        .unwrap();
        assert!(logic.evaluate_value(&state));
    }
}
