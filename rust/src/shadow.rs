use crate::pipeline::UNDEFINED_SENTINEL;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a value in the shadow state tree.
///
/// Mirrors the structure of JavaScript objects/arrays/primitives.
/// Used internally by WASM for fast diffing and BoolLogic evaluation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub(crate) enum ValueRepr {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<ValueRepr>),
    Object(HashMap<String, ValueRepr>),
}

impl From<serde_json::Value> for ValueRepr {
    fn from(v: serde_json::Value) -> Self {
        match v {
            serde_json::Value::Null => ValueRepr::Null,
            serde_json::Value::Bool(b) => ValueRepr::Bool(b),
            serde_json::Value::Number(n) => ValueRepr::Number(n.as_f64().unwrap_or(0.0)),
            serde_json::Value::String(s) => ValueRepr::String(s),
            serde_json::Value::Array(arr) => {
                ValueRepr::Array(arr.into_iter().map(ValueRepr::from).collect())
            }
            serde_json::Value::Object(map) => ValueRepr::Object(
                map.into_iter()
                    .map(|(k, v)| (k, ValueRepr::from(v)))
                    .collect(),
            ),
        }
    }
}

impl ValueRepr {
    /// Convert back to serde_json::Value (for serialization to JS).
    pub(crate) fn to_json_value(&self) -> serde_json::Value {
        match self {
            ValueRepr::Null => serde_json::Value::Null,
            ValueRepr::Bool(b) => serde_json::Value::Bool(*b),
            ValueRepr::Number(n) => {
                // Preserve integer representation for whole numbers
                if n.fract() == 0.0 && *n >= i64::MIN as f64 && *n <= i64::MAX as f64 {
                    serde_json::Value::Number((*n as i64).into())
                } else {
                    serde_json::Number::from_f64(*n)
                        .map(serde_json::Value::Number)
                        .unwrap_or(serde_json::Value::Null)
                }
            }
            ValueRepr::String(s) => serde_json::Value::String(s.clone()),
            ValueRepr::Array(arr) => {
                serde_json::Value::Array(arr.iter().map(|v| v.to_json_value()).collect())
            }
            ValueRepr::Object(map) => serde_json::Value::Object(
                map.iter()
                    .map(|(k, v)| (k.clone(), v.to_json_value()))
                    .collect(),
            ),
        }
    }
}

/// Nested tree shadow state mirroring valtio state structure.
///
/// Supports deep path traversal, partial updates, subtree replacement,
/// and affected path calculation.
pub(crate) struct ShadowState {
    root: ValueRepr,
}

impl ShadowState {
    pub(crate) fn new() -> Self {
        Self {
            root: ValueRepr::Object(HashMap::new()),
        }
    }

    /// Initialize shadow state from a JSON string.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn init(&mut self, state_json: &str) -> Result<(), String> {
        let json: serde_json::Value =
            serde_json::from_str(state_json).map_err(|e| format!("JSON parse error: {}", e))?;
        self.root = ValueRepr::from(json);
        Ok(())
    }

    /// Initialize shadow state from a pre-parsed serde_json::Value (no string intermediary).
    pub(crate) fn init_value(&mut self, value: serde_json::Value) -> Result<(), String> {
        self.root = ValueRepr::from(value);
        Ok(())
    }

    /// Get a reference to the value at the given dot-separated path.
    pub(crate) fn get(&self, path: &str) -> Option<&ValueRepr> {
        if path.is_empty() {
            return Some(&self.root);
        }
        Self::traverse(&self.root, path.split('.'))
    }

    /// Check if the parent structure of a dot-separated path exists.
    /// Returns true for root-level paths (no dots) since the root always exists.
    /// For nested paths, checks that the parent resolves to an Object.
    pub(crate) fn parent_exists(&self, path: &str) -> bool {
        match path.rsplit_once('.') {
            None => true, // root-level key, parent is root which always exists
            Some((parent, _)) => matches!(self.get(parent), Some(ValueRepr::Object(_))),
        }
    }

    /// Set a value at the given dot-separated path from a JSON string.
    /// Creates intermediate objects if they don't exist.
    /// Preserves sibling values (partial update).
    pub(crate) fn set(&mut self, path: &str, value_json: &str) -> Result<(), String> {
        let json: serde_json::Value =
            serde_json::from_str(value_json).map_err(|e| format!("JSON parse error: {}", e))?;
        let value = ValueRepr::from(json);

        if path.is_empty() {
            self.root = value;
            return Ok(());
        }

        let segments: Vec<&str> = path.split('.').collect();
        Self::set_at(&mut self.root, &segments, value)
    }

    /// Get all leaf paths affected by a change at the given path.
    /// For nested objects/arrays, returns all descendant leaf paths.
    /// For leaf values, returns just the path itself.
    pub(crate) fn affected_paths(&self, path: &str) -> Vec<String> {
        let value = if path.is_empty() {
            Some(&self.root)
        } else {
            Self::traverse(&self.root, path.split('.'))
        };

        let Some(value) = value else {
            return vec![];
        };

        let mut result = Vec::new();
        let base = if path.is_empty() {
            String::new()
        } else {
            path.to_owned()
        };
        Self::collect_leaves(value, &base, &mut result);
        result
    }

    /// Dump the entire shadow state as a JSON string.
    pub(crate) fn dump(&self) -> String {
        serde_json::to_string(&self.root.to_json_value()).unwrap_or_else(|_| "{}".to_owned())
    }

    /// Check if the value at the given path is null or missing.
    pub(crate) fn is_null(&self, path: &str) -> bool {
        matches!(self.get(path), None | Some(ValueRepr::Null))
    }

    /// Get the keys of an Object at the given path.
    /// Returns None if the value is not an object or the path doesn't exist.
    pub(crate) fn get_object_keys(&self, path: &str) -> Option<Vec<String>> {
        match self.get(path) {
            Some(ValueRepr::Object(map)) => Some(map.keys().cloned().collect()),
            _ => None,
        }
    }

    /// Get a mutable reference to the root (for direct manipulation in pipeline).
    pub(crate) fn root(&self) -> &ValueRepr {
        &self.root
    }

    // --- private helpers ---

    /// Check if a ValueRepr is null-like (Null or the undefined sentinel string).
    fn is_null_like(v: &ValueRepr) -> bool {
        match v {
            ValueRepr::Null => true,
            ValueRepr::String(s) if s == UNDEFINED_SENTINEL => true,
            _ => false,
        }
    }

    fn traverse<'a, 'b>(
        root: &'a ValueRepr,
        segments: impl Iterator<Item = &'b str>,
    ) -> Option<&'a ValueRepr> {
        let mut current = root;
        for seg in segments {
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

    fn set_at(node: &mut ValueRepr, segments: &[&str], value: ValueRepr) -> Result<(), String> {
        if segments.is_empty() {
            *node = value;
            return Ok(());
        }

        let (key, rest) = (segments[0], &segments[1..]);

        match node {
            ValueRepr::Object(map) => {
                if rest.is_empty() {
                    map.insert(key.to_owned(), value);
                    Ok(())
                } else {
                    // Create intermediate object if missing
                    let child = map
                        .entry(key.to_owned())
                        .or_insert_with(|| ValueRepr::Object(HashMap::new()));
                    Self::set_at(child, rest, value)
                }
            }
            ValueRepr::Array(arr) => {
                let idx: usize = key
                    .parse()
                    .map_err(|_| format!("Invalid array index '{}'", key))?;
                let len = arr.len();
                let child = arr
                    .get_mut(idx)
                    .ok_or_else(|| format!("Array index {} out of bounds (len: {})", idx, len))?;
                if rest.is_empty() {
                    *child = value;
                    Ok(())
                } else {
                    Self::set_at(child, rest, value)
                }
            }
            // Null or undefined sentinel → promote to Object so nested paths can be created.
            // This handles the case where e.g. `form: undefined` or `form: null`
            // gets populated via a change like `form.email.value = "x"`.
            // The "__APEX_UNDEFINED__" sentinel is a String in shadow state that
            // represents JS `undefined` — treat it like Null for traversal.
            _ if Self::is_null_like(node) => {
                let mut map = HashMap::new();
                if rest.is_empty() {
                    map.insert(key.to_owned(), value);
                } else {
                    let mut child = ValueRepr::Object(HashMap::new());
                    Self::set_at(&mut child, rest, value)?;
                    map.insert(key.to_owned(), child);
                }
                *node = ValueRepr::Object(map);
                Ok(())
            }
            _ => Err(format!("Cannot traverse through primitive at '{}'", key)),
        }
    }

    fn collect_leaves(value: &ValueRepr, prefix: &str, result: &mut Vec<String>) {
        match value {
            ValueRepr::Object(map) => {
                for (key, child) in map {
                    let child_path = if prefix.is_empty() {
                        key.clone()
                    } else {
                        crate::join_path(prefix, key)
                    };
                    Self::collect_leaves(child, &child_path, result);
                }
            }
            ValueRepr::Array(arr) => {
                for (i, child) in arr.iter().enumerate() {
                    let idx_str = i.to_string();
                    let child_path = if prefix.is_empty() {
                        idx_str
                    } else {
                        crate::join_path(prefix, &idx_str)
                    };
                    Self::collect_leaves(child, &child_path, result);
                }
            }
            _ => {
                result.push(prefix.to_owned());
            }
        }
    }
}

impl Default for ShadowState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_state(json: &str) -> ShadowState {
        let mut s = ShadowState::new();
        s.init(json).unwrap();
        s
    }

    // --- ValueRepr conversion ---

    #[test]
    fn from_json_primitives() {
        assert_eq!(ValueRepr::from(serde_json::json!(null)), ValueRepr::Null);
        assert_eq!(
            ValueRepr::from(serde_json::json!(true)),
            ValueRepr::Bool(true)
        );
        assert_eq!(
            ValueRepr::from(serde_json::json!(42)),
            ValueRepr::Number(42.0)
        );
        assert_eq!(
            ValueRepr::from(serde_json::json!("hello")),
            ValueRepr::String("hello".to_owned())
        );
    }

    #[test]
    fn from_json_nested() {
        let v = ValueRepr::from(serde_json::json!({"a": {"b": 1}}));
        match &v {
            ValueRepr::Object(map) => match map.get("a") {
                Some(ValueRepr::Object(inner)) => {
                    assert_eq!(inner.get("b"), Some(&ValueRepr::Number(1.0)));
                }
                other => panic!("Expected Object, got {:?}", other),
            },
            other => panic!("Expected Object, got {:?}", other),
        }
    }

    #[test]
    fn to_json_roundtrip() {
        let original = serde_json::json!({"user": {"name": "Alice", "age": 30, "tags": [1, 2]}});
        let repr = ValueRepr::from(original.clone());
        let back = repr.to_json_value();
        assert_eq!(original, back);
    }

    // --- ShadowState init + get ---

    #[test]
    fn init_and_get() {
        let state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#);

        assert!(matches!(state.get("user.name"), Some(ValueRepr::String(s)) if s == "Alice"));
        assert!(matches!(state.get("user.age"), Some(ValueRepr::Number(n)) if *n == 30.0));
        assert!(matches!(state.get("user"), Some(ValueRepr::Object(_))));
        assert!(state.get("user.email").is_none());
        assert!(state.get("missing").is_none());
    }

    #[test]
    fn get_empty_path_returns_root() {
        let state = make_state(r#"{"a": 1}"#);
        assert!(matches!(state.get(""), Some(ValueRepr::Object(_))));
    }

    #[test]
    fn get_array_index() {
        let state = make_state(r#"{"items": [10, 20, 30]}"#);
        assert!(matches!(state.get("items.0"), Some(ValueRepr::Number(n)) if *n == 10.0));
        assert!(matches!(state.get("items.2"), Some(ValueRepr::Number(n)) if *n == 30.0));
        assert!(state.get("items.3").is_none());
        assert!(state.get("items.abc").is_none());
    }

    #[test]
    fn get_nested_array_objects() {
        let state = make_state(r#"{"users": [{"name": "Alice"}, {"name": "Bob"}]}"#);
        assert!(matches!(state.get("users.0.name"), Some(ValueRepr::String(s)) if s == "Alice"));
        assert!(matches!(state.get("users.1.name"), Some(ValueRepr::String(s)) if s == "Bob"));
    }

    #[test]
    fn get_through_primitive_returns_none() {
        let state = make_state(r#"{"val": 42}"#);
        assert!(state.get("val.nested").is_none());
    }

    // --- ShadowState set ---

    #[test]
    fn set_leaf_value() {
        let mut state = make_state(r#"{"user": {"name": "Alice"}}"#);
        state.set("user.name", r#""Bob""#).unwrap();
        assert!(matches!(state.get("user.name"), Some(ValueRepr::String(s)) if s == "Bob"));
    }

    #[test]
    fn set_preserves_siblings() {
        let mut state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#);
        state.set("user.name", r#""Bob""#).unwrap();
        assert!(matches!(state.get("user.name"), Some(ValueRepr::String(s)) if s == "Bob"));
        assert!(matches!(state.get("user.age"), Some(ValueRepr::Number(n)) if *n == 30.0));
    }

    #[test]
    fn set_creates_intermediate_objects() {
        let mut state = ShadowState::new();
        state.set("a.b.c", r#""deep""#).unwrap();
        assert!(matches!(state.get("a.b.c"), Some(ValueRepr::String(s)) if s == "deep"));
    }

    #[test]
    fn set_replaces_subtree() {
        let mut state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#);
        state
            .set("user", r#"{"name": "Charlie", "email": "c@c.com"}"#)
            .unwrap();
        assert!(matches!(state.get("user.name"), Some(ValueRepr::String(s)) if s == "Charlie"));
        assert!(matches!(state.get("user.email"), Some(ValueRepr::String(s)) if s == "c@c.com"));
        // Old field gone
        assert!(state.get("user.age").is_none());
    }

    #[test]
    fn set_empty_path_replaces_root() {
        let mut state = make_state(r#"{"old": true}"#);
        state.set("", r#"{"new": true}"#).unwrap();
        assert!(state.get("old").is_none());
        assert!(matches!(state.get("new"), Some(ValueRepr::Bool(true))));
    }

    #[test]
    fn set_array_element() {
        let mut state = make_state(r#"{"items": [1, 2, 3]}"#);
        state.set("items.1", "99").unwrap();
        assert!(matches!(state.get("items.1"), Some(ValueRepr::Number(n)) if *n == 99.0));
        // Siblings unchanged
        assert!(matches!(state.get("items.0"), Some(ValueRepr::Number(n)) if *n == 1.0));
    }

    #[test]
    fn set_through_primitive_fails() {
        let mut state = make_state(r#"{"val": 42}"#);
        assert!(state.set("val.nested", "1").is_err());
    }

    #[test]
    fn set_array_out_of_bounds_fails() {
        let mut state = make_state(r#"{"items": [1]}"#);
        assert!(state.set("items.5", "99").is_err());
    }

    // --- affected_paths ---

    #[test]
    fn affected_paths_leaf() {
        let state = make_state(r#"{"user": {"name": "Alice"}}"#);
        let paths = state.affected_paths("user.name");
        assert_eq!(paths, vec!["user.name"]);
    }

    #[test]
    fn affected_paths_nested() {
        let state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#);
        let mut paths = state.affected_paths("user");
        paths.sort();
        assert_eq!(paths, vec!["user.age", "user.name"]);
    }

    #[test]
    fn affected_paths_with_arrays() {
        let state = make_state(r#"{"items": [{"a": 1}, {"b": 2}]}"#);
        let mut paths = state.affected_paths("items");
        paths.sort();
        assert_eq!(paths, vec!["items.0.a", "items.1.b"]);
    }

    #[test]
    fn affected_paths_nonexistent() {
        let state = make_state(r#"{"user": {}}"#);
        assert!(state.affected_paths("missing").is_empty());
    }

    #[test]
    fn affected_paths_root() {
        let state = make_state(r#"{"a": 1, "b": 2}"#);
        let mut paths = state.affected_paths("");
        paths.sort();
        assert_eq!(paths, vec!["a", "b"]);
    }

    #[test]
    fn affected_paths_empty_object() {
        let state = make_state(r#"{}"#);
        assert!(state.affected_paths("").is_empty());
    }

    // --- dump ---

    #[test]
    fn dump_roundtrips() {
        let json = r#"{"user":{"age":30,"name":"Alice"}}"#;
        let state = make_state(json);
        let dumped = state.dump();
        // Parse both and compare as values (order may differ)
        let a: serde_json::Value = serde_json::from_str(json).unwrap();
        let b: serde_json::Value = serde_json::from_str(&dumped).unwrap();
        assert_eq!(a, b);
    }

    // --- init error handling ---

    #[test]
    fn init_invalid_json_fails() {
        let mut state = ShadowState::new();
        assert!(state.init("not json").is_err());
    }

    #[test]
    fn set_invalid_json_fails() {
        let mut state = ShadowState::new();
        assert!(state.set("a", "not json").is_err());
    }

    #[test]
    fn set_through_null_promotes_to_object() {
        // form: null → set form.email.value → creates intermediate objects
        let mut state = make_state(r#"{"form": null}"#);
        state.set("form.email.value", "\"hello\"").unwrap();
        let val = state.get("form.email.value").unwrap();
        assert_eq!(val.to_json_value(), serde_json::json!("hello"));
        // form is now an object with key "email"
        let keys = state.get_object_keys("form").unwrap();
        assert_eq!(keys, vec!["email"]);
    }

    #[test]
    fn set_through_missing_key_creates_path() {
        // form doesn't exist → set form.email.value → creates everything
        let mut state = make_state(r#"{}"#);
        state.set("form.email.value", "\"world\"").unwrap();
        let val = state.get("form.email.value").unwrap();
        assert_eq!(val.to_json_value(), serde_json::json!("world"));
        let keys = state.get_object_keys("form").unwrap();
        assert_eq!(keys, vec!["email"]);
    }

    // --- undefined sentinel handling ---

    #[test]
    fn set_through_undefined_sentinel_promotes_to_object() {
        // notionalCcy was set to "__APEX_UNDEFINED__" (the JS undefined sentinel)
        // A later change sets notionalCcy.value = "USD" — should promote, not error
        let mut state = make_state(r#"{"notionalCcy": "__APEX_UNDEFINED__"}"#);
        state.set("notionalCcy.value", "\"USD\"").unwrap();
        let val = state.get("notionalCcy.value").unwrap();
        assert_eq!(val.to_json_value(), serde_json::json!("USD"));
        // notionalCcy is now an object
        let keys = state.get_object_keys("notionalCcy").unwrap();
        assert_eq!(keys, vec!["value"]);
    }

    #[test]
    fn set_through_deeply_nested_undefined_sentinel() {
        // Deep path: form.field is the sentinel, set form.field.nested.value
        let mut state = make_state(r#"{"form": {"field": "__APEX_UNDEFINED__"}}"#);
        state.set("form.field.nested.value", "42").unwrap();
        let val = state.get("form.field.nested.value").unwrap();
        assert_eq!(val.to_json_value(), serde_json::json!(42));
    }

    #[test]
    fn set_regular_string_still_fails_traversal() {
        // A real string (not the sentinel) should still error on traversal
        let mut state = make_state(r#"{"name": "Alice"}"#);
        assert!(state.set("name.nested", "1").is_err());
    }
}
