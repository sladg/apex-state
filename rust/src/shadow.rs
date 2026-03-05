use crate::change::UNDEFINED_SENTINEL;
use crate::intern::InternTable;
use crate::prelude::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Represents a value in the shadow state tree.
///
/// Mirrors the structure of JavaScript objects/arrays/primitives.
/// Used internally by WASM for fast diffing and BoolLogic evaluation.
///
/// Object keys are interned u32 IDs (via InternTable) to deduplicate field names
/// across arrays of records sharing the same schema.
///
/// Object and Array variants carry a precomputed structural hash (u64) computed
/// at construction time (`from_json`). This enables O(1) equality checks in
/// `is_different` — if the hash matches, the subtree is unchanged.
#[derive(Debug, Clone, Default)]
pub(crate) enum ValueRepr {
    #[default]
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<ValueRepr>, u64),
    Object(HashMap<u32, ValueRepr>, u64),
}

impl PartialEq for ValueRepr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueRepr::Null, ValueRepr::Null) => true,
            (ValueRepr::Bool(a), ValueRepr::Bool(b)) => a == b,
            (ValueRepr::Number(a), ValueRepr::Number(b)) => a.to_bits() == b.to_bits(),
            (ValueRepr::String(a), ValueRepr::String(b)) => a == b,
            (ValueRepr::Array(_, h1), ValueRepr::Array(_, h2)) => h1 == h2,
            (ValueRepr::Object(_, h1), ValueRepr::Object(_, h2)) => h1 == h2,
            _ => false,
        }
    }
}

impl Eq for ValueRepr {}

impl ValueRepr {
    /// Compute a structural hash for this value.
    /// Used internally during construction to populate the hash field
    /// on Array and Object variants.
    fn hash_into(&self, hasher: &mut impl Hasher) {
        match self {
            ValueRepr::Null => 0u8.hash(hasher),
            ValueRepr::Bool(b) => {
                1u8.hash(hasher);
                b.hash(hasher);
            }
            ValueRepr::Number(n) => {
                2u8.hash(hasher);
                n.to_bits().hash(hasher);
            }
            ValueRepr::String(s) => {
                3u8.hash(hasher);
                s.hash(hasher);
            }
            ValueRepr::Array(arr, _) => {
                4u8.hash(hasher);
                for item in arr {
                    item.hash_into(hasher);
                }
            }
            ValueRepr::Object(map, _) => {
                5u8.hash(hasher);
                // XOR per-entry hashes for order independence
                let mut combined = 0u64;
                for (&k, v) in map {
                    let mut entry_hasher = ahash::AHasher::default();
                    k.hash(&mut entry_hasher);
                    v.hash_into(&mut entry_hasher);
                    combined ^= entry_hasher.finish();
                }
                combined.hash(hasher);
            }
        }
    }

    /// Compute the structural hash of this value tree.
    fn structural_hash(&self) -> u64 {
        let mut hasher = ahash::AHasher::default();
        self.hash_into(&mut hasher);
        hasher.finish()
    }
}

impl ValueRepr {
    /// Convert a serde_json::Value into a ValueRepr, interning object keys.
    /// Computes structural hashes for Object and Array variants at construction time.
    pub(crate) fn from_json(v: serde_json::Value, intern: &mut InternTable) -> Self {
        match v {
            serde_json::Value::Null => ValueRepr::Null,
            serde_json::Value::Bool(b) => ValueRepr::Bool(b),
            serde_json::Value::Number(n) => ValueRepr::Number(n.as_f64().unwrap_or(0.0)),
            serde_json::Value::String(s) => ValueRepr::String(s),
            serde_json::Value::Array(arr) => {
                let children: Vec<ValueRepr> = arr
                    .into_iter()
                    .map(|v| ValueRepr::from_json(v, intern))
                    .collect();
                let node = ValueRepr::Array(children, 0);
                let hash = node.structural_hash();
                match node {
                    ValueRepr::Array(c, _) => ValueRepr::Array(c, hash),
                    _ => unreachable!(),
                }
            }
            serde_json::Value::Object(map) => {
                let fields: HashMap<u32, ValueRepr> = map
                    .into_iter()
                    .map(|(k, v)| (intern.intern(&k), ValueRepr::from_json(v, intern)))
                    .collect();
                let node = ValueRepr::Object(fields, 0);
                let hash = node.structural_hash();
                match node {
                    ValueRepr::Object(f, _) => ValueRepr::Object(f, hash),
                    _ => unreachable!(),
                }
            }
        }
    }
}

impl ValueRepr {
    /// Convert back to serde_json::Value (for serialization to JS).
    pub(crate) fn to_json_value(&self, intern: &InternTable) -> serde_json::Value {
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
            ValueRepr::Array(arr, _) => {
                serde_json::Value::Array(arr.iter().map(|v| v.to_json_value(intern)).collect())
            }
            ValueRepr::Object(map, _) => serde_json::Value::Object(
                map.iter()
                    .filter_map(|(&k, v)| {
                        intern
                            .resolve(k)
                            .map(|name| (name.to_owned(), v.to_json_value(intern)))
                    })
                    .collect(),
            ),
        }
    }
}

/// Nested tree shadow state mirroring valtio state structure.
///
/// Supports deep path traversal, partial updates, subtree replacement,
/// and affected path calculation.
#[derive(Clone, Default)]
pub(crate) struct ShadowState {
    root: ValueRepr,
    /// Paths that were explicitly removed from shadow due to a parent replacement.
    /// Used by `parent_exists` to distinguish "never initialized" (allow sync to create)
    /// from "explicitly deleted" (block stale sync writes).
    removed_paths: HashSet<String>,
}

impl ShadowState {
    pub(crate) fn new() -> Self {
        Self {
            root: ValueRepr::Object(HashMap::new(), 0),
            removed_paths: HashSet::new(),
        }
    }

    /// Initialize shadow state from a JSON string.
    pub(crate) fn init(
        &mut self,
        state_json: &str,
        intern: &mut InternTable,
    ) -> Result<(), String> {
        let json: serde_json::Value =
            serde_json::from_str(state_json).map_err(|e| format!("JSON parse error: {}", e))?;
        self.root = ValueRepr::from_json(json, intern);
        Ok(())
    }

    /// Get a reference to the value at the given dot-separated path.
    pub(crate) fn get(&self, path: &str, intern: &InternTable) -> Option<&ValueRepr> {
        if path.is_empty() {
            return Some(&self.root);
        }
        Self::traverse(&self.root, path.split('.'), intern)
    }

    /// Check if the parent structure of a dot-separated path exists.
    /// Returns true for root-level paths (no dots) since the root always exists.
    /// For nested paths, checks that the parent resolves to an Object in shadow,
    /// OR that the parent was never explicitly removed (allowing sync to create
    /// intermediate containers for never-initialized paths).
    ///
    /// This distinction prevents stale sync writes to explicitly-deleted subtrees
    /// while still allowing sync to initialize paths that were never written.
    pub(crate) fn parent_exists(&self, path: &str, intern: &InternTable) -> bool {
        match path.rsplit_once('.') {
            None => true, // root-level key, parent is root which always exists
            Some((parent, _)) => {
                if matches!(self.get(parent, intern), Some(ValueRepr::Object(..))) {
                    return true;
                }
                // Parent not in shadow — allow if it was never explicitly removed.
                // This permits sync to create intermediate containers for paths that
                // were registered but never initialized, while blocking stale writes
                // to paths whose parent was deliberately cleared/replaced.
                !self.removed_paths.contains(parent)
            }
        }
    }

    /// Set a value at the given dot-separated path from a JSON string.
    /// Creates intermediate objects if they don't exist.
    /// Preserves sibling values (partial update).
    ///
    /// Tracks removed paths: when an Object is replaced, all descendant paths that
    /// existed under the old value are recorded in `removed_paths`. This enables
    /// `parent_exists` to distinguish "explicitly removed" (stale, block sync) from
    /// "never initialized" (allow sync to create intermediate containers).
    pub(crate) fn set(
        &mut self,
        path: &str,
        value_json: &str,
        intern: &mut InternTable,
    ) -> Result<(), String> {
        let json: serde_json::Value =
            serde_json::from_str(value_json).map_err(|e| format!("JSON parse error: {}", e))?;
        let value = ValueRepr::from_json(json, intern);

        // Before overwriting, collect ALL paths (intermediate + leaves) under the old
        // Object value and mark them as explicitly removed. Leaf-typed old values have
        // no children to remove, so only bother when old value is an Object.
        // Collect into a local Vec first so we can release the immutable borrow on
        // self.root before mutating self.removed_paths.
        if !path.is_empty() {
            let removed: Vec<String> =
                if let Some(old @ ValueRepr::Object(..)) = self.get(path, intern) {
                    let mut r = Vec::new();
                    Self::collect_paths(old, path, &mut r, true, intern);
                    r
                } else {
                    Vec::new()
                };
            self.removed_paths.extend(removed);
        }

        if path.is_empty() {
            self.root = value;
            return Ok(());
        }

        let segments: Vec<&str> = path.split('.').collect();
        Self::set_at(&mut self.root, &segments, value, intern)
    }

    /// Get all leaf paths affected by a change at the given path.
    /// For nested objects/arrays, returns all descendant leaf paths.
    /// For leaf values, returns just the path itself.
    ///
    /// Only used in tests — production code uses `affected_path_ids`.
    #[cfg(test)]
    pub(crate) fn affected_paths(&self, path: &str, intern: &InternTable) -> Vec<String> {
        let Some(value) = self.get(path, intern) else {
            return vec![];
        };

        let mut result = Vec::new();
        let base = if path.is_empty() {
            String::new()
        } else {
            path.to_owned()
        };
        Self::collect_paths(value, &base, &mut result, false, intern);
        result
    }

    /// Like `affected_paths` but interns each leaf path and returns their IDs.
    ///
    /// Eliminates the per-leaf `String` allocation in the returned `Vec` — the
    /// path string is built temporarily, interned (possibly a no-op if already
    /// known), then dropped. Callers working with IDs avoid a second intern step.
    pub(crate) fn affected_path_ids(&self, path: &str, intern: &mut InternTable) -> Vec<u32> {
        let Some(value) = self.get(path, intern) else {
            return vec![];
        };

        let base = if path.is_empty() {
            String::new()
        } else {
            path.to_owned()
        };
        let mut result = Vec::new();
        Self::collect_path_ids(value, &base, intern, &mut result);
        result
    }

    /// Dump the entire shadow state as a JSON string.
    pub(crate) fn dump(&self, intern: &InternTable) -> String {
        serde_json::to_string(&self.root.to_json_value(intern)).unwrap_or_else(|_| "{}".to_owned())
    }

    /// Check if the value at the given path is null or missing.
    pub(crate) fn is_null(&self, path: &str, intern: &InternTable) -> bool {
        matches!(self.get(path, intern), None | Some(ValueRepr::Null))
    }

    /// Get the keys of an Object at the given path.
    /// Returns None if the value is not an object or the path doesn't exist.
    pub(crate) fn get_object_keys(&self, path: &str, intern: &InternTable) -> Option<Vec<String>> {
        match self.get(path, intern) {
            Some(ValueRepr::Object(map, _)) => Some(
                map.keys()
                    .filter_map(|&k| intern.resolve(k).map(|s| s.to_owned()))
                    .collect(),
            ),
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
        intern: &InternTable,
    ) -> Option<&'a ValueRepr> {
        let mut current = root;
        for seg in segments {
            match current {
                ValueRepr::Object(map, _) => {
                    let seg_id = intern.get_id(seg)?;
                    current = map.get(&seg_id)?;
                }
                ValueRepr::Array(arr, _) => {
                    let idx: usize = seg.parse().ok()?;
                    current = arr.get(idx)?;
                }
                _ => return None,
            }
        }
        Some(current)
    }

    fn set_at(
        node: &mut ValueRepr,
        segments: &[&str],
        value: ValueRepr,
        intern: &mut InternTable,
    ) -> Result<(), String> {
        if segments.is_empty() {
            *node = value;
            return Ok(());
        }

        let (key, rest) = (segments[0], &segments[1..]);

        match node {
            ValueRepr::Object(map, _) => {
                let key_id = intern.intern(key);
                if rest.is_empty() {
                    map.insert(key_id, value);
                    Ok(())
                } else {
                    // Create intermediate object if missing
                    let child = map
                        .entry(key_id)
                        .or_insert_with(|| ValueRepr::Object(HashMap::new(), 0));
                    Self::set_at(child, rest, value, intern)
                }
            }
            ValueRepr::Array(arr, _) => {
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
                    Self::set_at(child, rest, value, intern)
                }
            }
            // Null or undefined sentinel → promote to Object so nested paths can be created.
            _ if Self::is_null_like(node) => {
                let mut map = HashMap::new();
                let key_id = intern.intern(key);
                if rest.is_empty() {
                    map.insert(key_id, value);
                } else {
                    let mut child = ValueRepr::Object(HashMap::new(), 0);
                    Self::set_at(&mut child, rest, value, intern)?;
                    map.insert(key_id, child);
                }
                *node = ValueRepr::Object(map, 0);
                Ok(())
            }
            _ => Err(format!(
                "Cannot traverse through primitive at '{}' (value: {:?})",
                key, node
            )),
        }
    }

    /// Like `collect_paths` (leaf-only) but interns each path and stores the ID.
    /// Avoids returning owned `String`s — the path is a temporary allocation used
    /// only for the intern lookup, then dropped.
    fn collect_path_ids(
        value: &ValueRepr,
        prefix: &str,
        intern: &mut InternTable,
        result: &mut Vec<u32>,
    ) {
        match value {
            ValueRepr::Object(map, _) => {
                for (&key_id, child) in map {
                    let key_str = match intern.resolve(key_id) {
                        Some(s) => s.to_owned(),
                        None => continue,
                    };
                    let child_path = if prefix.is_empty() {
                        key_str
                    } else {
                        crate::join_path(prefix, &key_str)
                    };
                    Self::collect_path_ids(child, &child_path, intern, result);
                }
            }
            ValueRepr::Array(arr, _) => {
                for (i, child) in arr.iter().enumerate() {
                    let idx_str = i.to_string();
                    let child_path = if prefix.is_empty() {
                        idx_str
                    } else {
                        crate::join_path(prefix, &idx_str)
                    };
                    Self::collect_path_ids(child, &child_path, intern, result);
                }
            }
            _ => {
                result.push(intern.intern(prefix));
            }
        }
    }

    /// Collect paths reachable under `value`.
    /// - `include_intermediate = true`: collects all paths (objects, arrays, and leaves).
    ///   Used to populate `removed_paths` when an Object is replaced via `set()`.
    /// - `include_intermediate = false`: collects only leaf paths.
    ///   Used by `affected_paths()` to enumerate terminal values.
    fn collect_paths(
        value: &ValueRepr,
        prefix: &str,
        result: &mut Vec<String>,
        include_intermediate: bool,
        intern: &InternTable,
    ) {
        match value {
            ValueRepr::Object(map, _) => {
                for (&key_id, child) in map {
                    let key_str = match intern.resolve(key_id) {
                        Some(s) => s.to_owned(),
                        None => continue,
                    };
                    let child_path = if prefix.is_empty() {
                        key_str
                    } else {
                        crate::join_path(prefix, &key_str)
                    };
                    if include_intermediate {
                        result.push(child_path.clone());
                    }
                    Self::collect_paths(child, &child_path, result, include_intermediate, intern);
                }
            }
            ValueRepr::Array(arr, _) => {
                for (i, child) in arr.iter().enumerate() {
                    let idx_str = i.to_string();
                    let child_path = if prefix.is_empty() {
                        idx_str
                    } else {
                        crate::join_path(prefix, &idx_str)
                    };
                    if include_intermediate {
                        result.push(child_path.clone());
                    }
                    Self::collect_paths(child, &child_path, result, include_intermediate, intern);
                }
            }
            _ => {
                if !include_intermediate {
                    result.push(prefix.to_owned());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intern::InternTable;

    fn make_state(json: &str, intern: &mut InternTable) -> ShadowState {
        let mut s = ShadowState::new();
        s.init(json, intern).unwrap();
        s
    }

    // --- ValueRepr conversion ---

    #[test]
    fn from_json_primitives() {
        let mut intern = InternTable::new();
        assert_eq!(
            ValueRepr::from_json(serde_json::json!(null), &mut intern),
            ValueRepr::Null
        );
        assert_eq!(
            ValueRepr::from_json(serde_json::json!(true), &mut intern),
            ValueRepr::Bool(true)
        );
        assert_eq!(
            ValueRepr::from_json(serde_json::json!(42), &mut intern),
            ValueRepr::Number(42.0)
        );
        assert_eq!(
            ValueRepr::from_json(serde_json::json!("hello"), &mut intern),
            ValueRepr::String("hello".to_owned())
        );
    }

    #[test]
    fn from_json_nested() {
        let mut intern = InternTable::new();
        let v = ValueRepr::from_json(serde_json::json!({"a": {"b": 1}}), &mut intern);
        let a_id = intern.get_id("a").unwrap();
        let b_id = intern.get_id("b").unwrap();
        match &v {
            ValueRepr::Object(map, _) => match map.get(&a_id) {
                Some(ValueRepr::Object(inner, _)) => {
                    assert_eq!(inner.get(&b_id), Some(&ValueRepr::Number(1.0)));
                }
                other => panic!("Expected Object, got {:?}", other),
            },
            other => panic!("Expected Object, got {:?}", other),
        }
    }

    #[test]
    fn to_json_roundtrip() {
        let mut intern = InternTable::new();
        let original = serde_json::json!({"user": {"name": "Alice", "age": 30, "tags": [1, 2]}});
        let repr = ValueRepr::from_json(original.clone(), &mut intern);
        let back = repr.to_json_value(&intern);
        assert_eq!(original, back);
    }

    // --- ShadowState init + get ---

    #[test]
    fn init_and_get() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#, &mut intern);

        assert!(
            matches!(state.get("user.name", &intern), Some(ValueRepr::String(s)) if s == "Alice")
        );
        assert!(matches!(state.get("user.age", &intern), Some(ValueRepr::Number(n)) if *n == 30.0));
        assert!(matches!(
            state.get("user", &intern),
            Some(ValueRepr::Object(..))
        ));
        assert!(state.get("user.email", &intern).is_none());
        assert!(state.get("missing", &intern).is_none());
    }

    #[test]
    fn get_empty_path_returns_root() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"a": 1}"#, &mut intern);
        assert!(matches!(
            state.get("", &intern),
            Some(ValueRepr::Object(..))
        ));
    }

    #[test]
    fn get_array_index() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"items": [10, 20, 30]}"#, &mut intern);
        assert!(matches!(state.get("items.0", &intern), Some(ValueRepr::Number(n)) if *n == 10.0));
        assert!(matches!(state.get("items.2", &intern), Some(ValueRepr::Number(n)) if *n == 30.0));
        assert!(state.get("items.3", &intern).is_none());
        assert!(state.get("items.abc", &intern).is_none());
    }

    #[test]
    fn get_nested_array_objects() {
        let mut intern = InternTable::new();
        let state = make_state(
            r#"{"users": [{"name": "Alice"}, {"name": "Bob"}]}"#,
            &mut intern,
        );
        assert!(
            matches!(state.get("users.0.name", &intern), Some(ValueRepr::String(s)) if s == "Alice")
        );
        assert!(
            matches!(state.get("users.1.name", &intern), Some(ValueRepr::String(s)) if s == "Bob")
        );
    }

    #[test]
    fn get_through_primitive_returns_none() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"val": 42}"#, &mut intern);
        assert!(state.get("val.nested", &intern).is_none());
    }

    // --- ShadowState set ---

    #[test]
    fn set_leaf_value() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"user": {"name": "Alice"}}"#, &mut intern);
        state.set("user.name", r#""Bob""#, &mut intern).unwrap();
        assert!(
            matches!(state.get("user.name", &intern), Some(ValueRepr::String(s)) if s == "Bob")
        );
    }

    #[test]
    fn set_preserves_siblings() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#, &mut intern);
        state.set("user.name", r#""Bob""#, &mut intern).unwrap();
        assert!(
            matches!(state.get("user.name", &intern), Some(ValueRepr::String(s)) if s == "Bob")
        );
        assert!(matches!(state.get("user.age", &intern), Some(ValueRepr::Number(n)) if *n == 30.0));
    }

    #[test]
    fn set_creates_intermediate_objects() {
        let mut intern = InternTable::new();
        let mut state = ShadowState::new();
        state.set("a.b.c", r#""deep""#, &mut intern).unwrap();
        assert!(matches!(state.get("a.b.c", &intern), Some(ValueRepr::String(s)) if s == "deep"));
    }

    #[test]
    fn set_replaces_subtree() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#, &mut intern);
        state
            .set(
                "user",
                r#"{"name": "Charlie", "email": "c@c.com"}"#,
                &mut intern,
            )
            .unwrap();
        assert!(
            matches!(state.get("user.name", &intern), Some(ValueRepr::String(s)) if s == "Charlie")
        );
        assert!(
            matches!(state.get("user.email", &intern), Some(ValueRepr::String(s)) if s == "c@c.com")
        );
        // Old field gone
        assert!(state.get("user.age", &intern).is_none());
    }

    #[test]
    fn set_empty_path_replaces_root() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"old": true}"#, &mut intern);
        state.set("", r#"{"new": true}"#, &mut intern).unwrap();
        assert!(state.get("old", &intern).is_none());
        assert!(matches!(
            state.get("new", &intern),
            Some(ValueRepr::Bool(true))
        ));
    }

    #[test]
    fn set_array_element() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"items": [1, 2, 3]}"#, &mut intern);
        state.set("items.1", "99", &mut intern).unwrap();
        assert!(matches!(state.get("items.1", &intern), Some(ValueRepr::Number(n)) if *n == 99.0));
        // Siblings unchanged
        assert!(matches!(state.get("items.0", &intern), Some(ValueRepr::Number(n)) if *n == 1.0));
    }

    #[test]
    fn set_through_primitive_fails() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"val": 42}"#, &mut intern);
        assert!(state.set("val.nested", "1", &mut intern).is_err());
    }

    #[test]
    fn set_array_out_of_bounds_fails() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"items": [1]}"#, &mut intern);
        assert!(state.set("items.5", "99", &mut intern).is_err());
    }

    // --- affected_paths ---

    #[test]
    fn affected_paths_leaf() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"user": {"name": "Alice"}}"#, &mut intern);
        let paths = state.affected_paths("user.name", &intern);
        assert_eq!(paths, vec!["user.name"]);
    }

    #[test]
    fn affected_paths_nested() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"user": {"name": "Alice", "age": 30}}"#, &mut intern);
        let mut paths = state.affected_paths("user", &intern);
        paths.sort();
        assert_eq!(paths, vec!["user.age", "user.name"]);
    }

    #[test]
    fn affected_paths_with_arrays() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"items": [{"a": 1}, {"b": 2}]}"#, &mut intern);
        let mut paths = state.affected_paths("items", &intern);
        paths.sort();
        assert_eq!(paths, vec!["items.0.a", "items.1.b"]);
    }

    #[test]
    fn affected_paths_nonexistent() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"user": {}}"#, &mut intern);
        assert!(state.affected_paths("missing", &intern).is_empty());
    }

    #[test]
    fn affected_paths_root() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{"a": 1, "b": 2}"#, &mut intern);
        let mut paths = state.affected_paths("", &intern);
        paths.sort();
        assert_eq!(paths, vec!["a", "b"]);
    }

    #[test]
    fn affected_paths_empty_object() {
        let mut intern = InternTable::new();
        let state = make_state(r#"{}"#, &mut intern);
        assert!(state.affected_paths("", &intern).is_empty());
    }

    // --- dump ---

    #[test]
    fn dump_roundtrips() {
        let mut intern = InternTable::new();
        let json = r#"{"user":{"age":30,"name":"Alice"}}"#;
        let state = make_state(json, &mut intern);
        let dumped = state.dump(&intern);
        // Parse both and compare as values (order may differ)
        let a: serde_json::Value = serde_json::from_str(json).unwrap();
        let b: serde_json::Value = serde_json::from_str(&dumped).unwrap();
        assert_eq!(a, b);
    }

    // --- init error handling ---

    #[test]
    fn init_invalid_json_fails() {
        let mut intern = InternTable::new();
        let mut state = ShadowState::new();
        assert!(state.init("not json", &mut intern).is_err());
    }

    #[test]
    fn set_invalid_json_fails() {
        let mut intern = InternTable::new();
        let mut state = ShadowState::new();
        assert!(state.set("a", "not json", &mut intern).is_err());
    }

    #[test]
    fn set_through_null_promotes_to_object() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"form": null}"#, &mut intern);
        state
            .set("form.email.value", "\"hello\"", &mut intern)
            .unwrap();
        let val = state.get("form.email.value", &intern).unwrap();
        assert_eq!(val.to_json_value(&intern), serde_json::json!("hello"));
        let keys = state.get_object_keys("form", &intern).unwrap();
        assert_eq!(keys, vec!["email"]);
    }

    #[test]
    fn set_through_missing_key_creates_path() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{}"#, &mut intern);
        state
            .set("form.email.value", "\"world\"", &mut intern)
            .unwrap();
        let val = state.get("form.email.value", &intern).unwrap();
        assert_eq!(val.to_json_value(&intern), serde_json::json!("world"));
        let keys = state.get_object_keys("form", &intern).unwrap();
        assert_eq!(keys, vec!["email"]);
    }

    // --- undefined sentinel handling ---

    #[test]
    fn set_through_undefined_sentinel_promotes_to_object() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"shippingMethod": "__APEX_UNDEFINED__"}"#, &mut intern);
        state
            .set("shippingMethod.value", "\"express\"", &mut intern)
            .unwrap();
        let val = state.get("shippingMethod.value", &intern).unwrap();
        assert_eq!(val.to_json_value(&intern), serde_json::json!("express"));
        let keys = state.get_object_keys("shippingMethod", &intern).unwrap();
        assert_eq!(keys, vec!["value"]);
    }

    #[test]
    fn set_through_deeply_nested_undefined_sentinel() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"form": {"field": "__APEX_UNDEFINED__"}}"#, &mut intern);
        state
            .set("form.field.nested.value", "42", &mut intern)
            .unwrap();
        let val = state.get("form.field.nested.value", &intern).unwrap();
        assert_eq!(val.to_json_value(&intern), serde_json::json!(42));
    }

    #[test]
    fn set_regular_string_still_fails_traversal() {
        let mut intern = InternTable::new();
        let mut state = make_state(r#"{"name": "Alice"}"#, &mut intern);
        assert!(state.set("name.nested", "1", &mut intern).is_err());
    }
}
