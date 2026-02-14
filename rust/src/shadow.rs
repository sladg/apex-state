//! Shadow state management with nested tree structure
//!
//! This module provides the core data structure for maintaining a shadow copy
//! of the application state within WASM. The shadow state enables efficient
//! path-based operations, cascading updates, and serves as the foundation for
//! BoolLogic evaluation, sync/flip graphs, and listener routing.
//!
//! The shadow state is represented as a nested ValueRepr tree that mirrors the
//! structure of JavaScript objects, supporting arbitrarily deep nesting and
//! type-safe traversal operations.
//!
//! # Example
//!
//! ```rust
//! use apex_state_wasm::shadow::ValueRepr;
//! use std::collections::HashMap;
//!
//! // Create a nested structure: { "user": { "name": "Alice", "age": 30 } }
//! let mut user_obj = HashMap::new();
//! user_obj.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
//! user_obj.insert("age".to_string(), ValueRepr::Number(30.0));
//!
//! let mut root = HashMap::new();
//! root.insert("user".to_string(), ValueRepr::Object(user_obj));
//!
//! let state = ValueRepr::Object(root);
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a value in the shadow state tree
///
/// ValueRepr is a recursive enum that can represent any JSON-like value,
/// including deeply nested objects and arrays. The Object variant uses
/// HashMap<String, ValueRepr> to enable recursive nesting.
///
/// # Variants
///
/// - `Null`: Represents a null/undefined value
/// - `Bool`: Boolean true/false
/// - `Number`: Floating-point number (f64)
/// - `String`: UTF-8 string
/// - `Array`: Ordered list of values
/// - `Object`: Key-value map with recursive nesting capability
///
/// # Traits
///
/// - `Debug`: For development and debugging output
/// - `Clone`: Required for update operations that need to copy subtrees
/// - `Serialize`: Enables conversion to JavaScript via serde-wasm-bindgen
/// - `Deserialize`: Enables conversion from JavaScript via serde-wasm-bindgen
///
/// # Examples
///
/// ```
/// use apex_state_wasm::shadow::ValueRepr;
/// use std::collections::HashMap;
///
/// // Primitive values
/// let null_val = ValueRepr::Null;
/// let bool_val = ValueRepr::Bool(true);
/// let num_val = ValueRepr::Number(42.0);
/// let str_val = ValueRepr::String("hello".to_string());
///
/// // Array
/// let arr_val = ValueRepr::Array(vec![
///     ValueRepr::Number(1.0),
///     ValueRepr::Number(2.0),
///     ValueRepr::Number(3.0),
/// ]);
///
/// // Nested object
/// let mut inner = HashMap::new();
/// inner.insert("email".to_string(), ValueRepr::String("user@example.com".to_string()));
///
/// let mut outer = HashMap::new();
/// outer.insert("profile".to_string(), ValueRepr::Object(inner));
///
/// let nested_obj = ValueRepr::Object(outer);
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ValueRepr {
    /// Represents null or undefined values
    Null,

    /// Boolean value (true or false)
    Bool(bool),

    /// Numeric value stored as f64
    ///
    /// JavaScript numbers are represented as IEEE 754 double-precision
    /// floating-point, which maps to Rust's f64 type.
    Number(f64),

    /// String value (UTF-8)
    String(String),

    /// Ordered array of values
    ///
    /// Arrays can contain any mix of ValueRepr variants, including
    /// nested arrays and objects.
    Array(Vec<ValueRepr>),

    /// Key-value object with recursive nesting
    ///
    /// The Object variant enables arbitrary nesting depth by containing
    /// a HashMap where values are themselves ValueRepr instances.
    ///
    /// This recursive structure allows representing complex nested state
    /// like: `{ "user": { "profile": { "settings": { "theme": "dark" } } } }`
    Object(HashMap<String, ValueRepr>),
}

/// Resolve a path in a ValueRepr tree
///
/// Traverses a nested ValueRepr structure following the given path.
/// Supports both object key access and array index access.
///
/// # Examples
///
/// ```
/// use apex_state_wasm::shadow::{ValueRepr, get_value};
/// use std::collections::HashMap;
///
/// let mut user = HashMap::new();
/// user.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
/// user.insert("age".to_string(), ValueRepr::Number(30.0));
///
/// let mut root = HashMap::new();
/// root.insert("user".to_string(), ValueRepr::Object(user));
///
/// let state = ValueRepr::Object(root);
/// let name = get_value(&state, &["user", "name"]);
/// assert!(matches!(name, Some(&ValueRepr::String(_))));
/// ```
pub fn get_value<'a>(root: &'a ValueRepr, path: &[&str]) -> Option<&'a ValueRepr> {
    // Empty path returns the root
    if path.is_empty() {
        return Some(root);
    }

    let mut current = root;

    for part in path {
        match current {
            ValueRepr::Object(map) => {
                // Object key access
                current = map.get(*part)?;
            }
            ValueRepr::Array(arr) => {
                // Array index access - parse part as usize
                let index: usize = part.parse().ok()?;
                current = arr.get(index)?;
            }
            // Cannot traverse through primitive types
            _ => return None,
        }
    }

    Some(current)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_traversal() {
        // Create nested structure: { "user": { "name": "Alice", "age": 30 } }
        let mut user = HashMap::new();
        user.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
        user.insert("age".to_string(), ValueRepr::Number(30.0));

        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));

        let state = ValueRepr::Object(root);

        // Valid path - nested object access
        let name = get_value(&state, &["user", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Alice"));

        let age = get_value(&state, &["user", "age"]);
        assert!(matches!(age, Some(&ValueRepr::Number(n)) if n == 30.0));

        // Valid path - get intermediate object
        let user_obj = get_value(&state, &["user"]);
        assert!(matches!(user_obj, Some(&ValueRepr::Object(_))));

        // Empty path returns root
        let root_val = get_value(&state, &[]);
        assert!(matches!(root_val, Some(&ValueRepr::Object(_))));

        // Non-existent path
        let missing = get_value(&state, &["user", "email"]);
        assert!(missing.is_none());

        let missing_root = get_value(&state, &["missing"]);
        assert!(missing_root.is_none());

        // Type mismatch - trying to traverse through a primitive
        let invalid = get_value(&state, &["user", "name", "invalid"]);
        assert!(invalid.is_none());
    }

    #[test]
    fn test_path_traversal_arrays() {
        // Create structure with arrays: { "users": [{ "name": "Alice" }, { "name": "Bob" }] }
        let mut alice = HashMap::new();
        alice.insert("name".to_string(), ValueRepr::String("Alice".to_string()));

        let mut bob = HashMap::new();
        bob.insert("name".to_string(), ValueRepr::String("Bob".to_string()));

        let users_array = ValueRepr::Array(vec![
            ValueRepr::Object(alice),
            ValueRepr::Object(bob),
        ]);

        let mut root = HashMap::new();
        root.insert("users".to_string(), users_array);

        let state = ValueRepr::Object(root);

        // Array index access
        let alice_name = get_value(&state, &["users", "0", "name"]);
        assert!(matches!(alice_name, Some(&ValueRepr::String(ref s)) if s == "Alice"));

        let bob_name = get_value(&state, &["users", "1", "name"]);
        assert!(matches!(bob_name, Some(&ValueRepr::String(ref s)) if s == "Bob"));

        // Array index out of bounds
        let out_of_bounds = get_value(&state, &["users", "2", "name"]);
        assert!(out_of_bounds.is_none());

        // Invalid array index (not a number)
        let invalid_index = get_value(&state, &["users", "invalid", "name"]);
        assert!(invalid_index.is_none());
    }

    #[test]
    fn test_path_traversal_edge_cases() {
        // Null value
        let null_state = ValueRepr::Null;
        let result = get_value(&null_state, &["any", "path"]);
        assert!(result.is_none());

        // Primitive values
        let number_state = ValueRepr::Number(42.0);
        let result = get_value(&number_state, &["path"]);
        assert!(result.is_none());

        // Empty path on primitive returns the primitive
        let result = get_value(&number_state, &[]);
        assert!(matches!(result, Some(&ValueRepr::Number(n)) if n == 42.0));

        // Deep nesting (10+ levels)
        let mut deep = ValueRepr::String("leaf".to_string());
        for i in (0..10).rev() {
            let mut map = HashMap::new();
            map.insert(format!("level{}", i), deep);
            deep = ValueRepr::Object(map);
        }

        let path: Vec<&str> = (0..10).map(|i| {
            // This is a hack to get around lifetime issues in tests
            // In real code, path strings would have appropriate lifetimes
            match i {
                0 => "level0",
                1 => "level1",
                2 => "level2",
                3 => "level3",
                4 => "level4",
                5 => "level5",
                6 => "level6",
                7 => "level7",
                8 => "level8",
                9 => "level9",
                _ => unreachable!(),
            }
        }).collect();

        let leaf = get_value(&deep, &path);
        assert!(matches!(leaf, Some(&ValueRepr::String(ref s)) if s == "leaf"));
    }
}
