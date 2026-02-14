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

/// Update a value at a given path in the ValueRepr tree
///
/// Modifies the shadow state at the specified path. Supports three types of updates:
/// - **Leaf update**: Update a single value at a specific path
/// - **Subtree update**: Replace an entire object/array at a path
/// - **Root update**: Replace the entire state tree (empty path)
///
/// If intermediate nodes in the path don't exist, they will be created as empty Objects.
/// For array updates, the array must already exist and the index must be valid.
///
/// # Arguments
///
/// * `root` - Mutable reference to the root ValueRepr
/// * `path` - Path to the value to update (empty for root replacement)
/// * `value` - New value to set at the path
///
/// # Returns
///
/// Returns `Ok(())` on success, or `Err(String)` with an error message if the update fails.
///
/// # Examples
///
/// ```
/// use apex_state_wasm::shadow::{ValueRepr, update_value};
/// use std::collections::HashMap;
///
/// let mut state = ValueRepr::Object(HashMap::new());
///
/// // Create nested path and set value
/// update_value(&mut state, &["user", "name"], ValueRepr::String("Alice".to_string())).unwrap();
///
/// // Update existing value
/// update_value(&mut state, &["user", "name"], ValueRepr::String("Bob".to_string())).unwrap();
///
/// // Replace entire subtree
/// let mut new_user = HashMap::new();
/// new_user.insert("name".to_string(), ValueRepr::String("Charlie".to_string()));
/// new_user.insert("age".to_string(), ValueRepr::Number(30.0));
/// update_value(&mut state, &["user"], ValueRepr::Object(new_user)).unwrap();
/// ```
pub fn update_value(root: &mut ValueRepr, path: &[&str], value: ValueRepr) -> Result<(), String> {
    // Empty path means replace root entirely
    if path.is_empty() {
        *root = value;
        return Ok(());
    }

    // For non-empty paths, we need to traverse to the parent and update the final key
    let (parent_path, final_key) = path.split_at(path.len() - 1);
    let final_key = final_key[0];

    // Traverse/create path to parent
    let mut current = root;
    for part in parent_path {
        match current {
            ValueRepr::Object(map) => {
                // If key doesn't exist, create an empty object
                if !map.contains_key(*part) {
                    map.insert(part.to_string(), ValueRepr::Object(HashMap::new()));
                }
                // Get mutable reference to continue traversal
                current = map.get_mut(*part).ok_or_else(|| {
                    format!("Failed to access key '{}' after insertion", part)
                })?;
            }
            ValueRepr::Array(arr) => {
                // Array index access - parse part as usize
                let index: usize = part.parse().map_err(|_| {
                    format!("Invalid array index '{}': must be a valid number", part)
                })?;
                let arr_len = arr.len(); // Capture length before mutable borrow
                current = arr.get_mut(index).ok_or_else(|| {
                    format!(
                        "Array index {} out of bounds (length: {})",
                        index,
                        arr_len
                    )
                })?;
            }
            // Cannot traverse through primitive types
            _ => {
                return Err(format!(
                    "Cannot traverse through non-object/array type at path segment '{}'",
                    part
                ))
            }
        }
    }

    // Now update the final key/index
    match current {
        ValueRepr::Object(map) => {
            map.insert(final_key.to_string(), value);
            Ok(())
        }
        ValueRepr::Array(arr) => {
            let index: usize = final_key.parse().map_err(|_| {
                format!(
                    "Invalid array index '{}': must be a valid number",
                    final_key
                )
            })?;

            if index >= arr.len() {
                return Err(format!(
                    "Array index {} out of bounds (length: {})",
                    index,
                    arr.len()
                ));
            }

            arr[index] = value;
            Ok(())
        }
        _ => Err(format!(
            "Cannot set property '{}' on non-object/array type",
            final_key
        )),
    }
}

/// Get all affected leaf paths under a given path
///
/// When updating a subtree (object or array), this function calculates all
/// leaf paths that will be affected by the update. This is used for cascading
/// updates and listener notifications.
///
/// A "leaf" is defined as any value that is not an Object or Array
/// (i.e., Null, Bool, Number, or String).
///
/// # Arguments
///
/// * `root` - The root ValueRepr to search in
/// * `path` - Path to the subtree to analyze
///
/// # Returns
///
/// Returns a Vec<String> of all leaf paths under the given path,
/// formatted with dot notation (e.g., "user.profile.email").
/// Returns an empty Vec if the path doesn't exist.
///
/// # Examples
///
/// ```
/// use apex_state_wasm::shadow::{ValueRepr, get_affected_paths};
/// use std::collections::HashMap;
///
/// let mut profile = HashMap::new();
/// profile.insert("email".to_string(), ValueRepr::String("test@example.com".to_string()));
/// profile.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
///
/// let mut user = HashMap::new();
/// user.insert("profile".to_string(), ValueRepr::Object(profile));
///
/// let mut root = HashMap::new();
/// root.insert("user".to_string(), ValueRepr::Object(user));
///
/// let state = ValueRepr::Object(root);
/// let affected = get_affected_paths(&state, &["user", "profile"]);
/// // Returns: ["user.profile.email", "user.profile.name"]
/// ```
pub fn get_affected_paths(root: &ValueRepr, path: &[&str]) -> Vec<String> {
    // Get the value at the given path
    let value = match get_value(root, path) {
        Some(v) => v,
        None => return vec![], // Path doesn't exist, no affected paths
    };

    // Helper function to recursively collect leaf paths
    fn collect_leaves(value: &ValueRepr, current_path: &str, result: &mut Vec<String>) {
        match value {
            ValueRepr::Object(map) => {
                // For objects, recurse into each property
                for (key, child_value) in map {
                    let child_path = if current_path.is_empty() {
                        key.clone()
                    } else {
                        format!("{}.{}", current_path, key)
                    };
                    collect_leaves(child_value, &child_path, result);
                }
            }
            ValueRepr::Array(arr) => {
                // For arrays, recurse into each element
                for (index, child_value) in arr.iter().enumerate() {
                    let child_path = if current_path.is_empty() {
                        index.to_string()
                    } else {
                        format!("{}.{}", current_path, index)
                    };
                    collect_leaves(child_value, &child_path, result);
                }
            }
            // For leaf values (Null, Bool, Number, String), add to result
            _ => {
                result.push(current_path.to_string());
            }
        }
    }

    let mut affected = Vec::new();
    let base_path = path.join(".");
    collect_leaves(value, &base_path, &mut affected);
    affected
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

    #[test]
    fn test_updates() {
        // Test 1: Root update (empty path)
        let mut state = ValueRepr::Object(HashMap::new());
        let new_root = ValueRepr::String("replaced".to_string());
        assert!(update_value(&mut state, &[], new_root.clone()).is_ok());
        assert!(matches!(state, ValueRepr::String(ref s) if s == "replaced"));

        // Test 2: Leaf update - create new path
        let mut state = ValueRepr::Object(HashMap::new());
        let result = update_value(&mut state, &["user", "name"], ValueRepr::String("Alice".to_string()));
        assert!(result.is_ok());

        // Verify the value was set
        let name = get_value(&state, &["user", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Alice"));

        // Test 3: Leaf update - update existing value
        let result = update_value(&mut state, &["user", "name"], ValueRepr::String("Bob".to_string()));
        assert!(result.is_ok());

        let name = get_value(&state, &["user", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Bob"));

        // Test 4: Add another field to existing object
        let result = update_value(&mut state, &["user", "age"], ValueRepr::Number(30.0));
        assert!(result.is_ok());

        let age = get_value(&state, &["user", "age"]);
        assert!(matches!(age, Some(&ValueRepr::Number(n)) if n == 30.0));

        // Verify first field still exists
        let name = get_value(&state, &["user", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Bob"));

        // Test 5: Subtree update - replace entire object
        let mut new_user = HashMap::new();
        new_user.insert("name".to_string(), ValueRepr::String("Charlie".to_string()));
        new_user.insert("email".to_string(), ValueRepr::String("charlie@example.com".to_string()));

        let result = update_value(&mut state, &["user"], ValueRepr::Object(new_user));
        assert!(result.is_ok());

        // Verify new fields exist
        let name = get_value(&state, &["user", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Charlie"));

        let email = get_value(&state, &["user", "email"]);
        assert!(matches!(email, Some(&ValueRepr::String(ref s)) if s == "charlie@example.com"));

        // Verify old field (age) is gone
        let age = get_value(&state, &["user", "age"]);
        assert!(age.is_none());

        // Test 6: Deep nested path creation
        let mut state = ValueRepr::Object(HashMap::new());
        let result = update_value(
            &mut state,
            &["a", "b", "c", "d"],
            ValueRepr::String("deep".to_string()),
        );
        assert!(result.is_ok());

        let deep = get_value(&state, &["a", "b", "c", "d"]);
        assert!(matches!(deep, Some(&ValueRepr::String(ref s)) if s == "deep"));

        // Test 7: Array update - update existing array element
        let mut state = ValueRepr::Object(HashMap::new());
        let arr = ValueRepr::Array(vec![
            ValueRepr::Number(1.0),
            ValueRepr::Number(2.0),
            ValueRepr::Number(3.0),
        ]);

        // First set the array
        update_value(&mut state, &["numbers"], arr).unwrap();

        // Now update an element
        let result = update_value(&mut state, &["numbers", "1"], ValueRepr::Number(99.0));
        assert!(result.is_ok());

        let updated = get_value(&state, &["numbers", "1"]);
        assert!(matches!(updated, Some(&ValueRepr::Number(n)) if n == 99.0));

        // Verify other elements unchanged
        let first = get_value(&state, &["numbers", "0"]);
        assert!(matches!(first, Some(&ValueRepr::Number(n)) if n == 1.0));

        // Test 8: Array update - out of bounds should fail
        let result = update_value(&mut state, &["numbers", "10"], ValueRepr::Number(100.0));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("out of bounds"));

        // Test 9: Error - cannot traverse through primitive
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["value"], ValueRepr::Number(42.0)).unwrap();

        let result = update_value(&mut state, &["value", "nested"], ValueRepr::String("fail".to_string()));
        assert!(result.is_err());
        let err_msg = result.unwrap_err();
        assert!(
            err_msg.contains("Cannot traverse through") || err_msg.contains("Cannot set property"),
            "Unexpected error message: {}",
            err_msg
        );

        // Test 10: Error - invalid array index (not a number)
        let mut state = ValueRepr::Object(HashMap::new());
        let arr = ValueRepr::Array(vec![ValueRepr::Number(1.0)]);
        update_value(&mut state, &["arr"], arr).unwrap();

        let result = update_value(&mut state, &["arr", "invalid"], ValueRepr::Number(2.0));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Invalid array index"));

        // Test 11: Nested object in array
        let mut state = ValueRepr::Object(HashMap::new());
        let mut user1 = HashMap::new();
        user1.insert("name".to_string(), ValueRepr::String("Alice".to_string()));

        let mut user2 = HashMap::new();
        user2.insert("name".to_string(), ValueRepr::String("Bob".to_string()));

        let arr = ValueRepr::Array(vec![
            ValueRepr::Object(user1),
            ValueRepr::Object(user2),
        ]);
        update_value(&mut state, &["users"], arr).unwrap();

        // Update nested object property in array
        let result = update_value(&mut state, &["users", "0", "name"], ValueRepr::String("Alice Updated".to_string()));
        assert!(result.is_ok());

        let name = get_value(&state, &["users", "0", "name"]);
        assert!(matches!(name, Some(&ValueRepr::String(ref s)) if s == "Alice Updated"));

        // Test 12: Multiple levels of nesting with mixed objects and arrays
        let mut state = ValueRepr::Object(HashMap::new());

        // Create: { "data": { "items": [{ "value": 10 }] } }
        let mut item = HashMap::new();
        item.insert("value".to_string(), ValueRepr::Number(10.0));
        let items = ValueRepr::Array(vec![ValueRepr::Object(item)]);

        update_value(&mut state, &["data"], ValueRepr::Object(HashMap::new())).unwrap();
        update_value(&mut state, &["data", "items"], items).unwrap();

        // Update the nested value
        let result = update_value(&mut state, &["data", "items", "0", "value"], ValueRepr::Number(20.0));
        assert!(result.is_ok());

        let value = get_value(&state, &["data", "items", "0", "value"]);
        assert!(matches!(value, Some(&ValueRepr::Number(n)) if n == 20.0));
    }

    #[test]
    fn test_affected_paths() {
        // Test 1: Simple nested object
        let mut profile = HashMap::new();
        profile.insert(
            "email".to_string(),
            ValueRepr::String("test@example.com".to_string()),
        );
        profile.insert("name".to_string(), ValueRepr::String("Alice".to_string()));
        profile.insert("age".to_string(), ValueRepr::Number(30.0));

        let mut user = HashMap::new();
        user.insert("profile".to_string(), ValueRepr::Object(profile));
        user.insert("id".to_string(), ValueRepr::Number(123.0));

        let mut root = HashMap::new();
        root.insert("user".to_string(), ValueRepr::Object(user));

        let state = ValueRepr::Object(root);

        // Get affected paths for user.profile
        let mut affected = get_affected_paths(&state, &["user", "profile"]);
        affected.sort();

        assert_eq!(affected.len(), 3);
        assert!(affected.contains(&"user.profile.age".to_string()));
        assert!(affected.contains(&"user.profile.email".to_string()));
        assert!(affected.contains(&"user.profile.name".to_string()));

        // Test 2: Get affected paths for entire user object
        let mut affected = get_affected_paths(&state, &["user"]);
        affected.sort();

        assert_eq!(affected.len(), 4);
        assert!(affected.contains(&"user.id".to_string()));
        assert!(affected.contains(&"user.profile.age".to_string()));
        assert!(affected.contains(&"user.profile.email".to_string()));
        assert!(affected.contains(&"user.profile.name".to_string()));

        // Test 3: Leaf path returns just that path
        let affected = get_affected_paths(&state, &["user", "id"]);
        assert_eq!(affected.len(), 1);
        assert_eq!(affected[0], "user.id");

        // Test 4: Arrays with objects
        let mut item1 = HashMap::new();
        item1.insert("value".to_string(), ValueRepr::Number(10.0));
        item1.insert("label".to_string(), ValueRepr::String("Item 1".to_string()));

        let mut item2 = HashMap::new();
        item2.insert("value".to_string(), ValueRepr::Number(20.0));
        item2.insert("label".to_string(), ValueRepr::String("Item 2".to_string()));

        let items = ValueRepr::Array(vec![ValueRepr::Object(item1), ValueRepr::Object(item2)]);

        let mut root = HashMap::new();
        root.insert("items".to_string(), items);

        let state = ValueRepr::Object(root);

        let mut affected = get_affected_paths(&state, &["items"]);
        affected.sort();

        assert_eq!(affected.len(), 4);
        assert!(affected.contains(&"items.0.label".to_string()));
        assert!(affected.contains(&"items.0.value".to_string()));
        assert!(affected.contains(&"items.1.label".to_string()));
        assert!(affected.contains(&"items.1.value".to_string()));

        // Test 5: Non-existent path returns empty vec
        let affected = get_affected_paths(&state, &["nonexistent"]);
        assert_eq!(affected.len(), 0);

        // Test 6: Root path
        let mut simple = HashMap::new();
        simple.insert("a".to_string(), ValueRepr::Number(1.0));
        simple.insert("b".to_string(), ValueRepr::Number(2.0));

        let state = ValueRepr::Object(simple);

        let mut affected = get_affected_paths(&state, &[]);
        affected.sort();

        assert_eq!(affected.len(), 2);
        assert!(affected.contains(&"a".to_string()));
        assert!(affected.contains(&"b".to_string()));

        // Test 7: Empty object
        let state = ValueRepr::Object(HashMap::new());
        let affected = get_affected_paths(&state, &[]);
        assert_eq!(affected.len(), 0);

        // Test 8: Primitive at root
        let state = ValueRepr::String("test".to_string());
        let affected = get_affected_paths(&state, &[]);
        assert_eq!(affected.len(), 1);
        assert_eq!(affected[0], "");
    }

    #[test]
    fn test_value_repr_clone() {
        // Test that ValueRepr can be cloned properly
        let original = ValueRepr::String("test".to_string());
        let cloned = original.clone();

        assert!(matches!(cloned, ValueRepr::String(ref s) if s == "test"));

        // Test complex nested structure clone
        let mut inner = HashMap::new();
        inner.insert("key".to_string(), ValueRepr::Number(42.0));

        let mut outer = HashMap::new();
        outer.insert("nested".to_string(), ValueRepr::Object(inner));

        let original = ValueRepr::Object(outer);
        let cloned = original.clone();

        // Verify cloned structure
        let value = get_value(&cloned, &["nested", "key"]);
        assert!(matches!(value, Some(&ValueRepr::Number(n)) if n == 42.0));
    }

    #[test]
    fn test_unicode_handling() {
        // Test Unicode in keys
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["用户", "名字"], ValueRepr::String("Alice".to_string())).unwrap();

        let value = get_value(&state, &["用户", "名字"]);
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "Alice"));

        // Test Unicode in values
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["user", "name"], ValueRepr::String("Alice 🎉".to_string())).unwrap();

        let value = get_value(&state, &["user", "name"]);
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "Alice 🎉"));

        // Test emoji in keys
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["🚀", "test"], ValueRepr::Bool(true)).unwrap();

        let value = get_value(&state, &["🚀", "test"]);
        assert!(matches!(value, Some(&ValueRepr::Bool(true))));
    }

    #[test]
    fn test_empty_string_keys() {
        // Test empty string as key
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["", "value"], ValueRepr::Number(123.0)).unwrap();

        let value = get_value(&state, &["", "value"]);
        assert!(matches!(value, Some(&ValueRepr::Number(n)) if n == 123.0));

        // Test path with multiple empty strings
        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["", ""], ValueRepr::String("nested".to_string())).unwrap();

        let value = get_value(&state, &["", ""]);
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "nested"));
    }

    #[test]
    fn test_special_characters_in_keys() {
        // Test keys with dots (should be treated as literal, not path separator)
        let mut obj = HashMap::new();
        obj.insert("user.name".to_string(), ValueRepr::String("Alice".to_string()));

        let state = ValueRepr::Object(obj);

        // Access using single-segment path with dot in the key
        let value = get_value(&state, &["user.name"]);
        assert!(matches!(value, Some(&ValueRepr::String(ref s)) if s == "Alice"));

        // Test keys with special characters
        let mut state = ValueRepr::Object(HashMap::new());
        let special_keys = vec!["key-with-dash", "key_with_underscore", "key@with@at", "key#with#hash"];

        for key in special_keys {
            update_value(&mut state, &[key], ValueRepr::Bool(true)).unwrap();
            let value = get_value(&state, &[key]);
            assert!(matches!(value, Some(&ValueRepr::Bool(true))), "Failed for key: {}", key);
        }
    }

    #[test]
    fn test_deeply_nested_arrays() {
        // Test arrays nested in arrays
        let inner_array = ValueRepr::Array(vec![
            ValueRepr::Number(1.0),
            ValueRepr::Number(2.0),
        ]);

        let outer_array = ValueRepr::Array(vec![
            ValueRepr::String("first".to_string()),
            inner_array,
        ]);

        let mut root = HashMap::new();
        root.insert("nested_arrays".to_string(), outer_array);

        let state = ValueRepr::Object(root);

        // Access nested array element
        let value = get_value(&state, &["nested_arrays", "1", "0"]);
        assert!(matches!(value, Some(&ValueRepr::Number(n)) if n == 1.0));

        let value = get_value(&state, &["nested_arrays", "1", "1"]);
        assert!(matches!(value, Some(&ValueRepr::Number(n)) if n == 2.0));
    }

    #[test]
    fn test_mixed_primitive_types() {
        // Test object with all primitive types
        let mut state = ValueRepr::Object(HashMap::new());

        update_value(&mut state, &["null_val"], ValueRepr::Null).unwrap();
        update_value(&mut state, &["bool_val"], ValueRepr::Bool(true)).unwrap();
        update_value(&mut state, &["num_val"], ValueRepr::Number(42.5)).unwrap();
        update_value(&mut state, &["str_val"], ValueRepr::String("test".to_string())).unwrap();

        // Verify each type
        assert!(matches!(get_value(&state, &["null_val"]), Some(&ValueRepr::Null)));
        assert!(matches!(get_value(&state, &["bool_val"]), Some(&ValueRepr::Bool(true))));
        assert!(matches!(get_value(&state, &["num_val"]), Some(&ValueRepr::Number(n)) if n == 42.5));
        assert!(matches!(get_value(&state, &["str_val"]), Some(&ValueRepr::String(ref s)) if s == "test"));
    }

    #[test]
    fn test_large_arrays() {
        // Test array with many elements
        let large_array: Vec<ValueRepr> = (0..100)
            .map(|i| ValueRepr::Number(i as f64))
            .collect();

        let mut state = ValueRepr::Object(HashMap::new());
        update_value(&mut state, &["large"], ValueRepr::Array(large_array)).unwrap();

        // Access first, middle, and last elements
        assert!(matches!(get_value(&state, &["large", "0"]), Some(&ValueRepr::Number(n)) if n == 0.0));
        assert!(matches!(get_value(&state, &["large", "50"]), Some(&ValueRepr::Number(n)) if n == 50.0));
        assert!(matches!(get_value(&state, &["large", "99"]), Some(&ValueRepr::Number(n)) if n == 99.0));

        // Out of bounds should return None
        assert!(get_value(&state, &["large", "100"]).is_none());
        assert!(get_value(&state, &["large", "1000"]).is_none());
    }

    #[test]
    fn test_affected_paths_with_null_values() {
        // Test that null values are properly included in affected paths
        let mut obj = HashMap::new();
        obj.insert("null_field".to_string(), ValueRepr::Null);
        obj.insert("bool_field".to_string(), ValueRepr::Bool(false));

        let mut root = HashMap::new();
        root.insert("data".to_string(), ValueRepr::Object(obj));

        let state = ValueRepr::Object(root);

        let mut affected = get_affected_paths(&state, &["data"]);
        affected.sort();

        assert_eq!(affected.len(), 2);
        assert!(affected.contains(&"data.bool_field".to_string()));
        assert!(affected.contains(&"data.null_field".to_string()));
    }

    #[test]
    fn test_affected_paths_empty_arrays() {
        // Test that empty arrays have no affected paths
        let mut root = HashMap::new();
        root.insert("empty_array".to_string(), ValueRepr::Array(vec![]));

        let state = ValueRepr::Object(root);

        let affected = get_affected_paths(&state, &["empty_array"]);
        assert_eq!(affected.len(), 0);
    }

    #[test]
    fn test_update_replaces_type() {
        // Test that updating a value can change its type
        let mut state = ValueRepr::Object(HashMap::new());

        // Start with a string
        update_value(&mut state, &["field"], ValueRepr::String("text".to_string())).unwrap();
        assert!(matches!(get_value(&state, &["field"]), Some(&ValueRepr::String(_))));

        // Replace with a number
        update_value(&mut state, &["field"], ValueRepr::Number(123.0)).unwrap();
        assert!(matches!(get_value(&state, &["field"]), Some(&ValueRepr::Number(n)) if n == 123.0));

        // Replace with an object
        let mut obj = HashMap::new();
        obj.insert("nested".to_string(), ValueRepr::Bool(true));
        update_value(&mut state, &["field"], ValueRepr::Object(obj)).unwrap();
        assert!(matches!(get_value(&state, &["field"]), Some(&ValueRepr::Object(_))));

        // Replace with null
        update_value(&mut state, &["field"], ValueRepr::Null).unwrap();
        assert!(matches!(get_value(&state, &["field"]), Some(&ValueRepr::Null)));
    }

    #[test]
    fn test_number_edge_cases() {
        // Test special number values
        let mut state = ValueRepr::Object(HashMap::new());

        // Positive and negative infinity
        update_value(&mut state, &["pos_inf"], ValueRepr::Number(f64::INFINITY)).unwrap();
        update_value(&mut state, &["neg_inf"], ValueRepr::Number(f64::NEG_INFINITY)).unwrap();

        // Zero and negative zero
        update_value(&mut state, &["zero"], ValueRepr::Number(0.0)).unwrap();
        update_value(&mut state, &["neg_zero"], ValueRepr::Number(-0.0)).unwrap();

        // Very large and very small numbers
        update_value(&mut state, &["large"], ValueRepr::Number(f64::MAX)).unwrap();
        update_value(&mut state, &["small"], ValueRepr::Number(f64::MIN_POSITIVE)).unwrap();

        // Verify all values
        assert!(matches!(get_value(&state, &["pos_inf"]), Some(&ValueRepr::Number(n)) if n.is_infinite() && n.is_sign_positive()));
        assert!(matches!(get_value(&state, &["neg_inf"]), Some(&ValueRepr::Number(n)) if n.is_infinite() && n.is_sign_negative()));
        assert!(matches!(get_value(&state, &["zero"]), Some(&ValueRepr::Number(n)) if n == 0.0));
        assert!(matches!(get_value(&state, &["large"]), Some(&ValueRepr::Number(n)) if n == f64::MAX));
        assert!(matches!(get_value(&state, &["small"]), Some(&ValueRepr::Number(n)) if n == f64::MIN_POSITIVE));
    }

    #[test]
    fn test_complex_affected_paths_scenario() {
        // Test a complex nested structure with mixed types
        let mut settings = HashMap::new();
        settings.insert("theme".to_string(), ValueRepr::String("dark".to_string()));
        settings.insert("notifications".to_string(), ValueRepr::Bool(true));

        let permissions = vec![
            ValueRepr::String("read".to_string()),
            ValueRepr::String("write".to_string()),
        ];

        let mut profile = HashMap::new();
        profile.insert("settings".to_string(), ValueRepr::Object(settings));
        profile.insert("permissions".to_string(), ValueRepr::Array(permissions));
        profile.insert("score".to_string(), ValueRepr::Number(100.0));

        let mut root = HashMap::new();
        root.insert("profile".to_string(), ValueRepr::Object(profile));

        let state = ValueRepr::Object(root);

        // Get all affected paths for profile
        let mut affected = get_affected_paths(&state, &["profile"]);
        affected.sort();

        // Should include all leaf paths
        assert_eq!(affected.len(), 5);
        assert!(affected.contains(&"profile.permissions.0".to_string()));
        assert!(affected.contains(&"profile.permissions.1".to_string()));
        assert!(affected.contains(&"profile.score".to_string()));
        assert!(affected.contains(&"profile.settings.notifications".to_string()));
        assert!(affected.contains(&"profile.settings.theme".to_string()));
    }
}
