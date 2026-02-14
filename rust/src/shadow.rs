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
}
