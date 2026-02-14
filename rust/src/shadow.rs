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
