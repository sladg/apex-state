//! Boolean logic DSL for conditional expressions
//!
//! This module provides a BoolLogic enum that can deserialize JavaScript boolean
//! logic expressions directly using serde tuple variants, enabling high-performance
//! WASM-based reactive condition checking.
//!
//! The enum uses externally tagged representation (serde default) which matches
//! the JavaScript format: `{ "OPERATOR": [args] }` without requiring transformation.
//!
//! # Example
//!
//! ```rust
//! use apex_state_wasm::bool_logic::BoolLogic;
//! use serde_json::json;
//!
//! // Simple equality check
//! let logic: BoolLogic = serde_json::from_value(json!({
//!     "IS_EQUAL": ["user.role", "admin"]
//! })).unwrap();
//!
//! // Combined conditions
//! let complex_logic: BoolLogic = serde_json::from_value(json!({
//!     "AND": [
//!         { "IS_EQUAL": ["user.role", "editor"] },
//!         { "EXISTS": "document.id" }
//!     ]
//! })).unwrap();
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Resolve a dot-notation path in a JSON value
///
/// Returns the value at the specified path, or None if the path doesn't exist.
///
/// # Examples
///
/// ```
/// use serde_json::json;
/// use apex_state_wasm::bool_logic::get_path_value;
///
/// let state = json!({ "user": { "role": "admin" } });
/// let value = get_path_value(&state, "user.role");
/// assert_eq!(value, Some(&json!("admin")));
/// ```
pub fn get_path_value<'a>(state: &'a Value, path: &str) -> Option<&'a Value> {
    let parts: Vec<&str> = path.split('.').collect();
    let mut current = state;

    for part in parts {
        match current {
            Value::Object(map) => {
                current = map.get(part)?;
            }
            _ => return None,
        }
    }

    Some(current)
}

/// Check if a JSON value is considered "empty"
///
/// Empty values are:
/// - null
/// - empty string ("")
/// - empty array ([])
/// - empty object ({})
///
/// Non-empty values include: non-empty strings, numbers (including 0),
/// booleans, non-empty arrays/objects
fn is_empty(value: &Value) -> bool {
    match value {
        Value::Null => true,
        Value::String(s) => s.is_empty(),
        Value::Array(arr) => arr.is_empty(),
        Value::Object(obj) => obj.is_empty(),
        _ => false,
    }
}

/// Evaluate a BoolLogic expression against a state object
///
/// Returns `true` if the condition is satisfied, `false` otherwise.
///
/// # Examples
///
/// ```
/// use serde_json::json;
/// use apex_state_wasm::bool_logic::{BoolLogic, evaluate};
///
/// let state = json!({ "user": { "role": "admin" } });
/// let logic = BoolLogic::IsEqual("user.role".to_string(), json!("admin"));
/// assert_eq!(evaluate(&logic, &state), true);
/// ```
pub fn evaluate(logic: &BoolLogic, state: &Value) -> bool {
    match logic {
        BoolLogic::IsEqual(path, expected) => {
            // Get value at path and compare to expected
            match get_path_value(state, path) {
                Some(value) => value == expected,
                None => expected.is_null(), // Missing path equals null
            }
        }
        BoolLogic::Exists(path) => {
            // Check if path exists and is not null
            match get_path_value(state, path) {
                Some(value) => !value.is_null(),
                None => false,
            }
        }
        BoolLogic::IsEmpty(path) => {
            // Check if path value is empty (null, "", [], {})
            match get_path_value(state, path) {
                Some(value) => is_empty(value),
                None => true, // Missing path is considered empty
            }
        }
        BoolLogic::And(conditions) => {
            // All conditions must be true (short-circuit on first false)
            conditions
                .iter()
                .all(|condition| evaluate(condition, state))
        }
        BoolLogic::Or(conditions) => {
            // At least one condition must be true (short-circuit on first true)
            conditions
                .iter()
                .any(|condition| evaluate(condition, state))
        }
        BoolLogic::Not(condition) => {
            // Negate the inner condition
            !evaluate(condition, state)
        }
        BoolLogic::Gt(path, threshold) => {
            // Check if numeric value > threshold
            match get_path_value(state, path) {
                Some(Value::Number(n)) => n.as_f64().map_or(false, |v| v > *threshold),
                _ => false,
            }
        }
        BoolLogic::Lt(path, threshold) => {
            // Check if numeric value < threshold
            match get_path_value(state, path) {
                Some(Value::Number(n)) => n.as_f64().map_or(false, |v| v < *threshold),
                _ => false,
            }
        }
        BoolLogic::Gte(path, threshold) => {
            // Check if numeric value >= threshold
            match get_path_value(state, path) {
                Some(Value::Number(n)) => n.as_f64().map_or(false, |v| v >= *threshold),
                _ => false,
            }
        }
        BoolLogic::Lte(path, threshold) => {
            // Check if numeric value <= threshold
            match get_path_value(state, path) {
                Some(Value::Number(n)) => n.as_f64().map_or(false, |v| v <= *threshold),
                _ => false,
            }
        }
        BoolLogic::In(path, allowed) => {
            // Check if value is in the list of allowed values
            match get_path_value(state, path) {
                Some(value) => allowed.contains(value),
                None => false,
            }
        }
    }
}

/// Boolean logic DSL for conditional expressions
///
/// Supports declarative condition checking against state paths.
/// This enum uses externally tagged representation to match the JavaScript
/// format exactly: `{ "OPERATOR": [args] }`.
///
/// # Supported Operators
///
/// **Equality & Existence:**
/// - `IS_EQUAL`: Compare path value to expected value
/// - `EXISTS`: Check if path value is not null/undefined
/// - `IS_EMPTY`: Check if value is empty (null, "", [], {})
///
/// **Boolean Combinators:**
/// - `AND`: All conditions must be true
/// - `OR`: At least one condition must be true
/// - `NOT`: Negate a condition
///
/// **Numeric Comparisons:**
/// - `GT`: Greater than
/// - `LT`: Less than
/// - `GTE`: Greater than or equal
/// - `LTE`: Less than or equal
///
/// **Inclusion:**
/// - `IN`: Check if value is in a list
///
/// # Serialization Format
///
/// The enum uses `#[serde(rename_all = "SCREAMING_SNAKE_CASE")]` to match
/// the JavaScript operator names (IS_EQUAL, EXISTS, AND, etc.).
///
/// Externally tagged format (serde default):
/// - `{ "IS_EQUAL": ["path", value] }` - tuple variant
/// - `{ "EXISTS": "path" }` - newtype variant
/// - `{ "AND": [...] }` - newtype variant with array
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum BoolLogic {
    /// Compare path value to expected value
    ///
    /// Format: `{ "IS_EQUAL": ["path.to.value", expectedValue] }`
    ///
    /// Returns `true` if the value at the path equals the expected value.
    /// Supports string, number, boolean, and null comparisons.
    ///
    /// # Example
    /// ```json
    /// { "IS_EQUAL": ["user.role", "admin"] }
    /// ```
    IsEqual(String, serde_json::Value),

    /// Check if path value exists (not null/undefined)
    ///
    /// Format: `{ "EXISTS": "path.to.value" }`
    ///
    /// Returns `true` if the value at the path is not null or undefined.
    ///
    /// # Example
    /// ```json
    /// { "EXISTS": "user.email" }
    /// ```
    Exists(String),

    /// Check if path value is empty
    ///
    /// Format: `{ "IS_EMPTY": "path.to.value" }`
    ///
    /// Returns `true` if the value is null, "", [], or {}.
    /// Missing paths are considered empty.
    ///
    /// # Example
    /// ```json
    /// { "IS_EMPTY": "user.tags" }
    /// ```
    IsEmpty(String),

    /// Boolean AND combinator
    ///
    /// Format: `{ "AND": [condition1, condition2, ...] }`
    ///
    /// Returns `true` if all nested conditions evaluate to `true`.
    /// Short-circuits on first `false` value.
    ///
    /// # Example
    /// ```json
    /// {
    ///   "AND": [
    ///     { "IS_EQUAL": ["user.role", "admin"] },
    ///     { "EXISTS": "user.email" }
    ///   ]
    /// }
    /// ```
    And(Vec<BoolLogic>),

    /// Boolean OR combinator
    ///
    /// Format: `{ "OR": [condition1, condition2, ...] }`
    ///
    /// Returns `true` if at least one nested condition evaluates to `true`.
    /// Short-circuits on first `true` value.
    ///
    /// # Example
    /// ```json
    /// {
    ///   "OR": [
    ///     { "IS_EQUAL": ["user.role", "admin"] },
    ///     { "IS_EQUAL": ["user.role", "editor"] }
    ///   ]
    /// }
    /// ```
    Or(Vec<BoolLogic>),

    /// Boolean NOT combinator
    ///
    /// Format: `{ "NOT": condition }`
    ///
    /// Returns the negation of the nested condition.
    ///
    /// # Example
    /// ```json
    /// { "NOT": { "IS_EQUAL": ["user.role", "guest"] } }
    /// ```
    Not(Box<BoolLogic>),

    /// Greater than comparison
    ///
    /// Format: `{ "GT": ["path.to.number", threshold] }`
    ///
    /// Returns `true` if the numeric value is greater than the threshold.
    /// Non-numeric values return `false`.
    ///
    /// # Example
    /// ```json
    /// { "GT": ["user.age", 18] }
    /// ```
    Gt(String, f64),

    /// Less than comparison
    ///
    /// Format: `{ "LT": ["path.to.number", threshold] }`
    ///
    /// Returns `true` if the numeric value is less than the threshold.
    /// Non-numeric values return `false`.
    ///
    /// # Example
    /// ```json
    /// { "LT": ["user.age", 65] }
    /// ```
    Lt(String, f64),

    /// Greater than or equal comparison
    ///
    /// Format: `{ "GTE": ["path.to.number", threshold] }`
    ///
    /// Returns `true` if the numeric value is >= the threshold.
    /// Non-numeric values return `false`.
    ///
    /// # Example
    /// ```json
    /// { "GTE": ["user.score", 100] }
    /// ```
    Gte(String, f64),

    /// Less than or equal comparison
    ///
    /// Format: `{ "LTE": ["path.to.number", threshold] }`
    ///
    /// Returns `true` if the numeric value is <= the threshold.
    /// Non-numeric values return `false`.
    ///
    /// # Example
    /// ```json
    /// { "LTE": ["user.score", 999] }
    /// ```
    Lte(String, f64),

    /// Inclusion check
    ///
    /// Format: `{ "IN": ["path.to.value", [allowed, values]] }`
    ///
    /// Returns `true` if the value at the path is in the list of allowed values.
    /// Missing paths return `false`.
    ///
    /// # Example
    /// ```json
    /// { "IN": ["user.role", ["admin", "editor", "moderator"]] }
    /// ```
    In(String, Vec<serde_json::Value>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_deserialize_is_equal_string() {
        let json = json!({
            "IS_EQUAL": ["user.role", "admin"]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::IsEqual("user.role".to_string(), json!("admin"))
        );
    }

    #[test]
    fn test_deserialize_is_equal_number() {
        let json = json!({
            "IS_EQUAL": ["user.age", 25]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::IsEqual("user.age".to_string(), json!(25)));
    }

    #[test]
    fn test_deserialize_is_equal_boolean() {
        let json = json!({
            "IS_EQUAL": ["user.active", true]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::IsEqual("user.active".to_string(), json!(true))
        );
    }

    #[test]
    fn test_deserialize_is_equal_null() {
        let json = json!({
            "IS_EQUAL": ["user.deleted", null]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::IsEqual("user.deleted".to_string(), json!(null))
        );
    }

    #[test]
    fn test_deserialize_exists() {
        let json = json!({
            "EXISTS": "user.email"
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::Exists("user.email".to_string()));
    }

    #[test]
    fn test_deserialize_and_simple() {
        let json = json!({
            "AND": [
                { "EXISTS": "user.email" },
                { "IS_EQUAL": ["user.role", "admin"] }
            ]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::And(vec![
                BoolLogic::Exists("user.email".to_string()),
                BoolLogic::IsEqual("user.role".to_string(), json!("admin"))
            ])
        );
    }

    #[test]
    fn test_deserialize_and_nested() {
        let json = json!({
            "AND": [
                { "EXISTS": "user.id" },
                {
                    "AND": [
                        { "IS_EQUAL": ["user.role", "admin"] },
                        { "IS_EQUAL": ["user.active", true] }
                    ]
                }
            ]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::And(vec![
                BoolLogic::Exists("user.id".to_string()),
                BoolLogic::And(vec![
                    BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
                    BoolLogic::IsEqual("user.active".to_string(), json!(true))
                ])
            ])
        );
    }

    #[test]
    fn test_serialize_is_equal() {
        let logic = BoolLogic::IsEqual("user.role".to_string(), json!("admin"));
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "IS_EQUAL": ["user.role", "admin"]
            })
        );
    }

    #[test]
    fn test_serialize_exists() {
        let logic = BoolLogic::Exists("user.email".to_string());
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "EXISTS": "user.email"
            })
        );
    }

    #[test]
    fn test_serialize_and() {
        let logic = BoolLogic::And(vec![
            BoolLogic::Exists("user.email".to_string()),
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
        ]);
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "AND": [
                    { "EXISTS": "user.email" },
                    { "IS_EQUAL": ["user.role", "admin"] }
                ]
            })
        );
    }

    #[test]
    fn test_roundtrip_complex() {
        let original = json!({
            "AND": [
                { "IS_EQUAL": ["user.role", "admin"] },
                { "EXISTS": "user.email" },
                {
                    "AND": [
                        { "IS_EQUAL": ["user.active", true] },
                        { "IS_EQUAL": ["user.verified", true] }
                    ]
                }
            ]
        });

        // Deserialize
        let logic: BoolLogic = serde_json::from_value(original.clone()).unwrap();

        // Serialize back
        let serialized = serde_json::to_value(&logic).unwrap();

        // Should match original
        assert_eq!(serialized, original);
    }

    #[test]
    fn test_invalid_format_returns_error() {
        let json = json!({
            "INVALID_OPERATOR": ["path", "value"]
        });

        let result: Result<BoolLogic, _> = serde_json::from_value(json);
        assert!(result.is_err());
    }

    #[test]
    fn test_is_equal_wrong_number_of_args_returns_error() {
        let json = json!({
            "IS_EQUAL": ["only_one_arg"]
        });

        let result: Result<BoolLogic, _> = serde_json::from_value(json);
        assert!(result.is_err());
    }

    // === Path Resolution Tests ===

    #[test]
    fn test_get_path_value_simple() {
        let state = json!({ "role": "admin" });
        let value = super::get_path_value(&state, "role");
        assert_eq!(value, Some(&json!("admin")));
    }

    #[test]
    fn test_get_path_value_nested() {
        let state = json!({
            "user": {
                "profile": {
                    "name": "Alice"
                }
            }
        });
        let value = super::get_path_value(&state, "user.profile.name");
        assert_eq!(value, Some(&json!("Alice")));
    }

    #[test]
    fn test_get_path_value_missing() {
        let state = json!({ "user": { "role": "admin" } });
        let value = super::get_path_value(&state, "user.missing");
        assert_eq!(value, None);
    }

    #[test]
    fn test_get_path_value_missing_intermediate() {
        let state = json!({ "user": { "role": "admin" } });
        let value = super::get_path_value(&state, "missing.path.here");
        assert_eq!(value, None);
    }

    // === Evaluation Tests - IS_EQUAL ===

    #[test]
    fn test_evaluate_is_equal_string_match() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::IsEqual("user.role".to_string(), json!("admin"));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_equal_string_no_match() {
        let state = json!({ "user": { "role": "user" } });
        let logic = BoolLogic::IsEqual("user.role".to_string(), json!("admin"));
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_is_equal_number_match() {
        let state = json!({ "user": { "age": 25 } });
        let logic = BoolLogic::IsEqual("user.age".to_string(), json!(25));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_equal_number_no_match() {
        let state = json!({ "user": { "age": 30 } });
        let logic = BoolLogic::IsEqual("user.age".to_string(), json!(25));
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_is_equal_boolean_match() {
        let state = json!({ "user": { "active": true } });
        let logic = BoolLogic::IsEqual("user.active".to_string(), json!(true));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_equal_null_match() {
        let state = json!({ "user": { "deleted": null } });
        let logic = BoolLogic::IsEqual("user.deleted".to_string(), json!(null));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_equal_missing_path_equals_null() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::IsEqual("user.missing".to_string(), json!(null));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_equal_missing_path_not_equals_value() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::IsEqual("user.missing".to_string(), json!("admin"));
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Evaluation Tests - EXISTS ===

    #[test]
    fn test_evaluate_exists_present() {
        let state = json!({ "user": { "email": "alice@example.com" } });
        let logic = BoolLogic::Exists("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_exists_missing() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::Exists("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_exists_null() {
        let state = json!({ "user": { "email": null } });
        let logic = BoolLogic::Exists("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_exists_empty_string() {
        let state = json!({ "user": { "email": "" } });
        let logic = BoolLogic::Exists("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_exists_zero() {
        let state = json!({ "user": { "count": 0 } });
        let logic = BoolLogic::Exists("user.count".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_exists_false() {
        let state = json!({ "user": { "active": false } });
        let logic = BoolLogic::Exists("user.active".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    // === Evaluation Tests - AND ===

    #[test]
    fn test_evaluate_and_all_true() {
        let state = json!({
            "user": {
                "role": "admin",
                "active": true,
                "email": "alice@example.com"
            }
        });
        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
            BoolLogic::Exists("user.email".to_string()),
        ]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_and_one_false() {
        let state = json!({
            "user": {
                "role": "user",
                "active": true,
                "email": "alice@example.com"
            }
        });
        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
            BoolLogic::Exists("user.email".to_string()),
        ]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_and_all_false() {
        let state = json!({
            "user": {
                "role": "user",
                "active": false
            }
        });
        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
            BoolLogic::Exists("user.email".to_string()),
        ]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_and_empty() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::And(vec![]);
        assert_eq!(super::evaluate(&logic, &state), true); // Empty AND is true (vacuous truth)
    }

    #[test]
    fn test_evaluate_and_nested() {
        let state = json!({
            "user": {
                "role": "admin",
                "active": true,
                "email": "alice@example.com",
                "verified": true
            }
        });
        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::And(vec![
                BoolLogic::IsEqual("user.active".to_string(), json!(true)),
                BoolLogic::IsEqual("user.verified".to_string(), json!(true)),
            ]),
            BoolLogic::Exists("user.email".to_string()),
        ]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_and_nested_inner_false() {
        let state = json!({
            "user": {
                "role": "admin",
                "active": true,
                "email": "alice@example.com",
                "verified": false
            }
        });
        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::And(vec![
                BoolLogic::IsEqual("user.active".to_string(), json!(true)),
                BoolLogic::IsEqual("user.verified".to_string(), json!(true)),
            ]),
            BoolLogic::Exists("user.email".to_string()),
        ]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Complex Integration Tests ===

    #[test]
    fn test_evaluate_complex_real_world_scenario() {
        let state = json!({
            "user": {
                "id": 123,
                "role": "editor",
                "active": true,
                "email": "alice@example.com",
                "profile": {
                    "verified": true,
                    "name": "Alice"
                }
            },
            "document": {
                "id": "doc-456",
                "status": "draft"
            }
        });

        let logic = BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
            BoolLogic::Exists("document.id".to_string()),
            BoolLogic::And(vec![
                BoolLogic::IsEqual("user.active".to_string(), json!(true)),
                BoolLogic::IsEqual("user.profile.verified".to_string(), json!(true)),
            ]),
        ]);

        assert_eq!(super::evaluate(&logic, &state), true);
    }

    // === Evaluation Tests - OR ===

    #[test]
    fn test_evaluate_or_all_true() {
        let state = json!({
            "user": {
                "role": "admin",
                "active": true
            }
        });
        let logic = BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
        ]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_or_first_true() {
        let state = json!({
            "user": {
                "role": "admin",
                "active": false
            }
        });
        let logic = BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
        ]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_or_second_true() {
        let state = json!({
            "user": {
                "role": "user",
                "active": true
            }
        });
        let logic = BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
        ]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_or_all_false() {
        let state = json!({
            "user": {
                "role": "user",
                "active": false
            }
        });
        let logic = BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.active".to_string(), json!(true)),
        ]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_or_empty() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::Or(vec![]);
        assert_eq!(super::evaluate(&logic, &state), false); // Empty OR is false
    }

    // === Evaluation Tests - NOT ===

    #[test]
    fn test_evaluate_not_true_becomes_false() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "user.role".to_string(),
            json!("admin"),
        )));
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_not_false_becomes_true() {
        let state = json!({ "user": { "role": "user" } });
        let logic = BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "user.role".to_string(),
            json!("admin"),
        )));
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_not_nested() {
        let state = json!({ "user": { "active": true } });
        let logic = BoolLogic::Not(Box::new(BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "user.active".to_string(),
            json!(true),
        )))));
        assert_eq!(super::evaluate(&logic, &state), true); // Double negation
    }

    // === Evaluation Tests - GT (Greater Than) ===

    #[test]
    fn test_evaluate_gt_true() {
        let state = json!({ "user": { "age": 25 } });
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_gt_false() {
        let state = json!({ "user": { "age": 15 } });
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_gt_equal() {
        let state = json!({ "user": { "age": 18 } });
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_gt_non_numeric() {
        let state = json!({ "user": { "age": "25" } });
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_gt_missing_path() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Evaluation Tests - LT (Less Than) ===

    #[test]
    fn test_evaluate_lt_true() {
        let state = json!({ "user": { "age": 15 } });
        let logic = BoolLogic::Lt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_lt_false() {
        let state = json!({ "user": { "age": 25 } });
        let logic = BoolLogic::Lt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_lt_equal() {
        let state = json!({ "user": { "age": 18 } });
        let logic = BoolLogic::Lt("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Evaluation Tests - GTE (Greater Than or Equal) ===

    #[test]
    fn test_evaluate_gte_greater() {
        let state = json!({ "user": { "age": 25 } });
        let logic = BoolLogic::Gte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_gte_equal() {
        let state = json!({ "user": { "age": 18 } });
        let logic = BoolLogic::Gte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_gte_false() {
        let state = json!({ "user": { "age": 15 } });
        let logic = BoolLogic::Gte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Evaluation Tests - LTE (Less Than or Equal) ===

    #[test]
    fn test_evaluate_lte_less() {
        let state = json!({ "user": { "age": 15 } });
        let logic = BoolLogic::Lte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_lte_equal() {
        let state = json!({ "user": { "age": 18 } });
        let logic = BoolLogic::Lte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_lte_false() {
        let state = json!({ "user": { "age": 25 } });
        let logic = BoolLogic::Lte("user.age".to_string(), 18.0);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Evaluation Tests - IS_EMPTY ===

    #[test]
    fn test_evaluate_is_empty_null() {
        let state = json!({ "user": { "email": null } });
        let logic = BoolLogic::IsEmpty("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_empty_empty_string() {
        let state = json!({ "user": { "email": "" } });
        let logic = BoolLogic::IsEmpty("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_empty_empty_array() {
        let state = json!({ "user": { "tags": [] } });
        let logic = BoolLogic::IsEmpty("user.tags".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_empty_empty_object() {
        let state = json!({ "user": { "meta": {} } });
        let logic = BoolLogic::IsEmpty("user.meta".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_empty_missing_path() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::IsEmpty("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_is_empty_non_empty_string() {
        let state = json!({ "user": { "email": "alice@example.com" } });
        let logic = BoolLogic::IsEmpty("user.email".to_string());
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_is_empty_non_empty_array() {
        let state = json!({ "user": { "tags": ["admin"] } });
        let logic = BoolLogic::IsEmpty("user.tags".to_string());
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_is_empty_non_empty_object() {
        let state = json!({ "user": { "meta": { "key": "value" } } });
        let logic = BoolLogic::IsEmpty("user.meta".to_string());
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_is_empty_number() {
        let state = json!({ "user": { "age": 0 } });
        let logic = BoolLogic::IsEmpty("user.age".to_string());
        assert_eq!(super::evaluate(&logic, &state), false); // Numbers are never empty
    }

    #[test]
    fn test_evaluate_is_empty_boolean() {
        let state = json!({ "user": { "active": false } });
        let logic = BoolLogic::IsEmpty("user.active".to_string());
        assert_eq!(super::evaluate(&logic, &state), false); // Booleans are never empty
    }

    // === Evaluation Tests - IN ===

    #[test]
    fn test_evaluate_in_string_match() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::In(
            "user.role".to_string(),
            vec![json!("admin"), json!("editor"), json!("moderator")],
        );
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_in_string_no_match() {
        let state = json!({ "user": { "role": "guest" } });
        let logic = BoolLogic::In(
            "user.role".to_string(),
            vec![json!("admin"), json!("editor"), json!("moderator")],
        );
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_in_number_match() {
        let state = json!({ "user": { "status": 200 } });
        let logic = BoolLogic::In("user.status".to_string(), vec![json!(200), json!(201)]);
        assert_eq!(super::evaluate(&logic, &state), true);
    }

    #[test]
    fn test_evaluate_in_number_no_match() {
        let state = json!({ "user": { "status": 404 } });
        let logic = BoolLogic::In("user.status".to_string(), vec![json!(200), json!(201)]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_in_missing_path() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::In("user.status".to_string(), vec![json!(200), json!(201)]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    #[test]
    fn test_evaluate_in_empty_list() {
        let state = json!({ "user": { "role": "admin" } });
        let logic = BoolLogic::In("user.role".to_string(), vec![]);
        assert_eq!(super::evaluate(&logic, &state), false);
    }

    // === Serialization/Deserialization Tests for New Operators ===

    #[test]
    fn test_deserialize_or() {
        let json = json!({
            "OR": [
                { "IS_EQUAL": ["user.role", "admin"] },
                { "IS_EQUAL": ["user.role", "editor"] }
            ]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::Or(vec![
                BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
                BoolLogic::IsEqual("user.role".to_string(), json!("editor"))
            ])
        );
    }

    #[test]
    fn test_deserialize_not() {
        let json = json!({
            "NOT": { "IS_EQUAL": ["user.role", "guest"] }
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::Not(Box::new(BoolLogic::IsEqual(
                "user.role".to_string(),
                json!("guest")
            )))
        );
    }

    #[test]
    fn test_deserialize_gt() {
        let json = json!({
            "GT": ["user.age", 18]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::Gt("user.age".to_string(), 18.0));
    }

    #[test]
    fn test_deserialize_lt() {
        let json = json!({
            "LT": ["user.age", 65]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::Lt("user.age".to_string(), 65.0));
    }

    #[test]
    fn test_deserialize_gte() {
        let json = json!({
            "GTE": ["user.score", 100]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::Gte("user.score".to_string(), 100.0));
    }

    #[test]
    fn test_deserialize_lte() {
        let json = json!({
            "LTE": ["user.score", 999]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::Lte("user.score".to_string(), 999.0));
    }

    #[test]
    fn test_deserialize_is_empty() {
        let json = json!({
            "IS_EMPTY": "user.tags"
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(logic, BoolLogic::IsEmpty("user.tags".to_string()));
    }

    #[test]
    fn test_deserialize_in() {
        let json = json!({
            "IN": ["user.role", ["admin", "editor", "moderator"]]
        });

        let logic: BoolLogic = serde_json::from_value(json).unwrap();
        assert_eq!(
            logic,
            BoolLogic::In(
                "user.role".to_string(),
                vec![json!("admin"), json!("editor"), json!("moderator")]
            )
        );
    }

    #[test]
    fn test_serialize_or() {
        let logic = BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        ]);
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "OR": [
                    { "IS_EQUAL": ["user.role", "admin"] },
                    { "IS_EQUAL": ["user.role", "editor"] }
                ]
            })
        );
    }

    #[test]
    fn test_serialize_not() {
        let logic = BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "user.role".to_string(),
            json!("guest"),
        )));
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "NOT": { "IS_EQUAL": ["user.role", "guest"] }
            })
        );
    }

    #[test]
    fn test_serialize_gt() {
        let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "GT": ["user.age", 18.0]
            })
        );
    }

    #[test]
    fn test_serialize_is_empty() {
        let logic = BoolLogic::IsEmpty("user.tags".to_string());
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "IS_EMPTY": "user.tags"
            })
        );
    }

    #[test]
    fn test_serialize_in() {
        let logic = BoolLogic::In(
            "user.role".to_string(),
            vec![json!("admin"), json!("editor")],
        );
        let json = serde_json::to_value(&logic).unwrap();

        assert_eq!(
            json,
            json!({
                "IN": ["user.role", ["admin", "editor"]]
            })
        );
    }

    // === Complex Integration Test with New Operators ===

    #[test]
    fn test_complex_logic_with_all_operators() {
        let state = json!({
            "user": {
                "role": "editor",
                "age": 25,
                "score": 150,
                "tags": ["premium"],
                "bio": ""
            }
        });

        let logic = BoolLogic::And(vec![
            // Role is editor OR admin
            BoolLogic::Or(vec![
                BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
                BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
            ]),
            // Age >= 18 AND < 65
            BoolLogic::And(vec![
                BoolLogic::Gte("user.age".to_string(), 18.0),
                BoolLogic::Lt("user.age".to_string(), 65.0),
            ]),
            // Score > 100
            BoolLogic::Gt("user.score".to_string(), 100.0),
            // Tags are not empty
            BoolLogic::Not(Box::new(BoolLogic::IsEmpty("user.tags".to_string()))),
            // Role is in allowed list
            BoolLogic::In(
                "user.role".to_string(),
                vec![json!("admin"), json!("editor"), json!("moderator")],
            ),
        ]);

        assert_eq!(super::evaluate(&logic, &state), true);
    }
}
