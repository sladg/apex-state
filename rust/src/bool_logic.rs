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

/// Boolean logic DSL for conditional expressions
///
/// Supports declarative condition checking against state paths.
/// This enum uses externally tagged representation to match the JavaScript
/// format exactly: `{ "OPERATOR": [args] }`.
///
/// # Supported Operators (Phase 1 - Basic)
///
/// - `IS_EQUAL`: Compare path value to expected value
/// - `EXISTS`: Check if path value is not null/undefined
/// - `AND`: Boolean combinator for multiple conditions
///
/// # Serialization Format
///
/// The enum uses `#[serde(rename_all = "SCREAMING_SNAKE_CASE")]` to match
/// the JavaScript operator names (IS_EQUAL, EXISTS, AND).
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
        assert_eq!(
            logic,
            BoolLogic::IsEqual("user.age".to_string(), json!(25))
        );
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
}
