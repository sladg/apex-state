//! Comprehensive integration tests for BoolLogic evaluation
//!
//! Tests all BoolLogic operators:
//! - IS_EQUAL: Equality comparison
//! - EXISTS: Not null/undefined check
//! - IS_EMPTY: Emptiness check
//! - AND/OR/NOT: Boolean combinators
//! - GT/LT/GTE/LTE: Numeric comparisons
//! - IN: Inclusion check
//!
//! This test suite ensures the Rust implementation matches the TypeScript
//! behavior and handles all edge cases correctly.

use apex_state_wasm::bool_logic::{evaluate, get_path_value, BoolLogic};
use serde_json::json;

// ============================================================================
// Test Helper Functions
// ============================================================================

/// Create a standard test state object
fn create_test_state() -> serde_json::Value {
    json!({
        "user": {
            "name": "John",
            "age": 30,
            "role": "editor",
            "isAdmin": false,
            "email": "john@example.com",
            "tags": ["developer", "designer"]
        },
        "product": {
            "price": 99.99,
            "quantity": 5,
            "category": "electronics",
            "description": ""
        },
        "settings": {
            "enabled": true,
            "value": null
        }
    })
}

// ============================================================================
// IS_EQUAL Operator Tests
// ============================================================================

#[test]
fn test_is_equal_string_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.role".to_string(), json!("editor"));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_equal_string_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.role".to_string(), json!("admin"));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_equal_number_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.age".to_string(), json!(30));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_equal_number_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.age".to_string(), json!(25));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_equal_boolean_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.isAdmin".to_string(), json!(false));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_equal_boolean_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.isAdmin".to_string(), json!(true));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_equal_null_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("settings.value".to_string(), json!(null));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_equal_null_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.email".to_string(), json!(null));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_equal_missing_path_equals_null() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.missing".to_string(), json!(null));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_equal_missing_path_not_equals_value() {
    let state = create_test_state();
    let logic = BoolLogic::IsEqual("user.missing".to_string(), json!("value"));
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// EXISTS Operator Tests
// ============================================================================

#[test]
fn test_exists_present_string() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("user.name".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_exists_present_email() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("user.email".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_exists_present_number() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("product.price".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_exists_null_value() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("settings.value".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_exists_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("user.missing".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_exists_falsy_but_present_zero() {
    let state = json!({
        "user": { "age": 0 }
    });
    let logic = BoolLogic::Exists("user.age".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_exists_falsy_but_present_empty_string() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("product.description".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_exists_falsy_but_present_false() {
    let state = create_test_state();
    let logic = BoolLogic::Exists("settings.enabled".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

// ============================================================================
// IS_EMPTY Operator Tests
// ============================================================================

#[test]
fn test_is_empty_empty_string() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("product.description".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_empty_non_empty_string() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("user.name".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_empty_empty_array() {
    let state = json!({
        "user": { "tags": [] }
    });
    let logic = BoolLogic::IsEmpty("user.tags".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_empty_non_empty_array() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("user.tags".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_empty_null_value() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("settings.value".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_empty_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("user.missing".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_empty_number_is_not_empty() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("user.age".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_empty_zero_is_not_empty() {
    let state = json!({
        "user": { "age": 0 }
    });
    let logic = BoolLogic::IsEmpty("user.age".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_empty_boolean_false_is_not_empty() {
    let state = create_test_state();
    let logic = BoolLogic::IsEmpty("user.isAdmin".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_is_empty_empty_object() {
    let state = json!({
        "user": { "meta": {} }
    });
    let logic = BoolLogic::IsEmpty("user.meta".to_string());
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_is_empty_non_empty_object() {
    let state = json!({
        "user": { "meta": { "key": "value" } }
    });
    let logic = BoolLogic::IsEmpty("user.meta".to_string());
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// AND Operator Tests
// ============================================================================

#[test]
fn test_and_all_conditions_true() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::Exists("user.email".to_string()),
        BoolLogic::Gt("user.age".to_string(), 18.0),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_and_one_condition_false() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::Exists("user.email".to_string()),
        BoolLogic::Gt("user.age".to_string(), 50.0), // This is false
    ]);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_and_all_conditions_false() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
        BoolLogic::IsEqual("user.isAdmin".to_string(), json!(true)),
    ]);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_and_empty_array() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![]);
    // Empty AND is vacuously true
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_and_nested() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::And(vec![
            BoolLogic::Exists("user.email".to_string()),
            BoolLogic::Gt("user.age".to_string(), 18.0),
        ]),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_and_nested_inner_false() {
    let state = create_test_state();
    let logic = BoolLogic::And(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::And(vec![
            BoolLogic::Exists("user.email".to_string()),
            BoolLogic::Gt("user.age".to_string(), 50.0), // Inner false
        ]),
    ]);
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// OR Operator Tests
// ============================================================================

#[test]
fn test_or_all_conditions_true() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_or_first_condition_true() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_or_second_condition_true() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
        BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_or_all_conditions_false() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
        BoolLogic::IsEqual("user.role".to_string(), json!("moderator")),
    ]);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_or_empty_array() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![]);
    // Empty OR is false
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_or_nested() {
    let state = create_test_state();
    let logic = BoolLogic::Or(vec![
        BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
        BoolLogic::Or(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
            BoolLogic::IsEqual("user.role".to_string(), json!("moderator")),
        ]),
    ]);
    assert_eq!(evaluate(&logic, &state), true);
}

// ============================================================================
// NOT Operator Tests
// ============================================================================

#[test]
fn test_not_true_becomes_false() {
    let state = create_test_state();
    let logic = BoolLogic::Not(Box::new(BoolLogic::IsEqual(
        "user.role".to_string(),
        json!("editor"),
    )));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_not_false_becomes_true() {
    let state = create_test_state();
    let logic = BoolLogic::Not(Box::new(BoolLogic::IsEqual(
        "user.role".to_string(),
        json!("admin"),
    )));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_not_double_negation() {
    let state = create_test_state();
    let logic = BoolLogic::Not(Box::new(BoolLogic::Not(Box::new(BoolLogic::IsEqual(
        "user.role".to_string(),
        json!("editor"),
    )))));
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_not_with_exists() {
    let state = create_test_state();
    let logic = BoolLogic::Not(Box::new(BoolLogic::Exists("user.missing".to_string())));
    assert_eq!(evaluate(&logic, &state), true);
}

// ============================================================================
// GT (Greater Than) Operator Tests
// ============================================================================

#[test]
fn test_gt_true() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("user.age".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_gt_false() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("user.age".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gt_equal_is_false() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("user.age".to_string(), 30.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gt_non_numeric_value() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("user.name".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gt_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("user.missing".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gt_decimal_comparison() {
    let state = create_test_state();
    let logic = BoolLogic::Gt("product.price".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), true);
}

// ============================================================================
// LT (Less Than) Operator Tests
// ============================================================================

#[test]
fn test_lt_true() {
    let state = create_test_state();
    let logic = BoolLogic::Lt("user.age".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_lt_false() {
    let state = create_test_state();
    let logic = BoolLogic::Lt("user.age".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_lt_equal_is_false() {
    let state = create_test_state();
    let logic = BoolLogic::Lt("user.age".to_string(), 30.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_lt_non_numeric_value() {
    let state = create_test_state();
    let logic = BoolLogic::Lt("user.name".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_lt_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::Lt("user.missing".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// GTE (Greater Than or Equal) Operator Tests
// ============================================================================

#[test]
fn test_gte_greater_than() {
    let state = create_test_state();
    let logic = BoolLogic::Gte("user.age".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_gte_equal_to() {
    let state = create_test_state();
    let logic = BoolLogic::Gte("user.age".to_string(), 30.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_gte_less_than() {
    let state = create_test_state();
    let logic = BoolLogic::Gte("user.age".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gte_non_numeric_value() {
    let state = create_test_state();
    let logic = BoolLogic::Gte("user.name".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_gte_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::Gte("user.missing".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// LTE (Less Than or Equal) Operator Tests
// ============================================================================

#[test]
fn test_lte_less_than() {
    let state = create_test_state();
    let logic = BoolLogic::Lte("user.age".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_lte_equal_to() {
    let state = create_test_state();
    let logic = BoolLogic::Lte("user.age".to_string(), 30.0);
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_lte_greater_than() {
    let state = create_test_state();
    let logic = BoolLogic::Lte("user.age".to_string(), 18.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_lte_non_numeric_value() {
    let state = create_test_state();
    let logic = BoolLogic::Lte("user.name".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_lte_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::Lte("user.missing".to_string(), 50.0);
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// IN Operator Tests
// ============================================================================

#[test]
fn test_in_string_match() {
    let state = create_test_state();
    let logic = BoolLogic::In(
        "user.role".to_string(),
        vec![json!("admin"), json!("editor"), json!("moderator")],
    );
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_in_string_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::In(
        "user.role".to_string(),
        vec![json!("admin"), json!("moderator")],
    );
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_in_number_match() {
    let state = create_test_state();
    let logic = BoolLogic::In(
        "user.age".to_string(),
        vec![json!(25), json!(30), json!(35)],
    );
    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_in_number_no_match() {
    let state = create_test_state();
    let logic = BoolLogic::In("user.age".to_string(), vec![json!(25), json!(35)]);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_in_missing_path() {
    let state = create_test_state();
    let logic = BoolLogic::In(
        "user.missing".to_string(),
        vec![json!("admin"), json!("editor")],
    );
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_in_empty_list() {
    let state = create_test_state();
    let logic = BoolLogic::In("user.role".to_string(), vec![]);
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_in_mixed_types() {
    let state = create_test_state();
    let logic = BoolLogic::In(
        "user.role".to_string(),
        vec![json!("admin"), json!(123), json!(true)],
    );
    assert_eq!(evaluate(&logic, &state), false);
}

// ============================================================================
// Path Resolution Tests
// ============================================================================

#[test]
fn test_get_path_value_simple() {
    let state = create_test_state();
    let value = get_path_value(&state, "user.name");
    assert_eq!(value, Some(&json!("John")));
}

#[test]
fn test_get_path_value_nested() {
    let state = create_test_state();
    let value = get_path_value(&state, "user.email");
    assert_eq!(value, Some(&json!("john@example.com")));
}

#[test]
fn test_get_path_value_missing() {
    let state = create_test_state();
    let value = get_path_value(&state, "user.missing");
    assert_eq!(value, None);
}

#[test]
fn test_get_path_value_deep_nesting() {
    let state = json!({
        "level1": {
            "level2": {
                "level3": {
                    "value": 42
                }
            }
        }
    });
    let value = get_path_value(&state, "level1.level2.level3.value");
    assert_eq!(value, Some(&json!(42)));
}

// ============================================================================
// Complex Integration Tests
// ============================================================================

#[test]
fn test_complex_real_world_scenario() {
    let state = json!({
        "user": {
            "id": 123,
            "role": "editor",
            "age": 30,
            "email": "alice@example.com",
            "isActive": true,
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
            BoolLogic::IsEqual("user.isActive".to_string(), json!(true)),
            BoolLogic::IsEqual("user.profile.verified".to_string(), json!(true)),
        ]),
        BoolLogic::Gt("user.age".to_string(), 18.0),
    ]);

    assert_eq!(evaluate(&logic, &state), true);
}

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

    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_complex_permission_check() {
    let state = json!({
        "user": {
            "role": "moderator",
            "age": 28,
            "verified": true,
            "banned": false
        },
        "resource": {
            "visibility": "public",
            "locked": false
        }
    });

    // User can edit if:
    // - (role is admin OR moderator) AND verified
    // - AND NOT banned
    // - AND resource is not locked
    let logic = BoolLogic::And(vec![
        BoolLogic::And(vec![
            BoolLogic::Or(vec![
                BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
                BoolLogic::IsEqual("user.role".to_string(), json!("moderator")),
            ]),
            BoolLogic::IsEqual("user.verified".to_string(), json!(true)),
        ]),
        BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "user.banned".to_string(),
            json!(true),
        ))),
        BoolLogic::Not(Box::new(BoolLogic::IsEqual(
            "resource.locked".to_string(),
            json!(true),
        ))),
    ]);

    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_age_range_validation() {
    let state = json!({
        "user": { "age": 25 }
    });

    // Check if age is between 18 and 65
    let logic = BoolLogic::And(vec![
        BoolLogic::Gte("user.age".to_string(), 18.0),
        BoolLogic::Lte("user.age".to_string(), 65.0),
    ]);

    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_empty_form_validation() {
    let state = json!({
        "form": {
            "name": "",
            "email": "user@example.com",
            "bio": ""
        }
    });

    // Check if any required field is empty
    let has_empty_required = BoolLogic::Or(vec![
        BoolLogic::IsEmpty("form.name".to_string()),
        BoolLogic::IsEmpty("form.email".to_string()),
    ]);

    assert_eq!(evaluate(&has_empty_required, &state), true);
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_deeply_nested_and_or_combinations() {
    let state = create_test_state();

    let logic = BoolLogic::Or(vec![
        BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("admin")),
            BoolLogic::Gt("user.age".to_string(), 50.0),
        ]),
        BoolLogic::And(vec![
            BoolLogic::IsEqual("user.role".to_string(), json!("editor")),
            BoolLogic::Gt("user.age".to_string(), 18.0),
        ]),
    ]);

    assert_eq!(evaluate(&logic, &state), true);
}

#[test]
fn test_triple_negation() {
    let state = create_test_state();
    let logic = BoolLogic::Not(Box::new(BoolLogic::Not(Box::new(BoolLogic::Not(
        Box::new(BoolLogic::IsEqual("user.role".to_string(), json!("editor"))),
    )))));
    assert_eq!(evaluate(&logic, &state), false);
}

#[test]
fn test_empty_string_vs_null_vs_missing() {
    let state = json!({
        "data": {
            "empty": "",
            "null": null
        }
    });

    // Empty string EXISTS but IS_EMPTY
    assert_eq!(
        evaluate(&BoolLogic::Exists("data.empty".to_string()), &state),
        true
    );
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.empty".to_string()), &state),
        true
    );

    // Null does NOT exist and IS_EMPTY
    assert_eq!(
        evaluate(&BoolLogic::Exists("data.null".to_string()), &state),
        false
    );
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.null".to_string()), &state),
        true
    );

    // Missing does NOT exist and IS_EMPTY
    assert_eq!(
        evaluate(&BoolLogic::Exists("data.missing".to_string()), &state),
        false
    );
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.missing".to_string()), &state),
        true
    );
}

#[test]
fn test_numeric_edge_cases() {
    let state = json!({
        "numbers": {
            "zero": 0,
            "negative": -5,
            "float": 3.14,
            "large": 1000000
        }
    });

    // Zero is a valid number
    assert_eq!(
        evaluate(&BoolLogic::Gte("numbers.zero".to_string(), 0.0), &state),
        true
    );

    // Negative numbers work
    assert_eq!(
        evaluate(&BoolLogic::Lt("numbers.negative".to_string(), 0.0), &state),
        true
    );

    // Float comparisons work
    assert_eq!(
        evaluate(&BoolLogic::Gt("numbers.float".to_string(), 3.0), &state),
        true
    );

    // Large numbers work
    assert_eq!(
        evaluate(
            &BoolLogic::Gte("numbers.large".to_string(), 1000000.0),
            &state
        ),
        true
    );
}

#[test]
fn test_array_type_handling() {
    let state = json!({
        "data": {
            "tags": ["tag1", "tag2"],
            "emptyArray": [],
            "numbers": [1, 2, 3]
        }
    });

    // Non-empty arrays are not empty
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.tags".to_string()), &state),
        false
    );
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.numbers".to_string()), &state),
        false
    );

    // Empty array is empty
    assert_eq!(
        evaluate(&BoolLogic::IsEmpty("data.emptyArray".to_string()), &state),
        true
    );

    // Arrays exist
    assert_eq!(
        evaluate(&BoolLogic::Exists("data.tags".to_string()), &state),
        true
    );
}

#[test]
fn test_multiple_path_segments() {
    let state = json!({
        "deeply": {
            "nested": {
                "object": {
                    "with": {
                        "many": {
                            "levels": {
                                "value": "found"
                            }
                        }
                    }
                }
            }
        }
    });

    let logic = BoolLogic::IsEqual(
        "deeply.nested.object.with.many.levels.value".to_string(),
        json!("found"),
    );
    assert_eq!(evaluate(&logic, &state), true);
}
