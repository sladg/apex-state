//! Diff engine for comparing changes against shadow state.
//!
//! Filters out no-op changes by comparing incoming values against the nested
//! shadow state. Used by streaming data gateway to minimize pipeline work.

use crate::change::Change;
use crate::shadow::{ShadowState, ValueRepr};

/// Compare two f64 values for bitwise equality (handles NaN, -0/+0).
fn floats_equal(a: f64, b: f64) -> bool {
    // NaN != NaN (IEEE 754 semantics)
    if a.is_nan() && b.is_nan() {
        return false;
    }
    // -0.0 != +0.0 (bitwise comparison)
    if a == 0.0 && b == 0.0 {
        return a.to_bits() == b.to_bits();
    }
    // Normal comparison
    a == b
}

/// Check if a new value is different from the current shadow state value.
///
/// Diff rules:
/// - None (path doesn't exist) → always different
/// - Number → bitwise equal (handles NaN, -0/+0)
/// - Bool → `==`
/// - String → `==`
/// - Null → type match
/// - Object → always different (no deep comparison)
/// - Array → always different (no deep comparison)
/// - Type mismatch → always different
pub(crate) fn is_different(current: &Option<&ValueRepr>, new: &ValueRepr) -> bool {
    match (current, new) {
        (None, _) => true, // First time seeing this path
        (Some(ValueRepr::Number(a)), ValueRepr::Number(b)) => !floats_equal(*a, *b),
        (Some(ValueRepr::Bool(a)), ValueRepr::Bool(b)) => a != b,
        (Some(ValueRepr::String(a)), ValueRepr::String(b)) => a != b,
        (Some(ValueRepr::Null), ValueRepr::Null) => false,
        (Some(ValueRepr::Object(_)), ValueRepr::Object(_)) => {
            // Objects always pass through (no deep comparison)
            true
        }
        (Some(ValueRepr::Array(_)), ValueRepr::Array(_)) => {
            // Arrays always pass through (no deep comparison)
            true
        }
        _ => true, // Type mismatch = different
    }
}

/// Diff a batch of changes against shadow state.
///
/// Returns only changes where the value differs from the current shadow state.
/// Primitives are compared by value, objects/arrays always pass through.
///
/// Zero allocations on the fast path when all changes are no-ops.
pub(crate) fn diff_changes(shadow: &ShadowState, changes: &[Change]) -> Vec<Change> {
    let mut genuine_changes = Vec::new();

    for change in changes {
        // Get current value from shadow state
        let current = shadow.get(&change.path);

        // Parse incoming value
        let new_value: ValueRepr = match serde_json::from_str(&change.value_json) {
            Ok(v) => v,
            Err(_) => {
                // Can't parse → treat as different (keep the change)
                genuine_changes.push(change.clone());
                continue;
            }
        };

        // Compare
        if is_different(&current, &new_value) {
            genuine_changes.push(change.clone());
        }
    }

    genuine_changes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::change::{ChangeKind, Lineage};

    fn make_shadow(json: &str) -> ShadowState {
        let mut s = ShadowState::new();
        s.init(json).unwrap();
        s
    }

    fn make_change(path: &str, value_json: &str) -> Change {
        Change {
            path: path.to_owned(),
            value_json: value_json.to_owned(),
            kind: ChangeKind::Real,
            lineage: Lineage::Input,
            audit: None,
            ..Default::default()
        }
    }

    // --- floats_equal ---

    #[test]
    fn floats_equal_normal_values() {
        assert!(floats_equal(42.0, 42.0));
        assert!(!floats_equal(42.0, 43.0));
    }

    #[test]
    fn floats_equal_nan() {
        // NaN != NaN (IEEE 754)
        assert!(!floats_equal(f64::NAN, f64::NAN));
    }

    #[test]
    fn floats_equal_zero() {
        // -0.0 != +0.0 (bitwise)
        assert!(!floats_equal(-0.0, 0.0));
        assert!(floats_equal(0.0, 0.0));
        assert!(floats_equal(-0.0, -0.0));
    }

    // --- is_different ---

    #[test]
    fn is_different_none_always_true() {
        assert!(is_different(&None, &ValueRepr::Number(42.0)));
        assert!(is_different(&None, &ValueRepr::String("test".to_owned())));
    }

    #[test]
    fn is_different_number() {
        let current = Some(&ValueRepr::Number(42.0));
        assert!(!is_different(&current, &ValueRepr::Number(42.0)));
        assert!(is_different(&current, &ValueRepr::Number(43.0)));
    }

    #[test]
    fn is_different_bool() {
        let current = Some(&ValueRepr::Bool(true));
        assert!(!is_different(&current, &ValueRepr::Bool(true)));
        assert!(is_different(&current, &ValueRepr::Bool(false)));
    }

    #[test]
    fn is_different_string() {
        let current = Some(&ValueRepr::String("hello".to_owned()));
        assert!(!is_different(
            &current,
            &ValueRepr::String("hello".to_owned())
        ));
        assert!(is_different(
            &current,
            &ValueRepr::String("world".to_owned())
        ));
    }

    #[test]
    fn is_different_null() {
        let current = Some(&ValueRepr::Null);
        assert!(!is_different(&current, &ValueRepr::Null));
    }

    #[test]
    fn is_different_object_always_true() {
        use std::collections::HashMap;
        let current = Some(&ValueRepr::Object(HashMap::new()));
        assert!(is_different(&current, &ValueRepr::Object(HashMap::new())));
    }

    #[test]
    fn is_different_array_always_true() {
        let current = Some(&ValueRepr::Array(vec![]));
        assert!(is_different(&current, &ValueRepr::Array(vec![])));
    }

    #[test]
    fn is_different_type_mismatch() {
        let current = Some(&ValueRepr::Number(42.0));
        assert!(is_different(&current, &ValueRepr::String("42".to_owned())));
        assert!(is_different(&current, &ValueRepr::Bool(true)));
    }

    // --- diff_changes ---

    #[test]
    fn diff_changes_all_unchanged() {
        let shadow = make_shadow(r#"{"a": 1, "b": "hello"}"#);
        let changes = vec![make_change("a", "1"), make_change("b", r#""hello""#)];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 0); // All filtered out
    }

    #[test]
    fn diff_changes_all_changed() {
        let shadow = make_shadow(r#"{"a": 1, "b": "hello"}"#);
        let changes = vec![make_change("a", "2"), make_change("b", r#""world""#)];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 2); // All kept
        assert_eq!(result[0].path, "a");
        assert_eq!(result[1].path, "b");
    }

    #[test]
    fn diff_changes_partial() {
        let shadow = make_shadow(r#"{"a": 1, "b": "hello", "c": true}"#);
        let changes = vec![
            make_change("a", "1"),          // unchanged → drop
            make_change("b", r#""world""#), // changed → keep
            make_change("c", "true"),       // unchanged → drop
        ];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].path, "b");
    }

    #[test]
    fn diff_changes_new_path() {
        let shadow = make_shadow(r#"{"a": 1}"#);
        let changes = vec![make_change("b", "2")];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1); // New path → keep
        assert_eq!(result[0].path, "b");
    }

    #[test]
    fn diff_changes_object_always_keeps() {
        let shadow = make_shadow(r#"{"obj": {"x": 1}}"#);
        // Same object content
        let changes = vec![make_change("obj", r#"{"x": 1}"#)];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1); // Object → always kept
    }

    #[test]
    fn diff_changes_array_always_keeps() {
        let shadow = make_shadow(r#"{"arr": [1, 2, 3]}"#);
        // Same array content
        let changes = vec![make_change("arr", "[1, 2, 3]")];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1); // Array → always kept
    }

    #[test]
    fn diff_changes_nested_path() {
        let shadow = make_shadow(r#"{"user": {"name": "Alice", "age": 30}}"#);
        let changes = vec![
            make_change("user.name", r#""Alice""#), // unchanged → drop
            make_change("user.age", "31"),          // changed → keep
        ];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].path, "user.age");
    }

    #[test]
    fn diff_changes_number_edge_cases() {
        let shadow = make_shadow(r#"{"a": 0, "b": null}"#);
        let changes = vec![
            make_change("a", "-0.0"), // -0 != +0 → keep
            make_change("b", "0"),    // null != 0 (type mismatch) → keep
        ];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn diff_changes_invalid_json_kept() {
        let shadow = make_shadow(r#"{"a": 1}"#);
        let changes = vec![make_change("a", "not json")];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 1); // Invalid JSON → kept
    }

    #[test]
    fn diff_changes_empty_input() {
        let shadow = make_shadow(r#"{"a": 1}"#);
        let changes = vec![];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn diff_changes_zero_allocations_fast_path() {
        // This test verifies behavior, not allocations directly.
        // The fast path (all unchanged) should return an empty Vec.
        let shadow = make_shadow(r#"{"a": 1, "b": 2, "c": 3}"#);
        let changes = vec![
            make_change("a", "1"),
            make_change("b", "2"),
            make_change("c", "3"),
        ];
        let result = diff_changes(&shadow, &changes);
        assert_eq!(result.len(), 0);
    }
}
