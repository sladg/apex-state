//! Diff engine for comparing changes against shadow state.
//!
//! Filters out no-op changes by comparing incoming values against the nested
//! shadow state. Used by streaming data gateway to minimize pipeline work.

use crate::change::Change;
use crate::intern::InternTable;
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
/// - Object → structural hash comparison (O(1), precomputed at construction)
/// - Array → structural hash comparison (O(1), precomputed at construction)
/// - Type mismatch → always different
pub(crate) fn is_different(current: &Option<&ValueRepr>, new: &ValueRepr) -> bool {
    match (current, new) {
        (None, _) => true, // First time seeing this path
        (Some(ValueRepr::Number(a)), ValueRepr::Number(b)) => !floats_equal(*a, *b),
        (Some(ValueRepr::Bool(a)), ValueRepr::Bool(b)) => a != b,
        (Some(ValueRepr::String(a)), ValueRepr::String(b)) => a != b,
        (Some(ValueRepr::Null), ValueRepr::Null) => false,
        (Some(ValueRepr::Object(_, h1)), ValueRepr::Object(_, h2)) => {
            // Hash-based comparison: if structural hashes match, content is identical
            h1 != h2
        }
        (Some(ValueRepr::Array(_, h1)), ValueRepr::Array(_, h2)) => {
            // Hash-based comparison: if structural hashes match, content is identical
            h1 != h2
        }
        _ => true, // Type mismatch = different
    }
}

/// Diff a batch of changes against shadow state.
///
/// Returns only changes where the value differs from the current shadow state.
/// Primitives are compared by value, objects/arrays compared by structural hash.
///
/// Zero allocations on the fast path when all changes are no-ops.
pub(crate) fn diff_changes(
    shadow: &ShadowState,
    changes: &[Change],
    intern: &mut InternTable,
) -> Vec<Change> {
    let mut genuine_changes = Vec::new();

    for change in changes {
        // Get current value from shadow state
        let current = shadow.get(&change.path, intern);

        // Parse incoming value
        let new_value: ValueRepr =
            match serde_json::from_str::<serde_json::Value>(&change.value_json) {
                Ok(v) => ValueRepr::from_json(v, intern),
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

    fn make_shadow(json: &str) -> (ShadowState, InternTable) {
        let mut s = ShadowState::new();
        let mut intern = InternTable::new();
        s.init(json, &mut intern).unwrap();
        (s, intern)
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
    fn is_different_object_hash_compare_identical() {
        // Same empty objects → same hash → not different
        use crate::prelude::HashMap;
        let a = ValueRepr::Object(HashMap::new(), 42);
        let b = ValueRepr::Object(HashMap::new(), 42);
        assert!(!is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_object_hash_compare_different() {
        // Different hashes → different
        use crate::prelude::HashMap;
        let a = ValueRepr::Object(HashMap::new(), 42);
        let b = ValueRepr::Object(HashMap::new(), 99);
        assert!(is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_array_hash_compare_identical() {
        let a = ValueRepr::Array(vec![], 42);
        let b = ValueRepr::Array(vec![], 42);
        assert!(!is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_array_hash_compare_different() {
        let a = ValueRepr::Array(vec![], 42);
        let b = ValueRepr::Array(vec![], 99);
        assert!(is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_object_via_from_json_identical() {
        // Full round-trip: same JSON → same hash → not different
        let mut intern = InternTable::new();
        let a = ValueRepr::from_json(serde_json::json!({"x": 1, "y": "hello"}), &mut intern);
        let b = ValueRepr::from_json(serde_json::json!({"x": 1, "y": "hello"}), &mut intern);
        assert!(!is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_object_via_from_json_changed_field() {
        // One field changed → different hash → different
        let mut intern = InternTable::new();
        let a = ValueRepr::from_json(serde_json::json!({"x": 1, "y": "hello"}), &mut intern);
        let b = ValueRepr::from_json(serde_json::json!({"x": 1, "y": "world"}), &mut intern);
        assert!(is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_nested_object_unchanged() {
        // Nested objects with identical content → same hash
        let mut intern = InternTable::new();
        let a = ValueRepr::from_json(
            serde_json::json!({"config": {"currency": "USD", "amount": 100}}),
            &mut intern,
        );
        let b = ValueRepr::from_json(
            serde_json::json!({"config": {"currency": "USD", "amount": 100}}),
            &mut intern,
        );
        assert!(!is_different(&Some(&a), &b));
    }

    #[test]
    fn is_different_nested_object_one_field_changed() {
        let mut intern = InternTable::new();
        let a = ValueRepr::from_json(
            serde_json::json!({"config": {"currency": "USD", "amount": 100}}),
            &mut intern,
        );
        let b = ValueRepr::from_json(
            serde_json::json!({"config": {"currency": "EUR", "amount": 100}}),
            &mut intern,
        );
        assert!(is_different(&Some(&a), &b));
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
        let (shadow, mut intern) = make_shadow(r#"{"a": 1, "b": "hello"}"#);
        let changes = vec![make_change("a", "1"), make_change("b", r#""hello""#)];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0); // All filtered out
    }

    #[test]
    fn diff_changes_all_changed() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 1, "b": "hello"}"#);
        let changes = vec![make_change("a", "2"), make_change("b", r#""world""#)];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 2); // All kept
        assert_eq!(result[0].path, "a");
        assert_eq!(result[1].path, "b");
    }

    #[test]
    fn diff_changes_partial() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 1, "b": "hello", "c": true}"#);
        let changes = vec![
            make_change("a", "1"),          // unchanged → drop
            make_change("b", r#""world""#), // changed → keep
            make_change("c", "true"),       // unchanged → drop
        ];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].path, "b");
    }

    #[test]
    fn diff_changes_new_path() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 1}"#);
        let changes = vec![make_change("b", "2")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1); // New path → keep
        assert_eq!(result[0].path, "b");
    }

    #[test]
    fn diff_changes_object_same_content_filtered() {
        let (shadow, mut intern) = make_shadow(r#"{"obj": {"x": 1}}"#);
        // Same object content → hash matches → filtered out
        let changes = vec![make_change("obj", r#"{"x": 1}"#)];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0); // Same hash → filtered
    }

    #[test]
    fn diff_changes_object_different_content_kept() {
        let (shadow, mut intern) = make_shadow(r#"{"obj": {"x": 1}}"#);
        // Different object content → different hash → kept
        let changes = vec![make_change("obj", r#"{"x": 2}"#)];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn diff_changes_array_same_content_filtered() {
        let (shadow, mut intern) = make_shadow(r#"{"arr": [1, 2, 3]}"#);
        // Same array content → hash matches → filtered out
        let changes = vec![make_change("arr", "[1, 2, 3]")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0); // Same hash → filtered
    }

    #[test]
    fn diff_changes_array_different_content_kept() {
        let (shadow, mut intern) = make_shadow(r#"{"arr": [1, 2, 3]}"#);
        // Different array content → different hash → kept
        let changes = vec![make_change("arr", "[1, 2, 4]")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn diff_changes_nested_path() {
        let (shadow, mut intern) = make_shadow(r#"{"user": {"name": "Alice", "age": 30}}"#);
        let changes = vec![
            make_change("user.name", r#""Alice""#), // unchanged → drop
            make_change("user.age", "31"),          // changed → keep
        ];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].path, "user.age");
    }

    #[test]
    fn diff_changes_number_edge_cases() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 0, "b": null}"#);
        let changes = vec![
            make_change("a", "-0.0"), // -0 != +0 → keep
            make_change("b", "0"),    // null != 0 (type mismatch) → keep
        ];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn diff_changes_invalid_json_kept() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 1}"#);
        let changes = vec![make_change("a", "not json")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1); // Invalid JSON → kept
    }

    #[test]
    fn diff_changes_empty_input() {
        let (shadow, mut intern) = make_shadow(r#"{"a": 1}"#);
        let changes = vec![];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn diff_changes_zero_allocations_fast_path() {
        // This test verifies behavior, not allocations directly.
        // The fast path (all unchanged) should return an empty Vec.
        let (shadow, mut intern) = make_shadow(r#"{"a": 1, "b": 2, "c": 3}"#);
        let changes = vec![
            make_change("a", "1"),
            make_change("b", "2"),
            make_change("c", "3"),
        ];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0);
    }

    // --- WASM-044 regression: object-valued sync targets ---

    #[test]
    fn diff_changes_object_sync_target_identical_content_filtered() {
        // Customer bug: bidirectional sync pair on object-valued path.
        // Re-setting src.config to the same { currency, amount } object caused
        // is_different to return true (no deep comparison), triggering spurious
        // sync writes to dst.config.
        //
        // After WASM-044 (structural hash), identical objects are detected as
        // no-ops and filtered out by diff_changes.
        let (shadow, mut intern) = make_shadow(
            r#"{"src": {"config": {"currency": "USD", "amount": 100}}, "dst": {"config": {"currency": "USD", "amount": 100}}}"#,
        );

        // Re-set src.config to identical content
        let changes = vec![make_change(
            "src.config",
            r#"{"currency": "USD", "amount": 100}"#,
        )];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(
            result.len(),
            0,
            "identical object should be filtered as no-op"
        );
    }

    #[test]
    fn diff_changes_object_sync_target_changed_field_kept() {
        // Sanity check: when a field inside the object actually changes,
        // diff_changes must keep the change.
        let (shadow, mut intern) =
            make_shadow(r#"{"src": {"config": {"currency": "USD", "amount": 100}}}"#);

        let changes = vec![make_change(
            "src.config",
            r#"{"currency": "EUR", "amount": 100}"#,
        )];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 1, "changed object must pass through");
    }

    #[test]
    fn diff_changes_nested_object_unchanged_after_parent_set() {
        // Simulates parent-path reassignment where a deeply nested object
        // is re-set with identical content. The diff should detect the no-op.
        let (shadow, mut intern) =
            make_shadow(r#"{"level1": {"level2": {"level3": {"value": "L3", "extra": true}}}}"#);

        // Re-set the nested subtree with identical content
        let changes = vec![make_change(
            "level1.level2.level3",
            r#"{"value": "L3", "extra": true}"#,
        )];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(
            result.len(),
            0,
            "identical nested object should be filtered"
        );
    }

    #[test]
    fn diff_changes_array_of_objects_unchanged() {
        // Array containing objects, re-set with identical content.
        let (shadow, mut intern) =
            make_shadow(r#"{"items": [{"id": 1, "name": "A"}, {"id": 2, "name": "B"}]}"#);

        let changes = vec![make_change(
            "items",
            r#"[{"id": 1, "name": "A"}, {"id": 2, "name": "B"}]"#,
        )];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(
            result.len(),
            0,
            "identical array of objects should be filtered"
        );
    }

    #[test]
    fn diff_changes_array_of_objects_one_element_changed() {
        let (shadow, mut intern) =
            make_shadow(r#"{"items": [{"id": 1, "name": "A"}, {"id": 2, "name": "B"}]}"#);

        let changes = vec![make_change(
            "items",
            r#"[{"id": 1, "name": "A"}, {"id": 2, "name": "C"}]"#,
        )];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(
            result.len(),
            1,
            "array with changed element must pass through"
        );
    }

    #[test]
    fn diff_changes_empty_object_unchanged() {
        let (shadow, mut intern) = make_shadow(r#"{"meta": {}}"#);
        let changes = vec![make_change("meta", "{}")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(
            result.len(),
            0,
            "identical empty objects should be filtered"
        );
    }

    #[test]
    fn diff_changes_empty_array_unchanged() {
        let (shadow, mut intern) = make_shadow(r#"{"tags": []}"#);
        let changes = vec![make_change("tags", "[]")];
        let result = diff_changes(&shadow, &changes, &mut intern);
        assert_eq!(result.len(), 0, "identical empty arrays should be filtered");
    }
}
