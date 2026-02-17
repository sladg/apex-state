//! ClearPaths side-effect: "when X changes, set Y to null."
//!
//! Supports wildcards:
//! - `[*]`  (WildcardBind)  — positional capture in triggers, substitution in targets
//! - `[**]` (WildcardAll)   — expands to all keys at that object level in shadow state
//!
//! See docs/CLEAR_PATHS.md for full architecture.

// TODO: Implementation — this file contains only the module structure and tests for now.

#[cfg(test)]
mod tests {
    // NOTE: These tests reference types and helpers that will be created during implementation.
    // They are written against the planned API from docs/CLEAR_PATHS.md and will compile
    // once the implementation is in place.
    //
    // For now they serve as executable documentation of the expected behavior.

    // =========================================================================
    // Test helpers (will use real types once implemented)
    // =========================================================================

    // --- PathSegment parsing ---

    #[test]
    fn parse_segments_literal_only() {
        // "form.email" → [Literal("form"), Literal("email")]
        // No wildcards, all literal segments
        todo!("Implement: parse 'form.email' into Vec<PathSegment>")
    }

    #[test]
    fn parse_segments_with_wildcard_bind() {
        // "form.fields.[*].value" → [Literal("form"), Literal("fields"), WildcardBind, Literal("value")]
        todo!("Implement: parse path with [*] wildcard")
    }

    #[test]
    fn parse_segments_with_wildcard_all() {
        // "form.fields.[**].error" → [Literal("form"), Literal("fields"), WildcardAll, Literal("error")]
        todo!("Implement: parse path with [**] wildcard")
    }

    #[test]
    fn parse_segments_multiple_wildcards() {
        // "form.[*].fields.[**].error" → [Literal, WildcardBind, Literal, WildcardAll, Literal]
        todo!("Implement: parse path with mixed wildcards")
    }

    // --- Trigger matching ---

    #[test]
    fn match_trigger_exact_literal() {
        // Pattern: "form.email" against path: "form.email" → Some(captures=[])
        todo!("Implement: exact literal trigger matching")
    }

    #[test]
    fn match_trigger_literal_mismatch() {
        // Pattern: "form.email" against path: "form.name" → None
        todo!("Implement: literal mismatch returns None")
    }

    #[test]
    fn match_trigger_length_mismatch() {
        // Pattern: "form.email" (2 segments) against path: "form.fields.email" (3 segments) → None
        todo!("Implement: segment count mismatch returns None")
    }

    #[test]
    fn match_trigger_single_wildcard_capture() {
        // Pattern: "form.fields.[*].value"
        // Path:    "form.fields.email.value"
        // → Some(captures=["email"])
        todo!("Implement: single [*] captures matched key")
    }

    #[test]
    fn match_trigger_multiple_wildcard_captures() {
        // Pattern: "form.[*].[*].value"
        // Path:    "form.billing.address.value"
        // → Some(captures=["billing", "address"])
        todo!("Implement: multiple [*] capture positionally")
    }

    #[test]
    fn match_trigger_wildcard_at_end() {
        // Pattern: "form.fields.[*]"
        // Path:    "form.fields.email"
        // → Some(captures=["email"])
        todo!("Implement: wildcard at last segment position")
    }

    #[test]
    fn match_trigger_wildcard_no_match_wrong_literal() {
        // Pattern: "form.fields.[*].value"
        // Path:    "form.items.email.value"
        // → None (fields != items)
        todo!("Implement: wildcard trigger fails on non-matching literals")
    }

    // --- Target resolution: direct (no wildcards) ---

    #[test]
    fn resolve_direct_target_clears_to_null() {
        // Shadow: { form: { errors: "some error" } }
        // Target: "form.errors" (direct, interned ID)
        // → clears form.errors to null, returns one Change
        todo!("Implement: direct target sets path to null in shadow")
    }

    #[test]
    fn resolve_direct_target_already_null_skips() {
        // Shadow: { form: { errors: null } }
        // Target: "form.errors" (direct)
        // → already null, no Change produced (deduplication)
        todo!("Implement: skip clearing already-null paths")
    }

    // --- Target resolution: WildcardBind (correlated) ---

    #[test]
    fn resolve_wildcard_bind_substitutes_capture() {
        // Shadow: { form: { fields: { email: { error: "required", value: "x" } } } }
        // Target segments: [Literal("form"), Literal("fields"), WildcardBind, Literal("error")]
        // Captures: ["email"]
        // → resolves to "form.fields.email.error", clears to null
        todo!("Implement: WildcardBind substitutes captured key into target")
    }

    #[test]
    fn resolve_wildcard_bind_multi_capture() {
        // Shadow: { form: { billing: { address: { error: "bad", value: "123" } } } }
        // Target segments: [Literal("form"), WildcardBind, WildcardBind, Literal("error")]
        // Captures: ["billing", "address"]
        // → resolves to "form.billing.address.error"
        todo!("Implement: multiple WildcardBind substitutions")
    }

    // --- Target resolution: WildcardAll (expand) ---

    #[test]
    fn resolve_wildcard_all_expands_to_all_keys() {
        // Shadow: { form: { fields: { email: { error: "e1" }, name: { error: "e2" }, age: { error: null } } } }
        // Target segments: [Literal("form"), Literal("fields"), WildcardAll, Literal("error")]
        // Captures: [] (no bind in trigger)
        // → expands to:
        //   "form.fields.email.error" → null (was "e1")
        //   "form.fields.name.error"  → null (was "e2")
        //   "form.fields.age.error"   → skip (already null)
        todo!("Implement: WildcardAll enumerates all object keys from shadow")
    }

    #[test]
    fn resolve_wildcard_all_empty_object() {
        // Shadow: { form: { fields: {} } }
        // Target segments with [**] at fields level
        // → no keys to expand, produces zero changes
        todo!("Implement: WildcardAll on empty object produces no changes")
    }

    #[test]
    fn resolve_wildcard_all_non_object_produces_nothing() {
        // Shadow: { form: { fields: "not an object" } }
        // Target segments with [**] at fields level
        // → fields is a string, not an object, produces zero changes
        todo!("Implement: WildcardAll on non-object value produces nothing")
    }

    // --- Target resolution: mixed WildcardBind + WildcardAll ---

    #[test]
    fn resolve_mixed_bind_then_all() {
        // Shadow: {
        //   form: {
        //     billing: {
        //       fields: { street: { error: "e1" }, city: { error: "e2" } }
        //     }
        //   }
        // }
        // Target: "form.[*].fields.[**].error"
        // Captures: ["billing"]  (from trigger match)
        // → [*] substituted with "billing"
        // → [**] expands to all keys under form.billing.fields
        // → produces: "form.billing.fields.street.error" → null
        //             "form.billing.fields.city.error"   → null
        todo!("Implement: WildcardBind substitution + WildcardAll expansion in same target")
    }

    #[test]
    fn resolve_all_then_bind() {
        // Target: "form.[**].fields.[*].error"
        // Captures: ["street"]
        // Shadow: {
        //   form: {
        //     billing: { fields: { street: { error: "e1" } } },
        //     shipping: { fields: { street: { error: "e2" } } }
        //   }
        // }
        // → [**] expands to ["billing", "shipping"]
        // → [*] substituted with "street"
        // → produces: "form.billing.fields.street.error"  → null
        //             "form.shipping.fields.street.error" → null
        todo!("Implement: WildcardAll expansion followed by WildcardBind substitution")
    }

    // =========================================================================
    // Complex integration scenarios (pipeline-level, using ProcessingPipeline)
    // =========================================================================

    #[test]
    fn pipeline_clear_concrete_trigger_concrete_target() {
        // Shadow: { form: { email: "a@b.com", errors: "invalid", touched: true } }
        // Clear rule: triggers=["form.email"], targets=["form.errors", "form.touched"]
        //
        // Change: form.email = "new@b.com"
        // Expected output changes:
        //   - form.email = "new@b.com"    (input)
        //   - form.errors = null          (cleared)
        //   - form.touched = null         (cleared)
        todo!("Implement: basic concrete clear in pipeline")
    }

    #[test]
    fn pipeline_clear_skips_when_trigger_is_noop() {
        // Shadow: { form: { email: "same", errors: "err" } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        //
        // Change: form.email = "same" (no-op, diff filters it out at Step 0)
        // Expected: no changes at all (early exit), errors NOT cleared
        todo!("Implement: clear does not fire on diff-eliminated no-ops")
    }

    #[test]
    fn pipeline_clear_target_already_null() {
        // Shadow: { form: { email: "a@b.com", errors: null } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        //
        // Change: form.email = "new@b.com"
        // Expected: form.email change only, form.errors already null → not in output
        todo!("Implement: clear skips already-null targets")
    }

    #[test]
    fn pipeline_clear_feeds_into_sync() {
        // Shadow: { form: { email: "a", errors: "err" }, mirror: { errors: "err" } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        // Sync pair: ["form.errors", "mirror.errors"]
        //
        // Change: form.email = "b"
        // Expected pipeline:
        //   Step 3:   form.email = "b" applied to shadow
        //   Step 3.5: form.errors = null (cleared)
        //   Step 4-5: mirror.errors = null (synced from form.errors)
        // Output: [form.email="b", form.errors=null, mirror.errors=null]
        todo!("Implement: cleared path propagates through sync")
    }

    #[test]
    fn pipeline_clear_feeds_into_flip() {
        // Shadow: { toggle: false, enabled: true, disabled: false }
        // Clear rule: triggers=["toggle"], targets=["enabled"]
        // Flip pair: ["enabled", "disabled"]
        //
        // Change: toggle = true
        // Expected pipeline:
        //   Step 3:   toggle = true
        //   Step 3.5: enabled = null (cleared)
        //   Step 6-7: flip sees enabled=null, non-boolean → no flip (passes through)
        // Output: [toggle=true, enabled=null]
        // Note: flip skips non-boolean values, null is not boolean
        todo!("Implement: cleared path interaction with flip")
    }

    #[test]
    fn pipeline_clear_feeds_into_boollogic() {
        // Shadow: { form: { email: "a@b.com", errors: "invalid" } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        // BoolLogic: _concerns.form.hasErrors = EXISTS("form.errors")
        //
        // Change: form.email = "new@b.com"
        // Expected pipeline:
        //   Step 3:   form.email updated
        //   Step 3.5: form.errors = null (cleared)
        //   Step 8-9: EXISTS("form.errors") → false (value is null)
        //   → _concerns.form.hasErrors = false
        todo!("Implement: cleared path triggers BoolLogic re-evaluation")
    }

    #[test]
    fn pipeline_clear_feeds_into_aggregation_reads() {
        // Shadow: { items: { a: { price: 10 }, b: { price: 10 } }, totals: { price: 10 } }
        // Clear rule: triggers=["items.a.price"], targets=["items.b.price"]
        // Aggregation: target="totals.price", sources=["items.a.price", "items.b.price"]
        //
        // Change: items.a.price = 20
        // Expected pipeline:
        //   Step 3:   items.a.price = 20
        //   Step 3.5: items.b.price = null (cleared)
        //   Step 7.5: aggregation reads — sources are [20, null], not all-equal → totals.price = null
        todo!("Implement: cleared path affects aggregation read recomputation")
    }

    #[test]
    fn pipeline_clear_no_cascade() {
        // Shadow: { a: "x", b: "y", c: "z" }
        // Clear rule 1: triggers=["a"], targets=["b"]
        // Clear rule 2: triggers=["b"], targets=["c"]
        //
        // Change: a = "new"
        // Expected: a="new", b=null (from rule 1)
        // c should NOT be cleared — clearPaths does not self-cascade.
        // Only original genuine changes (Step 3) feed into clearPaths.
        todo!("Implement: clearPaths does not cascade into itself")
    }

    #[test]
    fn pipeline_clear_wildcard_trigger_correlated_target() {
        // Shadow: {
        //   form: {
        //     fields: {
        //       email: { value: "old@b.com", error: "invalid" },
        //       name:  { value: "Alice",     error: "too short" }
        //     }
        //   }
        // }
        // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
        //   (correlated: [*] in target binds to same key as trigger)
        //
        // Change: form.fields.email.value = "new@b.com"
        // Expected:
        //   - form.fields.email.value = "new@b.com" (input)
        //   - form.fields.email.error = null         (cleared, correlated with "email")
        //   - form.fields.name.error  stays "too short" (NOT cleared, different key)
        todo!("Implement: wildcard trigger with correlated target clears only matched key")
    }

    #[test]
    fn pipeline_clear_wildcard_trigger_expanded_target() {
        // Shadow: {
        //   form: {
        //     fields: {
        //       email: { value: "old", error: "e1" },
        //       name:  { value: "old", error: "e2" },
        //       age:   { value: "old", error: "e3" }
        //     }
        //   }
        // }
        // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[**].error"]
        //   (expanded: [**] in target expands to ALL keys regardless of trigger match)
        //
        // Change: form.fields.email.value = "new"
        // Expected:
        //   - form.fields.email.value = "new"  (input)
        //   - form.fields.email.error = null   (cleared, all keys)
        //   - form.fields.name.error  = null   (cleared, all keys)
        //   - form.fields.age.error   = null   (cleared, all keys)
        todo!("Implement: wildcard trigger with expanded target clears ALL keys")
    }

    #[test]
    fn pipeline_clear_wildcard_trigger_does_not_match_wrong_prefix() {
        // Shadow: {
        //   form: { fields: { email: { value: "x", error: "e" } } },
        //   other: { fields: { email: { value: "y", error: "f" } } }
        // }
        // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
        //
        // Change: other.fields.email.value = "z"
        // Expected: only the input change, no clears (trigger pattern doesn't match "other.fields...")
        todo!("Implement: wildcard trigger only matches correct prefix")
    }

    #[test]
    fn pipeline_clear_multiple_triggers_same_rule() {
        // Shadow: { form: { email: "a", name: "b", errors: "err", touched: true } }
        // Clear rule: triggers=["form.email", "form.name"], targets=["form.errors", "form.touched"]
        //
        // Change: form.email = "new"
        // Expected: form.email + form.errors=null + form.touched=null
        //
        // Then change: form.name = "new"
        // Expected: form.name + form.errors=null (already null → skip) + form.touched=null (already null → skip)
        todo!("Implement: multiple concrete triggers on same rule, dedup on second fire")
    }

    #[test]
    fn pipeline_clear_deduplicates_overlapping_targets() {
        // Shadow: { a: "x", b: "y", target: "z" }
        // Clear rule 1: triggers=["a"], targets=["target"]
        // Clear rule 2: triggers=["b"], targets=["target"]
        //
        // Change both: a = "x2", b = "y2"
        // Expected: a, b, target=null (only once, deduped via `seen` set)
        todo!("Implement: overlapping targets from multiple rules are deduplicated")
    }

    #[test]
    fn pipeline_clear_subtree_sets_object_to_null() {
        // Shadow: { form: { errors: { email: "e1", name: "e2", nested: { deep: "e3" } } } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        //
        // Change: form.email = "new"
        // Expected: form.errors = null (entire subtree replaced with null, single change entry)
        // After clear: shadow.get("form.errors") → null
        //              shadow.get("form.errors.email") → None (subtree gone)
        todo!("Implement: clearing an object path sets entire subtree to null")
    }

    #[test]
    fn pipeline_clear_wildcard_multi_level_correlated() {
        // Shadow: {
        //   app: {
        //     section1: {
        //       fieldA: { value: "v1", error: "e1" },
        //       fieldB: { value: "v2", error: "e2" }
        //     },
        //     section2: {
        //       fieldC: { value: "v3", error: "e3" }
        //     }
        //   }
        // }
        // Clear rule: triggers=["app.[*].[*].value"], targets=["app.[*].[*].error"]
        //   (correlated: both [*] bind positionally)
        //
        // Change: app.section1.fieldA.value = "new"
        // Trigger match: captures = ["section1", "fieldA"]
        // Target substitution: "app.section1.fieldA.error" → null
        //
        // Expected: only fieldA's error cleared, not fieldB or section2
        todo!("Implement: multi-level wildcard bind with positional correlation")
    }

    #[test]
    fn pipeline_clear_wildcard_trigger_multiple_changes_same_batch() {
        // Shadow: {
        //   form: {
        //     fields: {
        //       email: { value: "old1", error: "e1" },
        //       name:  { value: "old2", error: "e2" }
        //     }
        //   }
        // }
        // Clear rule: triggers=["form.fields.[*].value"], targets=["form.fields.[*].error"]
        //
        // Batch change: form.fields.email.value = "new1", form.fields.name.value = "new2"
        // Expected: both email.error and name.error cleared (two separate trigger matches)
        todo!("Implement: multiple changes in one batch each independently match wildcard trigger")
    }

    // =========================================================================
    // Registration and validation
    // =========================================================================

    #[test]
    fn registration_concrete_rule() {
        // Register: triggers=["a"], targets=["b"]
        // Verify: direct_triggers has entry for interned "a" → [Direct(interned "b")]
        todo!("Implement: concrete rule populates direct_triggers")
    }

    #[test]
    fn registration_wildcard_trigger_rule() {
        // Register: triggers=["form.[*].value"], targets=["form.[*].error"]
        // Verify: wildcard_triggers has one entry with correct segments
        todo!("Implement: wildcard trigger goes into wildcard_triggers vec")
    }

    #[test]
    fn registration_mixed_triggers_same_rule() {
        // Register: triggers=["form.email", "form.fields.[*].value"], targets=["form.errors"]
        // Verify: "form.email" in direct_triggers, wildcard pattern in wildcard_triggers
        //         both pointing to same target
        todo!("Implement: mixed concrete+wildcard triggers in same rule")
    }

    #[test]
    fn registration_rejects_wildcard_all_in_trigger() {
        // Attempt: triggers=["form.[**].value"], targets=["form.errors"]
        // Expected: error — [**] (WildcardAll) not allowed in triggers
        todo!("Implement: reject [**] in trigger paths")
    }

    #[test]
    fn registration_rejects_unmatched_bind_in_target() {
        // Attempt: triggers=["form.email"], targets=["form.[*].error"]
        // Expected: error — target has [*] (WildcardBind) but trigger has no [*] to capture from
        todo!("Implement: reject target [*] when trigger has fewer wildcards")
    }

    #[test]
    fn registration_allows_wildcard_all_in_target_without_trigger_bind() {
        // Attempt: triggers=["form.email"], targets=["form.fields.[**].error"]
        // Expected: OK — [**] doesn't need a trigger capture
        todo!("Implement: [**] in target is always valid")
    }

    #[test]
    fn unregistration_removes_rules() {
        // Register with registration_id = "test-1"
        // Verify triggers exist
        // Unregister "test-1"
        // Verify triggers removed
        todo!("Implement: unregistration by registration_id")
    }
}
