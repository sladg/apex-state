//! ClearPaths side-effect: "when X changes, set Y to null."
//!
//! Supports wildcards:
//! - `[*]`  (WildcardBind)  — positional capture in triggers, substitution in targets
//! - `[**]` (WildcardAll)   — expands to all keys at that object level in shadow state
//!
//! See docs/CLEAR_PATHS.md for full architecture.

use crate::intern::InternTable;
use crate::pipeline::Change;
use crate::shadow::ShadowState;
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum PathSegment {
    Literal(String),
    WildcardBind, // [*]  — capture in trigger, substitute in target
    WildcardAll,  // [**] — expand to all keys from shadow state
}

#[derive(Debug, Clone)]
pub(crate) enum ClearTarget {
    /// No wildcards — interned path ID, O(1) resolve.
    Direct(u32),
    /// Contains [*] or [**] — resolved at runtime against shadow + bindings.
    Wildcard(Vec<PathSegment>),
}

#[derive(Debug, Clone)]
struct WildcardTrigger {
    segments: Vec<PathSegment>, // only Literal + WildcardBind
    targets: Vec<ClearTarget>,
    registration_id: String,
}

#[derive(Debug, Clone)]
struct DirectEntry {
    targets: Vec<ClearTarget>,
    registration_id: String,
}

/// Dual-index registry for clear-path rules.
pub(crate) struct ClearPathsRegistry {
    /// Fast path: concrete trigger path_id → targets.
    direct_triggers: HashMap<u32, Vec<DirectEntry>>,
    /// Slow path: wildcard trigger patterns, matched against each changed path.
    wildcard_triggers: Vec<WildcardTrigger>,
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Parse a dot-separated path into segments.
/// `[*]` → WildcardBind, `[**]` → WildcardAll, else → Literal.
pub(crate) fn parse_segments(path: &str) -> Vec<PathSegment> {
    path.split('.')
        .map(|seg| match seg {
            "[*]" => PathSegment::WildcardBind,
            "[**]" => PathSegment::WildcardAll,
            _ => PathSegment::Literal(seg.to_owned()),
        })
        .collect()
}

/// Check if segments contain any wildcard.
fn has_wildcard(segments: &[PathSegment]) -> bool {
    segments
        .iter()
        .any(|s| matches!(s, PathSegment::WildcardBind | PathSegment::WildcardAll))
}

/// Count WildcardBind occurrences.
fn count_binds(segments: &[PathSegment]) -> usize {
    segments
        .iter()
        .filter(|s| matches!(s, PathSegment::WildcardBind))
        .count()
}

// ---------------------------------------------------------------------------
// Trigger matching
// ---------------------------------------------------------------------------

/// Match a concrete path against a trigger pattern.
/// Returns positional captures from WildcardBind segments, or None if no match.
pub(crate) fn match_trigger(path: &str, pattern: &[PathSegment]) -> Option<Vec<String>> {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.len() != pattern.len() {
        return None;
    }

    let mut captures = Vec::new();
    for (part, seg) in parts.iter().zip(pattern) {
        match seg {
            PathSegment::Literal(s) => {
                if *part != s.as_str() {
                    return None;
                }
            }
            PathSegment::WildcardBind => {
                captures.push((*part).to_owned());
            }
            PathSegment::WildcardAll => {
                // [**] should never appear in triggers (validated at registration)
                return None;
            }
        }
    }
    Some(captures)
}

// ---------------------------------------------------------------------------
// Target resolution
// ---------------------------------------------------------------------------

/// Resolve a wildcard target pattern into concrete paths using captures and shadow state.
///
/// - WildcardBind: substitute captures positionally (1st bind → 1st capture, etc.)
/// - WildcardAll: enumerate all Object keys from shadow at current prefix, branch into multiple paths
pub(crate) fn resolve_wildcard_target(
    shadow: &ShadowState,
    segments: &[PathSegment],
    captures: &[String],
) -> Vec<String> {
    // We build paths incrementally, branching at WildcardAll.
    let mut prefixes: Vec<(String, usize)> = vec![(String::new(), 0)]; // (prefix, capture_index)

    for seg in segments {
        let mut new_prefixes = Vec::new();
        for (prefix, cap_idx) in &prefixes {
            match seg {
                PathSegment::Literal(s) => {
                    let new_prefix = if prefix.is_empty() {
                        s.clone()
                    } else {
                        format!("{}.{}", prefix, s)
                    };
                    new_prefixes.push((new_prefix, *cap_idx));
                }
                PathSegment::WildcardBind => {
                    if let Some(captured) = captures.get(*cap_idx) {
                        let new_prefix = if prefix.is_empty() {
                            captured.clone()
                        } else {
                            format!("{}.{}", prefix, captured)
                        };
                        new_prefixes.push((new_prefix, cap_idx + 1));
                    }
                    // If no capture available, this path dies (shouldn't happen with validation)
                }
                PathSegment::WildcardAll => {
                    // Enumerate all keys at the current prefix in shadow
                    if let Some(keys) = shadow.get_object_keys(prefix) {
                        for key in keys {
                            let new_prefix = if prefix.is_empty() {
                                key.clone()
                            } else {
                                format!("{}.{}", prefix, key)
                            };
                            new_prefixes.push((new_prefix, *cap_idx));
                        }
                    }
                    // If not an object or doesn't exist, no paths produced
                }
            }
        }
        prefixes = new_prefixes;
    }

    prefixes.into_iter().map(|(path, _)| path).collect()
}

// ---------------------------------------------------------------------------
// Registry implementation
// ---------------------------------------------------------------------------

impl ClearPathsRegistry {
    pub(crate) fn new() -> Self {
        Self {
            direct_triggers: HashMap::new(),
            wildcard_triggers: Vec::new(),
        }
    }

    /// Register a clear-path rule.
    ///
    /// Each trigger → targets mapping. Concrete triggers go to `direct_triggers`,
    /// wildcard triggers go to `wildcard_triggers`.
    pub(crate) fn register(
        &mut self,
        registration_id: &str,
        triggers: &[String],
        targets: &[String],
        intern: &mut InternTable,
    ) -> Result<(), String> {
        if triggers.is_empty() || targets.is_empty() {
            return Err("ClearPaths rule must have at least one trigger and one target".to_owned());
        }

        // Find the max bind count across all triggers for validation
        let max_trigger_binds = triggers
            .iter()
            .map(|t| {
                let segs = parse_segments(t);
                count_binds(&segs)
            })
            .max()
            .unwrap_or(0);

        // Validate triggers: no [**] allowed
        for trigger in triggers {
            let segs = parse_segments(trigger);
            for seg in &segs {
                if matches!(seg, PathSegment::WildcardAll) {
                    return Err("Triggers cannot use expand-all wildcards [**]".to_owned());
                }
            }
        }

        // Validate targets: [*] count must not exceed trigger [*] count
        for target in targets {
            let segs = parse_segments(target);
            let target_binds = count_binds(&segs);
            if target_binds > max_trigger_binds {
                return Err("Target has more positional wildcards [*] than trigger".to_owned());
            }
        }

        // Build ClearTarget list
        let clear_targets: Vec<ClearTarget> = targets
            .iter()
            .map(|target| {
                let segs = parse_segments(target);
                if has_wildcard(&segs) {
                    ClearTarget::Wildcard(segs)
                } else {
                    ClearTarget::Direct(intern.intern(target))
                }
            })
            .collect();

        // Register each trigger
        for trigger in triggers {
            let segs = parse_segments(trigger);
            if has_wildcard(&segs) {
                self.wildcard_triggers.push(WildcardTrigger {
                    segments: segs,
                    targets: clear_targets.clone(),
                    registration_id: registration_id.to_owned(),
                });
            } else {
                let path_id = intern.intern(trigger);
                self.direct_triggers
                    .entry(path_id)
                    .or_default()
                    .push(DirectEntry {
                        targets: clear_targets.clone(),
                        registration_id: registration_id.to_owned(),
                    });
            }
        }

        Ok(())
    }

    /// Unregister all rules for a given registration ID.
    pub(crate) fn unregister(&mut self, registration_id: &str) {
        // Remove from direct_triggers
        for entries in self.direct_triggers.values_mut() {
            entries.retain(|e| e.registration_id != registration_id);
        }
        // Remove empty entries
        self.direct_triggers.retain(|_, v| !v.is_empty());

        // Remove from wildcard_triggers
        self.wildcard_triggers
            .retain(|wt| wt.registration_id != registration_id);
    }

    /// Check if registry is empty (no rules registered).
    pub(crate) fn is_empty(&self) -> bool {
        self.direct_triggers.is_empty() && self.wildcard_triggers.is_empty()
    }

    /// Process changed paths and produce clear changes.
    ///
    /// For each changed path_id: check direct_triggers (O(1)), then scan wildcard_triggers.
    /// Deduplicates targets via HashSet<u32>. Returns clear changes.
    pub(crate) fn process(
        &self,
        changed_path_ids: &[u32],
        intern: &mut InternTable,
        shadow: &mut ShadowState,
    ) -> Vec<Change> {
        if self.is_empty() {
            return Vec::new();
        }

        let mut clears = Vec::new();
        let mut seen: HashSet<u32> = HashSet::new();

        for &path_id in changed_path_ids {
            // --- Fast path: direct triggers ---
            if let Some(entries) = self.direct_triggers.get(&path_id) {
                for entry in entries {
                    resolve_and_clear(&entry.targets, &[], intern, shadow, &mut clears, &mut seen);
                }
            }

            // --- Slow path: wildcard triggers ---
            if !self.wildcard_triggers.is_empty() {
                if let Some(path) = intern.resolve(path_id) {
                    let path = path.to_owned(); // avoid borrow conflict
                    for wt in &self.wildcard_triggers {
                        if let Some(captures) = match_trigger(&path, &wt.segments) {
                            resolve_and_clear(
                                &wt.targets,
                                &captures,
                                intern,
                                shadow,
                                &mut clears,
                                &mut seen,
                            );
                        }
                    }
                }
            }
        }

        clears
    }
}

impl Default for ClearPathsRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Resolve targets and produce clear changes (set to null).
fn resolve_and_clear(
    targets: &[ClearTarget],
    captures: &[String],
    intern: &mut InternTable,
    shadow: &mut ShadowState,
    clears: &mut Vec<Change>,
    seen: &mut HashSet<u32>,
) {
    for target in targets {
        match target {
            ClearTarget::Direct(target_id) => {
                if seen.insert(*target_id) {
                    if let Some(path) = intern.resolve(*target_id) {
                        let path = path.to_owned();
                        if !shadow.is_null(&path) {
                            let _ = shadow.set(&path, "null");
                            clears.push(Change {
                                path,
                                value_json: "null".to_owned(),
                            });
                        }
                    }
                }
            }
            ClearTarget::Wildcard(segments) => {
                let concrete_paths = resolve_wildcard_target(shadow, segments, captures);
                for path in concrete_paths {
                    let id = intern.intern(&path);
                    if seen.insert(id) && !shadow.is_null(&path) {
                        let _ = shadow.set(&path, "null");
                        clears.push(Change {
                            path,
                            value_json: "null".to_owned(),
                        });
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::ProcessingPipeline;

    // =========================================================================
    // Test helpers
    // =========================================================================

    fn make_shadow(json: &str) -> ShadowState {
        let mut s = ShadowState::new();
        s.init(json).unwrap();
        s
    }

    // --- PathSegment parsing ---

    #[test]
    fn parse_segments_literal_only() {
        // "form.email" → [Literal("form"), Literal("email")]
        // No wildcards, all literal segments
        let segs = parse_segments("form.email");
        assert_eq!(
            segs,
            vec![
                PathSegment::Literal("form".to_owned()),
                PathSegment::Literal("email".to_owned()),
            ]
        );
    }

    #[test]
    fn parse_segments_with_wildcard_bind() {
        // "form.fields.[*].value" → [Literal("form"), Literal("fields"), WildcardBind, Literal("value")]
        let segs = parse_segments("form.fields.[*].value");
        assert_eq!(
            segs,
            vec![
                PathSegment::Literal("form".to_owned()),
                PathSegment::Literal("fields".to_owned()),
                PathSegment::WildcardBind,
                PathSegment::Literal("value".to_owned()),
            ]
        );
    }

    #[test]
    fn parse_segments_with_wildcard_all() {
        // "form.fields.[**].error" → [Literal("form"), Literal("fields"), WildcardAll, Literal("error")]
        let segs = parse_segments("form.fields.[**].error");
        assert_eq!(
            segs,
            vec![
                PathSegment::Literal("form".to_owned()),
                PathSegment::Literal("fields".to_owned()),
                PathSegment::WildcardAll,
                PathSegment::Literal("error".to_owned()),
            ]
        );
    }

    #[test]
    fn parse_segments_multiple_wildcards() {
        // "form.[*].fields.[**].error" → [Literal, WildcardBind, Literal, WildcardAll, Literal]
        let segs = parse_segments("form.[*].fields.[**].error");
        assert_eq!(
            segs,
            vec![
                PathSegment::Literal("form".to_owned()),
                PathSegment::WildcardBind,
                PathSegment::Literal("fields".to_owned()),
                PathSegment::WildcardAll,
                PathSegment::Literal("error".to_owned()),
            ]
        );
    }

    // --- Trigger matching ---

    #[test]
    fn match_trigger_exact_literal() {
        // Pattern: "form.email" against path: "form.email" → Some(captures=[])
        let pattern = parse_segments("form.email");
        let result = match_trigger("form.email", &pattern);
        assert_eq!(result, Some(vec![]));
    }

    #[test]
    fn match_trigger_literal_mismatch() {
        // Pattern: "form.email" against path: "form.name" → None
        let pattern = parse_segments("form.email");
        let result = match_trigger("form.name", &pattern);
        assert_eq!(result, None);
    }

    #[test]
    fn match_trigger_length_mismatch() {
        // Pattern: "form.email" (2 segments) against path: "form.fields.email" (3 segments) → None
        let pattern = parse_segments("form.email");
        let result = match_trigger("form.fields.email", &pattern);
        assert_eq!(result, None);
    }

    #[test]
    fn match_trigger_single_wildcard_capture() {
        // Pattern: "form.fields.[*].value"
        // Path:    "form.fields.email.value"
        // → Some(captures=["email"])
        let pattern = parse_segments("form.fields.[*].value");
        let result = match_trigger("form.fields.email.value", &pattern);
        assert_eq!(result, Some(vec!["email".to_owned()]));
    }

    #[test]
    fn match_trigger_multiple_wildcard_captures() {
        // Pattern: "form.[*].[*].value"
        // Path:    "form.billing.address.value"
        // → Some(captures=["billing", "address"])
        let pattern = parse_segments("form.[*].[*].value");
        let result = match_trigger("form.billing.address.value", &pattern);
        assert_eq!(
            result,
            Some(vec!["billing".to_owned(), "address".to_owned()])
        );
    }

    #[test]
    fn match_trigger_wildcard_at_end() {
        // Pattern: "form.fields.[*]"
        // Path:    "form.fields.email"
        // → Some(captures=["email"])
        let pattern = parse_segments("form.fields.[*]");
        let result = match_trigger("form.fields.email", &pattern);
        assert_eq!(result, Some(vec!["email".to_owned()]));
    }

    #[test]
    fn match_trigger_wildcard_no_match_wrong_literal() {
        // Pattern: "form.fields.[*].value"
        // Path:    "form.items.email.value"
        // → None (fields != items)
        let pattern = parse_segments("form.fields.[*].value");
        let result = match_trigger("form.items.email.value", &pattern);
        assert_eq!(result, None);
    }

    // --- Target resolution: direct (no wildcards) ---

    #[test]
    fn resolve_direct_target_clears_to_null() {
        // Shadow: { form: { errors: "some error" } }
        // Target: "form.errors" (direct, interned ID)
        // → clears form.errors to null, returns one Change
        let mut shadow = make_shadow(r#"{"form": {"errors": "some error"}}"#);
        let mut intern = InternTable::new();
        let target_id = intern.intern("form.errors");
        let targets = vec![ClearTarget::Direct(target_id)];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &[],
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 1);
        assert_eq!(clears[0].path, "form.errors");
        assert_eq!(clears[0].value_json, "null");
        assert!(shadow.is_null("form.errors"));
    }

    #[test]
    fn resolve_direct_target_already_null_skips() {
        // Shadow: { form: { errors: null } }
        // Target: "form.errors" (direct)
        // → already null, no Change produced (deduplication)
        let mut shadow = make_shadow(r#"{"form": {"errors": null}}"#);
        let mut intern = InternTable::new();
        let target_id = intern.intern("form.errors");
        let targets = vec![ClearTarget::Direct(target_id)];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &[],
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 0);
    }

    // --- Target resolution: WildcardBind (correlated) ---

    #[test]
    fn resolve_wildcard_bind_substitutes_capture() {
        // Shadow: { form: { fields: { email: { error: "required", value: "x" } } } }
        // Target segments: [Literal("form"), Literal("fields"), WildcardBind, Literal("error")]
        // Captures: ["email"]
        // → resolves to "form.fields.email.error", clears to null
        let mut shadow =
            make_shadow(r#"{"form": {"fields": {"email": {"error": "required", "value": "x"}}}}"#);
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.fields.[*].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let captures = vec!["email".to_owned()];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &captures,
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 1);
        assert_eq!(clears[0].path, "form.fields.email.error");
        assert_eq!(clears[0].value_json, "null");
    }

    #[test]
    fn resolve_wildcard_bind_multi_capture() {
        // Shadow: { form: { billing: { address: { error: "bad", value: "123" } } } }
        // Target segments: [Literal("form"), WildcardBind, WildcardBind, Literal("error")]
        // Captures: ["billing", "address"]
        // → resolves to "form.billing.address.error"
        let mut shadow =
            make_shadow(r#"{"form": {"billing": {"address": {"error": "bad", "value": "123"}}}}"#);
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.[*].[*].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let captures = vec!["billing".to_owned(), "address".to_owned()];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &captures,
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 1);
        assert_eq!(clears[0].path, "form.billing.address.error");
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
        let mut shadow = make_shadow(
            r#"{"form": {"fields": {"email": {"error": "e1"}, "name": {"error": "e2"}, "age": {"error": null}}}}"#,
        );
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.fields.[**].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &[],
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        // Should clear email.error and name.error but skip age.error (already null)
        assert_eq!(clears.len(), 2);
        let paths: HashSet<String> = clears.iter().map(|c| c.path.clone()).collect();
        assert!(paths.contains("form.fields.email.error"));
        assert!(paths.contains("form.fields.name.error"));
    }

    #[test]
    fn resolve_wildcard_all_empty_object() {
        // Shadow: { form: { fields: {} } }
        // Target segments with [**] at fields level
        // → no keys to expand, produces zero changes
        let mut shadow = make_shadow(r#"{"form": {"fields": {}}}"#);
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.fields.[**].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &[],
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 0);
    }

    #[test]
    fn resolve_wildcard_all_non_object_produces_nothing() {
        // Shadow: { form: { fields: "not an object" } }
        // Target segments with [**] at fields level
        // → fields is a string, not an object, produces zero changes
        let mut shadow = make_shadow(r#"{"form": {"fields": "not an object"}}"#);
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.fields.[**].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &[],
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 0);
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
        let mut shadow = make_shadow(
            r#"{"form": {"billing": {"fields": {"street": {"error": "e1"}, "city": {"error": "e2"}}}}}"#,
        );
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.[*].fields.[**].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let captures = vec!["billing".to_owned()];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &captures,
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 2);
        let paths: HashSet<String> = clears.iter().map(|c| c.path.clone()).collect();
        assert!(paths.contains("form.billing.fields.street.error"));
        assert!(paths.contains("form.billing.fields.city.error"));
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
        let mut shadow = make_shadow(
            r#"{"form": {"billing": {"fields": {"street": {"error": "e1"}}}, "shipping": {"fields": {"street": {"error": "e2"}}}}}"#,
        );
        let mut intern = InternTable::new();
        let target_segs = parse_segments("form.[**].fields.[*].error");
        let targets = vec![ClearTarget::Wildcard(target_segs)];
        let captures = vec!["street".to_owned()];
        let mut clears = Vec::new();
        let mut seen = HashSet::new();
        resolve_and_clear(
            &targets,
            &captures,
            &mut intern,
            &mut shadow,
            &mut clears,
            &mut seen,
        );
        assert_eq!(clears.len(), 2);
        let paths: HashSet<String> = clears.iter().map(|c| c.path.clone()).collect();
        assert!(paths.contains("form.billing.fields.street.error"));
        assert!(paths.contains("form.shipping.fields.street.error"));
    }

    // =========================================================================
    // Complex integration scenarios (pipeline-level, using ProcessingPipeline)
    // =========================================================================

    /// Helper to set up pipeline with shadow state and clear rules.
    fn setup_pipeline(
        shadow_json: &str,
        clear_rules: Vec<(Vec<&str>, Vec<&str>)>,
    ) -> ProcessingPipeline {
        let mut pipeline = ProcessingPipeline::new();
        pipeline.shadow_init(shadow_json).unwrap();
        for (triggers, targets) in clear_rules {
            let triggers: Vec<String> = triggers.iter().map(|s| s.to_string()).collect();
            let targets: Vec<String> = targets.iter().map(|s| s.to_string()).collect();
            pipeline
                .register_clear_paths("test", &triggers, &targets)
                .unwrap();
        }
        pipeline
    }

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
        let mut pipeline = setup_pipeline(
            r#"{"form": {"email": "a@b.com", "errors": "invalid", "touched": true}}"#,
            vec![(vec!["form.email"], vec!["form.errors", "form.touched"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"new@b.com\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.email"));
        assert!(paths.contains(&"form.errors"));
        assert!(paths.contains(&"form.touched"));
        // Verify cleared values
        for c in &result.changes {
            if c.path == "form.errors" || c.path == "form.touched" {
                assert_eq!(c.value_json, "null");
            }
        }
    }

    #[test]
    fn pipeline_clear_skips_when_trigger_is_noop() {
        // Shadow: { form: { email: "same", errors: "err" } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        //
        // Change: form.email = "same" (no-op, diff filters it out at Step 0)
        // Expected: no changes at all (early exit), errors NOT cleared
        let mut pipeline = setup_pipeline(
            r#"{"form": {"email": "same", "errors": "err"}}"#,
            vec![(vec!["form.email"], vec!["form.errors"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"same\"".to_owned(),
            }])
            .unwrap();
        assert!(result.changes.is_empty());
    }

    #[test]
    fn pipeline_clear_target_already_null() {
        // Shadow: { form: { email: "a@b.com", errors: null } }
        // Clear rule: triggers=["form.email"], targets=["form.errors"]
        //
        // Change: form.email = "new@b.com"
        // Expected: form.email change only, form.errors already null → not in output
        let mut pipeline = setup_pipeline(
            r#"{"form": {"email": "a@b.com", "errors": null}}"#,
            vec![(vec!["form.email"], vec!["form.errors"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"new@b.com\"".to_owned(),
            }])
            .unwrap();
        assert_eq!(result.changes.len(), 1);
        assert_eq!(result.changes[0].path, "form.email");
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
        let mut pipeline = ProcessingPipeline::new();
        pipeline
            .shadow_init(
                r#"{"form": {"email": "a", "errors": "err"}, "mirror": {"errors": "err"}}"#,
            )
            .unwrap();
        pipeline
            .register_clear_paths(
                "test",
                &["form.email".to_owned()],
                &["form.errors".to_owned()],
            )
            .unwrap();
        pipeline
            .register_sync_batch(r#"[["form.errors", "mirror.errors"]]"#)
            .unwrap();

        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"b\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.email"));
        assert!(paths.contains(&"form.errors"));
        assert!(paths.contains(&"mirror.errors"));
        // Mirror should be null (synced from cleared form.errors)
        let mirror = result
            .changes
            .iter()
            .find(|c| c.path == "mirror.errors")
            .unwrap();
        assert_eq!(mirror.value_json, "null");
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
        let mut pipeline = ProcessingPipeline::new();
        pipeline
            .shadow_init(r#"{"toggle": false, "enabled": true, "disabled": false}"#)
            .unwrap();
        pipeline
            .register_clear_paths("test", &["toggle".to_owned()], &["enabled".to_owned()])
            .unwrap();
        pipeline
            .register_flip_batch(r#"[["enabled", "disabled"]]"#)
            .unwrap();

        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "toggle".to_owned(),
                value_json: "true".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"toggle"));
        assert!(paths.contains(&"enabled"));
        // disabled should NOT be in output (null is not boolean, flip skips)
        assert!(!paths.contains(&"disabled"));
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
        let mut pipeline = ProcessingPipeline::new();
        pipeline
            .shadow_init(r#"{"form": {"email": "a@b.com", "errors": "invalid"}}"#)
            .unwrap();
        pipeline
            .register_clear_paths(
                "test",
                &["form.email".to_owned()],
                &["form.errors".to_owned()],
            )
            .unwrap();
        pipeline
            .register_boollogic("_concerns.form.hasErrors", r#"{"EXISTS": "form.errors"}"#)
            .unwrap();

        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"new@b.com\"".to_owned(),
            }])
            .unwrap();
        // BoolLogic concern should be in output
        let concern = result
            .changes
            .iter()
            .find(|c| c.path == "_concerns.form.hasErrors");
        assert!(concern.is_some());
        assert_eq!(concern.unwrap().value_json, "false");
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
        let mut pipeline = ProcessingPipeline::new();
        pipeline
            .shadow_init(
                r#"{"items": {"a": {"price": 10}, "b": {"price": 10}}, "totals": {"price": 10}}"#,
            )
            .unwrap();
        pipeline
            .register_clear_paths(
                "test",
                &["items.a.price".to_owned()],
                &["items.b.price".to_owned()],
            )
            .unwrap();
        pipeline
            .register_aggregation_batch(
                r#"[["totals.price", "items.a.price"], ["totals.price", "items.b.price"]]"#,
            )
            .unwrap();

        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "items.a.price".to_owned(),
                value_json: "20".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"items.a.price"));
        assert!(paths.contains(&"items.b.price"));
        // totals.price should be null (sources are [20, null], not all-equal)
        let totals = result.changes.iter().find(|c| c.path == "totals.price");
        assert!(totals.is_some());
        assert_eq!(totals.unwrap().value_json, "null");
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
        let mut pipeline = setup_pipeline(
            r#"{"a": "x", "b": "y", "c": "z"}"#,
            vec![(vec!["a"], vec!["b"]), (vec!["b"], vec!["c"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "a".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"a"));
        assert!(paths.contains(&"b"));
        // c should NOT be cleared
        assert!(!paths.contains(&"c"));
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
        let mut pipeline = setup_pipeline(
            r#"{"form": {"fields": {"email": {"value": "old@b.com", "error": "invalid"}, "name": {"value": "Alice", "error": "too short"}}}}"#,
            vec![(vec!["form.fields.[*].value"], vec!["form.fields.[*].error"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.fields.email.value".to_owned(),
                value_json: "\"new@b.com\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.fields.email.value"));
        assert!(paths.contains(&"form.fields.email.error"));
        // name.error should NOT be cleared
        assert!(!paths.contains(&"form.fields.name.error"));
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
        let mut pipeline = setup_pipeline(
            r#"{"form": {"fields": {"email": {"value": "old", "error": "e1"}, "name": {"value": "old", "error": "e2"}, "age": {"value": "old", "error": "e3"}}}}"#,
            vec![(
                vec!["form.fields.[*].value"],
                vec!["form.fields.[**].error"],
            )],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.fields.email.value".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.fields.email.value"));
        assert!(paths.contains(&"form.fields.email.error"));
        assert!(paths.contains(&"form.fields.name.error"));
        assert!(paths.contains(&"form.fields.age.error"));
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
        let mut pipeline = setup_pipeline(
            r#"{"form": {"fields": {"email": {"value": "x", "error": "e"}}}, "other": {"fields": {"email": {"value": "y", "error": "f"}}}}"#,
            vec![(vec!["form.fields.[*].value"], vec!["form.fields.[*].error"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "other.fields.email.value".to_owned(),
                value_json: "\"z\"".to_owned(),
            }])
            .unwrap();
        assert_eq!(result.changes.len(), 1);
        assert_eq!(result.changes[0].path, "other.fields.email.value");
    }

    #[test]
    fn pipeline_clear_multiple_triggers_same_rule() {
        // Shadow: { form: { email: "a", name: "b", errors: "err", touched: true } }
        // Clear rule: triggers=["form.email", "form.name"], targets=["form.errors", "form.touched"]
        //
        // Change: form.email = "new"
        // Expected: form.email + form.errors=null + form.touched=null
        let mut pipeline = setup_pipeline(
            r#"{"form": {"email": "a", "name": "b", "errors": "err", "touched": true}}"#,
            vec![(
                vec!["form.email", "form.name"],
                vec!["form.errors", "form.touched"],
            )],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.email"));
        assert!(paths.contains(&"form.errors"));
        assert!(paths.contains(&"form.touched"));

        // Second call: form.name = "new" — targets are already null, so should be skipped
        let result2 = pipeline
            .process_changes_vec(vec![Change {
                path: "form.name".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let paths2: Vec<&str> = result2.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths2.contains(&"form.name"));
        // form.errors and form.touched already null, should not appear
        assert!(!paths2.contains(&"form.errors"));
        assert!(!paths2.contains(&"form.touched"));
    }

    #[test]
    fn pipeline_clear_deduplicates_overlapping_targets() {
        // Shadow: { a: "x", b: "y", target: "z" }
        // Clear rule 1: triggers=["a"], targets=["target"]
        // Clear rule 2: triggers=["b"], targets=["target"]
        //
        // Change both: a = "x2", b = "y2"
        // Expected: a, b, target=null (only once, deduped via `seen` set)
        let mut pipeline = setup_pipeline(
            r#"{"a": "x", "b": "y", "target": "z"}"#,
            vec![(vec!["a"], vec!["target"]), (vec!["b"], vec!["target"])],
        );
        let result = pipeline
            .process_changes_vec(vec![
                Change {
                    path: "a".to_owned(),
                    value_json: "\"x2\"".to_owned(),
                },
                Change {
                    path: "b".to_owned(),
                    value_json: "\"y2\"".to_owned(),
                },
            ])
            .unwrap();
        let target_count = result.changes.iter().filter(|c| c.path == "target").count();
        assert_eq!(target_count, 1, "target should appear exactly once");
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
        let mut pipeline = ProcessingPipeline::new();
        pipeline
            .shadow_init(r#"{"form": {"email": "old", "errors": {"email": "e1", "name": "e2", "nested": {"deep": "e3"}}}}"#)
            .unwrap();
        pipeline
            .register_clear_paths(
                "test",
                &["form.email".to_owned()],
                &["form.errors".to_owned()],
            )
            .unwrap();

        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "form.email".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let errors_change = result.changes.iter().find(|c| c.path == "form.errors");
        assert!(errors_change.is_some());
        assert_eq!(errors_change.unwrap().value_json, "null");
        // Verify shadow state
        assert_eq!(pipeline.shadow_get("form.errors"), Some("null".to_owned()));
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
        let mut pipeline = setup_pipeline(
            r#"{"app": {"section1": {"fieldA": {"value": "v1", "error": "e1"}, "fieldB": {"value": "v2", "error": "e2"}}, "section2": {"fieldC": {"value": "v3", "error": "e3"}}}}"#,
            vec![(vec!["app.[*].[*].value"], vec!["app.[*].[*].error"])],
        );
        let result = pipeline
            .process_changes_vec(vec![Change {
                path: "app.section1.fieldA.value".to_owned(),
                value_json: "\"new\"".to_owned(),
            }])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"app.section1.fieldA.value"));
        assert!(paths.contains(&"app.section1.fieldA.error"));
        assert!(!paths.contains(&"app.section1.fieldB.error"));
        assert!(!paths.contains(&"app.section2.fieldC.error"));
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
        let mut pipeline = setup_pipeline(
            r#"{"form": {"fields": {"email": {"value": "old1", "error": "e1"}, "name": {"value": "old2", "error": "e2"}}}}"#,
            vec![(vec!["form.fields.[*].value"], vec!["form.fields.[*].error"])],
        );
        let result = pipeline
            .process_changes_vec(vec![
                Change {
                    path: "form.fields.email.value".to_owned(),
                    value_json: "\"new1\"".to_owned(),
                },
                Change {
                    path: "form.fields.name.value".to_owned(),
                    value_json: "\"new2\"".to_owned(),
                },
            ])
            .unwrap();
        let paths: Vec<&str> = result.changes.iter().map(|c| c.path.as_str()).collect();
        assert!(paths.contains(&"form.fields.email.error"));
        assert!(paths.contains(&"form.fields.name.error"));
    }

    // =========================================================================
    // Registration and validation
    // =========================================================================

    #[test]
    fn registration_concrete_rule() {
        // Register: triggers=["a"], targets=["b"]
        // Verify: direct_triggers has entry for interned "a" → [Direct(interned "b")]
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        registry
            .register("test", &["a".to_owned()], &["b".to_owned()], &mut intern)
            .unwrap();
        let a_id = intern.get_id("a").unwrap();
        assert!(registry.direct_triggers.contains_key(&a_id));
        assert!(registry.wildcard_triggers.is_empty());
    }

    #[test]
    fn registration_wildcard_trigger_rule() {
        // Register: triggers=["form.[*].value"], targets=["form.[*].error"]
        // Verify: wildcard_triggers has one entry with correct segments
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        registry
            .register(
                "test",
                &["form.[*].value".to_owned()],
                &["form.[*].error".to_owned()],
                &mut intern,
            )
            .unwrap();
        assert!(registry.direct_triggers.is_empty());
        assert_eq!(registry.wildcard_triggers.len(), 1);
        assert_eq!(
            registry.wildcard_triggers[0].segments,
            vec![
                PathSegment::Literal("form".to_owned()),
                PathSegment::WildcardBind,
                PathSegment::Literal("value".to_owned()),
            ]
        );
    }

    #[test]
    fn registration_mixed_triggers_same_rule() {
        // Register: triggers=["form.email", "form.fields.[*].value"], targets=["form.errors"]
        // Verify: "form.email" in direct_triggers, wildcard pattern in wildcard_triggers
        //         both pointing to same target
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        registry
            .register(
                "test",
                &["form.email".to_owned(), "form.fields.[*].value".to_owned()],
                &["form.errors".to_owned()],
                &mut intern,
            )
            .unwrap();
        let email_id = intern.get_id("form.email").unwrap();
        assert!(registry.direct_triggers.contains_key(&email_id));
        assert_eq!(registry.wildcard_triggers.len(), 1);
    }

    #[test]
    fn registration_rejects_wildcard_all_in_trigger() {
        // Attempt: triggers=["form.[**].value"], targets=["form.errors"]
        // Expected: error — [**] (WildcardAll) not allowed in triggers
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        let result = registry.register(
            "test",
            &["form.[**].value".to_owned()],
            &["form.errors".to_owned()],
            &mut intern,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expand-all wildcards"));
    }

    #[test]
    fn registration_rejects_unmatched_bind_in_target() {
        // Attempt: triggers=["form.email"], targets=["form.[*].error"]
        // Expected: error — target has [*] (WildcardBind) but trigger has no [*] to capture from
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        let result = registry.register(
            "test",
            &["form.email".to_owned()],
            &["form.[*].error".to_owned()],
            &mut intern,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("positional wildcards"));
    }

    #[test]
    fn registration_allows_wildcard_all_in_target_without_trigger_bind() {
        // Attempt: triggers=["form.email"], targets=["form.fields.[**].error"]
        // Expected: OK — [**] doesn't need a trigger capture
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        let result = registry.register(
            "test",
            &["form.email".to_owned()],
            &["form.fields.[**].error".to_owned()],
            &mut intern,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn unregistration_removes_rules() {
        // Register with registration_id = "test-1"
        // Verify triggers exist
        // Unregister "test-1"
        // Verify triggers removed
        let mut registry = ClearPathsRegistry::new();
        let mut intern = InternTable::new();
        registry
            .register("test-1", &["a".to_owned()], &["b".to_owned()], &mut intern)
            .unwrap();
        assert!(!registry.is_empty());

        registry.unregister("test-1");
        assert!(registry.is_empty());
    }
}
