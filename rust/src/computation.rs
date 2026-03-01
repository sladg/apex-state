//! Computation processor.
//!
//! Handles unidirectional numeric reduction operations (SUM, AVG).
//! Sources flow into a target path via a reduction operation.
//! Unlike aggregation (bidirectional all-equal), computations are source→target only.
//!
//! Algorithm:
//! 1. Identify affected computation targets from changed paths
//! 2. For each target, filter active sources (evaluate exclude_when)
//! 3. Read source values from shadow, parse as f64 (skip non-numeric/missing)
//! 4. SUM: sum of present values (0 if none)
//! 5. AVG: sum/count (null if no valid sources)
//! 6. No-op filter against current target in shadow

use crate::bool_logic::BoolLogicNode;
use crate::change::{Change, ChangeKind, Lineage, UNDEFINED_SENTINEL_JSON};
use crate::shadow::ShadowState;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use ts_rs::TS;

/// Supported computation operations.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, TS)]
pub enum ComputationOp {
    Sum,
    Avg,
}

/// A single computation source with an optional exclude condition.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
pub struct ComputationSource {
    pub path: String,
    pub exclude_when: Option<BoolLogicNode>,
}

/// A single computation: operation + target path + multiple sources.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
pub struct Computation {
    pub op: ComputationOp,
    pub target: String,
    #[ts(inline)]
    pub sources: Vec<ComputationSource>,
}

/// Registry of all registered computations, keyed by target path.
#[derive(Debug)]
pub(crate) struct ComputationRegistry {
    computations: HashMap<String, Computation>,
    /// Reverse index: source path → target paths (for reactive updates)
    source_to_targets: HashMap<String, Vec<String>>,
    /// Reverse index: BoolLogic condition path → target paths (for condition re-evaluation)
    condition_path_to_targets: HashMap<String, Vec<String>>,
}

impl ComputationRegistry {
    pub(crate) fn new() -> Self {
        Self {
            computations: HashMap::new(),
            source_to_targets: HashMap::new(),
            condition_path_to_targets: HashMap::new(),
        }
    }

    /// Register a single computation.
    pub(crate) fn register(
        &mut self,
        op: ComputationOp,
        target: String,
        sources: Vec<ComputationSource>,
    ) {
        // Update reverse index: each source points to this target
        for source in &sources {
            self.source_to_targets
                .entry(source.path.clone())
                .or_default()
                .push(target.clone());

            // Extract paths from BoolLogic conditions and index them
            if let Some(ref condition) = source.exclude_when {
                for cond_path in condition.extract_paths() {
                    self.condition_path_to_targets
                        .entry(cond_path)
                        .or_default()
                        .push(target.clone());
                }
            }
        }

        // Register the computation
        self.computations.insert(
            target.clone(),
            Computation {
                op,
                target,
                sources,
            },
        );
    }

    /// Unregister a single computation by target path.
    #[allow(dead_code)] // Called via WASM exports (invisible to clippy)
    pub(crate) fn unregister(&mut self, target: &str) {
        // Remove from reverse indices
        if let Some(comp) = self.computations.get(target) {
            for source in &comp.sources {
                if let Some(targets) = self.source_to_targets.get_mut(&source.path) {
                    targets.retain(|t| t != target);
                    if targets.is_empty() {
                        self.source_to_targets.remove(&source.path);
                    }
                }

                // Clean up condition path reverse index
                if let Some(ref condition) = source.exclude_when {
                    for cond_path in condition.extract_paths() {
                        if let Some(targets) = self.condition_path_to_targets.get_mut(&cond_path) {
                            targets.retain(|t| t != target);
                            if targets.is_empty() {
                                self.condition_path_to_targets.remove(&cond_path);
                            }
                        }
                    }
                }
            }
        }

        // Remove the computation
        self.computations.remove(target);
    }

    /// Check if a path is a computation target.
    pub(crate) fn is_computation_target(&self, path: &str) -> bool {
        self.computations.contains_key(path)
    }

    /// Get affected computation targets for a set of changed paths.
    /// Returns unique target paths that need recomputation.
    /// Checks both source paths and condition paths.
    pub(crate) fn get_affected_targets(&self, changed_paths: &[String]) -> Vec<String> {
        let mut targets = std::collections::HashSet::new();

        for path in changed_paths {
            // Direct match: path is a source
            if let Some(target_list) = self.source_to_targets.get(path) {
                targets.extend(target_list.iter().cloned());
            }

            // Direct match: path is a condition dependency
            if let Some(target_list) = self.condition_path_to_targets.get(path) {
                targets.extend(target_list.iter().cloned());
            }

            // Parent match: path is a child of a source
            for (source, target_list) in &self.source_to_targets {
                if crate::is_child_path(path, source) {
                    targets.extend(target_list.iter().cloned());
                }
            }

            // Parent match for condition paths
            for (cond_path, target_list) in &self.condition_path_to_targets {
                if crate::is_child_path(path, cond_path) {
                    targets.extend(target_list.iter().cloned());
                }
            }
        }

        targets.into_iter().collect()
    }

    /// Dump all registered computations as (target, op_str, sources) triples (debug only).
    pub(crate) fn dump_infos(&self) -> Vec<(String, String, Vec<String>)> {
        self.computations
            .iter()
            .map(|(target, comp)| {
                let op = match comp.op {
                    ComputationOp::Sum => "SUM".to_owned(),
                    ComputationOp::Avg => "AVG".to_owned(),
                };
                let sources = comp.sources.iter().map(|s| s.path.clone()).collect();
                (target.clone(), op, sources)
            })
            .collect()
    }
}

/// Try to extract an f64 from a shadow value.
/// Returns None for missing, null, undefined sentinel, and non-numeric values.
fn try_extract_f64(shadow: &ShadowState, path: &str) -> Option<f64> {
    let value_repr = shadow.get(path)?;
    let json_str =
        serde_json::to_string(&value_repr.to_json_value()).unwrap_or_else(|_| "null".to_string());

    // Skip undefined sentinel and null
    if json_str == UNDEFINED_SENTINEL_JSON || json_str == "null" {
        return None;
    }

    // Try parsing as f64
    json_str.parse::<f64>().ok()
}

/// Process computation reads: recompute target values when sources change.
///
/// Algorithm:
/// 1. Identify affected computations (where any source or condition changed)
/// 2. Filter active sources (evaluate exclude_when via BoolLogic)
/// 3. Read values from shadow, parse as f64 (skip non-numeric/missing)
/// 4. SUM: sum of present values (0 if none active)
/// 5. AVG: sum/count (null if no valid sources)
/// 6. No-op filter against current target in shadow
pub(crate) fn process_computation_reads(
    registry: &ComputationRegistry,
    shadow: &ShadowState,
    changed_paths: &[String],
) -> Vec<Change> {
    let affected_targets = registry.get_affected_targets(changed_paths);

    if affected_targets.is_empty() {
        return Vec::new();
    }

    let mut changes = Vec::new();

    for target_path in affected_targets {
        if let Some(comp) = registry.computations.get(&target_path) {
            // Filter out excluded sources (condition evaluates to true = excluded)
            let active_sources: Vec<&ComputationSource> = comp
                .sources
                .iter()
                .filter(|s| {
                    s.exclude_when
                        .as_ref()
                        .is_none_or(|cond| !cond.evaluate(shadow))
                })
                .collect();

            // Collect numeric values from active sources
            let values: Vec<f64> = active_sources
                .iter()
                .filter_map(|s| try_extract_f64(shadow, &s.path))
                .collect();

            let desired_value = match comp.op {
                ComputationOp::Sum => {
                    if active_sources.is_empty() {
                        // All sources excluded → SUM = 0
                        "0".to_string()
                    } else {
                        // Sum of present values (missing/non-numeric skipped, 0 if all skipped)
                        let sum: f64 = values.iter().sum();
                        format_f64(sum)
                    }
                }
                ComputationOp::Avg => {
                    if active_sources.is_empty() || values.is_empty() {
                        // All excluded or no valid numeric values → null (undefined sentinel)
                        UNDEFINED_SENTINEL_JSON.to_string()
                    } else {
                        let sum: f64 = values.iter().sum();
                        let avg = sum / values.len() as f64;
                        format_f64(avg)
                    }
                }
            };

            // Filter no-op: only create change if target value actually differs
            let current_target = shadow.get(&comp.target).map(|v| {
                serde_json::to_string(&v.to_json_value()).unwrap_or_else(|_| "null".to_string())
            });

            if current_target.as_ref() != Some(&desired_value) {
                changes.push(Change {
                    path: comp.target.clone(),
                    value_json: desired_value,
                    kind: ChangeKind::Real,
                    lineage: Lineage::Input,
                    audit: None,
                    ..Default::default()
                });
            }
        }
    }

    changes
}

/// Format f64 as a clean JSON number (integer when possible).
fn format_f64(value: f64) -> String {
    if value.fract() == 0.0 && value.abs() < (i64::MAX as f64) {
        format!("{}", value as i64)
    } else {
        // Use serde_json for proper formatting
        serde_json::to_string(&value).unwrap_or_else(|_| format!("{}", value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: create simple ComputationSource with no condition
    fn src(path: &str) -> ComputationSource {
        ComputationSource {
            path: path.to_string(),
            exclude_when: None,
        }
    }

    /// Helper: source path names for changed_paths
    fn paths(names: &[&str]) -> Vec<String> {
        names.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn test_sum_basic() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"total": 0, "price1": 10, "price2": 20, "price3": 30}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![src("price1"), src("price2"), src("price3")],
        );

        let changes =
            process_computation_reads(&registry, &shadow, &paths(&["price1", "price2", "price3"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "total");
        assert_eq!(changes[0].value_json, "60");
    }

    #[test]
    fn test_avg_basic() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"average": 0, "score1": 10, "score2": 20, "score3": 30}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Avg,
            "average".to_string(),
            vec![src("score1"), src("score2"), src("score3")],
        );

        let changes =
            process_computation_reads(&registry, &shadow, &paths(&["score1", "score2", "score3"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "average");
        assert_eq!(changes[0].value_json, "20");
    }

    #[test]
    fn test_sum_all_excluded_gives_zero() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"total": 100, "price1": 10, "disabled": true}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![ComputationSource {
                path: "price1".to_string(),
                exclude_when: Some(BoolLogicNode::IsEqual(
                    "disabled".to_string(),
                    serde_json::Value::Bool(true),
                )),
            }],
        );

        let changes = process_computation_reads(&registry, &shadow, &paths(&["price1"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "total");
        assert_eq!(changes[0].value_json, "0");
    }

    #[test]
    fn test_avg_all_excluded_gives_undefined() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"average": 50, "score1": 10, "disabled": true}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Avg,
            "average".to_string(),
            vec![ComputationSource {
                path: "score1".to_string(),
                exclude_when: Some(BoolLogicNode::IsEqual(
                    "disabled".to_string(),
                    serde_json::Value::Bool(true),
                )),
            }],
        );

        let changes = process_computation_reads(&registry, &shadow, &paths(&["score1"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "average");
        assert_eq!(changes[0].value_json, UNDEFINED_SENTINEL_JSON);
    }

    #[test]
    fn test_sum_skips_non_numeric() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"total": 0, "price1": 10, "price2": "not_a_number", "price3": 30}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![src("price1"), src("price2"), src("price3")],
        );

        let changes =
            process_computation_reads(&registry, &shadow, &paths(&["price1", "price2", "price3"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "total");
        assert_eq!(changes[0].value_json, "40"); // 10 + 30, skip "not_a_number"
    }

    #[test]
    fn test_no_op_filter() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"total": 60, "price1": 10, "price2": 20, "price3": 30}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![src("price1"), src("price2"), src("price3")],
        );

        let changes =
            process_computation_reads(&registry, &shadow, &paths(&["price1", "price2", "price3"]));

        // Target already has the correct value
        assert_eq!(changes.len(), 0);
    }

    #[test]
    fn test_is_computation_target() {
        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![src("price1"), src("price2")],
        );

        assert!(registry.is_computation_target("total"));
        assert!(!registry.is_computation_target("price1"));
        assert!(!registry.is_computation_target("unknown"));
    }

    #[test]
    fn test_single_source() {
        let mut shadow = ShadowState::new();
        shadow.init(r#"{"total": 0, "price1": 42}"#).unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(ComputationOp::Sum, "total".to_string(), vec![src("price1")]);

        let changes = process_computation_reads(&registry, &shadow, &paths(&["price1"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].value_json, "42");
    }

    #[test]
    fn test_avg_single_source() {
        let mut shadow = ShadowState::new();
        shadow.init(r#"{"average": 0, "score1": 42}"#).unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Avg,
            "average".to_string(),
            vec![src("score1")],
        );

        let changes = process_computation_reads(&registry, &shadow, &paths(&["score1"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].value_json, "42");
    }

    #[test]
    fn test_exclude_when_partial() {
        let mut shadow = ShadowState::new();
        shadow
            .init(r#"{"total": 0, "price1": 10, "price2": 20, "price2_disabled": true}"#)
            .unwrap();

        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![
                src("price1"),
                ComputationSource {
                    path: "price2".to_string(),
                    exclude_when: Some(BoolLogicNode::IsEqual(
                        "price2_disabled".to_string(),
                        serde_json::Value::Bool(true),
                    )),
                },
            ],
        );

        let changes = process_computation_reads(&registry, &shadow, &paths(&["price1", "price2"]));

        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, "total");
        assert_eq!(changes[0].value_json, "10"); // Only price1, price2 excluded
    }

    #[test]
    fn test_unregister() {
        let mut registry = ComputationRegistry::new();
        registry.register(
            ComputationOp::Sum,
            "total".to_string(),
            vec![src("price1"), src("price2")],
        );

        assert!(registry.is_computation_target("total"));

        registry.unregister("total");

        assert!(!registry.is_computation_target("total"));
        assert!(registry
            .get_affected_targets(&["price1".to_string()])
            .is_empty());
    }
}
