//! Scoped trace recorder for pipeline debug tracing.
//!
//! All methods are no-ops when disabled (zero cost in production).

use crate::change::{
    Change, ChangeKind, PipelineTrace, ProducedChange, SkipReason, SkippedChange, Stage, StageTrace,
};

// ---------------------------------------------------------------------------
// Timing helpers
// ---------------------------------------------------------------------------

// `performance.now()` binding — sub-millisecond precision in browsers/workers.
// Compiled only for WASM targets; native test builds return 0.0.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen::prelude::wasm_bindgen]
extern "C" {
    #[wasm_bindgen::prelude::wasm_bindgen(js_namespace = performance, js_name = now)]
    fn perf_now() -> f64;
}

/// Current time in milliseconds (sub-millisecond precision via `performance.now()`).
/// Returns 0.0 when compiled for native targets (tests don't measure timing).
#[allow(dead_code)]
pub(crate) fn now_ms() -> f64 {
    #[cfg(target_arch = "wasm32")]
    {
        perf_now()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        0.0
    }
}

/// Elapsed microseconds since `t0` (from `now_ms()`).
/// Returns 0 immediately if `t0 == 0.0` (disabled path — no second JS call).
#[allow(dead_code)]
pub(crate) fn elapsed_us(t0: f64) -> u64 {
    if t0 == 0.0 {
        return 0;
    }
    ((now_ms() - t0) * 1000.0) as u64
}

/// Scoped trace recorder. All methods are no-ops when disabled (zero cost in production).
pub(crate) struct TraceRecorder<'a> {
    trace: &'a mut PipelineTrace,
    enabled: bool,
}

impl<'a> TraceRecorder<'a> {
    /// Create a new trace recorder. When `!enabled`, all methods are no-ops.
    pub fn new(trace: &'a mut PipelineTrace, enabled: bool) -> Self {
        Self { trace, enabled }
    }

    /// Whether tracing is enabled. Use for inner-loop guards that can't use other methods.
    #[inline]
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Return current time (ms) for timing a stage. Returns 0.0 when disabled (no JS call).
    #[inline]
    pub fn stage_timer(&self) -> f64 {
        if self.enabled {
            now_ms()
        } else {
            0.0
        }
    }

    /// Patch the `duration_us` of the last pushed stage entry. No-op when disabled.
    pub fn set_duration(&mut self, duration_us: u64) {
        if !self.enabled {
            return;
        }
        if let Some(last) = self.trace.stages.last_mut() {
            last.duration_us = duration_us;
        }
    }

    /// Push a stage trace entry. No-op when disabled.
    pub fn stage(
        &mut self,
        stage: Stage,
        matched: Vec<[String; 2]>,
        produced: Vec<ProducedChange>,
        skipped: Vec<SkippedChange>,
    ) {
        if !self.enabled {
            return;
        }
        self.trace.stages.push(StageTrace {
            stage,
            duration_us: 0,
            matched,
            produced,
            skipped,
            followup: Vec::new(),
        });
    }

    /// Collect [path, value] pairs from changes for the `matched` field.
    /// Returns empty Vec when disabled.
    pub fn collect_matched(&self, changes: &[Change]) -> Vec<[String; 2]> {
        if !self.enabled {
            return Vec::new();
        }
        changes
            .iter()
            .map(|c| [c.path.clone(), c.value_json.clone()])
            .collect()
    }

    /// Collect paths-only as matched entries (value set to empty string).
    /// Used for stages where only identifiers are available (e.g. BoolLogic IDs).
    /// Returns empty Vec when disabled.
    #[allow(dead_code)]
    pub fn collect_matched_labels(&self, labels: &[String]) -> Vec<[String; 2]> {
        if !self.enabled {
            return Vec::new();
        }
        labels.iter().map(|l| [l.clone(), String::new()]).collect()
    }

    /// Collect ProducedChange entries from changes (simple path+value, no provenance).
    /// Returns empty Vec when disabled.
    pub fn collect_produced(&self, changes: &[Change]) -> Vec<ProducedChange> {
        if !self.enabled {
            return Vec::new();
        }
        changes
            .iter()
            .map(|c| ProducedChange {
                path: c.path.clone(),
                value: c.value_json.clone(),
                registration_id: None,
                source_path: None,
            })
            .collect()
    }

    /// Collect ProducedChange entries with provenance (registration_id and/or source_path).
    /// Returns empty Vec when disabled.
    #[allow(dead_code)]
    pub fn collect_produced_with_provenance(
        &self,
        changes: &[Change],
        registration_id: Option<&str>,
        source_path: Option<&str>,
    ) -> Vec<ProducedChange> {
        if !self.enabled {
            return Vec::new();
        }
        changes
            .iter()
            .map(|c| ProducedChange {
                path: c.path.clone(),
                value: c.value_json.clone(),
                registration_id: registration_id.map(|s| s.to_owned()),
                source_path: source_path.map(|s| s.to_owned()),
            })
            .collect()
    }

    /// Create a SkippedChange with GuardFailed reason and detail string.
    #[allow(dead_code)]
    pub fn skipped_guard(path: &str, kind: &ChangeKind, detail: &str) -> SkippedChange {
        SkippedChange {
            path: path.to_owned(),
            kind: kind.clone(),
            reason: SkipReason::GuardFailed,
            detail: if detail.is_empty() {
                "guard_failed: stage guard condition not met".to_owned()
            } else {
                detail.to_owned()
            },
            registration_id: None,
            anchor_path: None,
        }
    }

    /// Create a SkippedChange with WrongKind reason.
    #[allow(dead_code)]
    pub fn skipped_wrong_kind(path: &str, kind: &ChangeKind, detail: &str) -> SkippedChange {
        SkippedChange {
            path: path.to_owned(),
            kind: kind.clone(),
            reason: SkipReason::WrongKind,
            detail: if detail.is_empty() {
                "wrong_kind: change kind not handled by this stage".to_owned()
            } else {
                detail.to_owned()
            },
            registration_id: None,
            anchor_path: None,
        }
    }

    /// Create a SkippedChange for a missing anchor.
    pub fn skipped_anchor(
        path: &str,
        kind: &ChangeKind,
        anchor_path: &str,
        registration_id: Option<&str>,
    ) -> SkippedChange {
        SkippedChange {
            path: path.to_owned(),
            kind: kind.clone(),
            reason: SkipReason::AnchorMissing,
            detail: format!("anchor_missing: anchor '{}' not in state", anchor_path),
            registration_id: registration_id.map(|s| s.to_owned()),
            anchor_path: Some(anchor_path.to_owned()),
        }
    }

    /// Create a SkippedChange for a redundant value (matches shadow state).
    pub fn skipped_redundant(path: &str) -> SkippedChange {
        SkippedChange {
            path: path.to_owned(),
            kind: ChangeKind::Redundant,
            reason: SkipReason::Redundant,
            detail: "redundant: value unchanged".to_owned(),
            registration_id: None,
            anchor_path: None,
        }
    }

    /// Compute paths in `all` but not in `kept`, as SkippedChange entries.
    /// Both arguments are `[path, value]` matched pairs.
    /// Returns empty Vec when disabled.
    pub fn diff_skipped(&self, all: &[[String; 2]], kept: &[[String; 2]]) -> Vec<SkippedChange> {
        if !self.enabled {
            return Vec::new();
        }
        let kept_paths: Vec<&str> = kept.iter().map(|pair| pair[0].as_str()).collect();
        all.iter()
            .filter(|pair| !kept_paths.contains(&pair[0].as_str()))
            .map(|pair| TraceRecorder::skipped_redundant(&pair[0]))
            .collect()
    }

    /// Set anchor states on the trace. No-op when disabled.
    pub fn set_anchor_states(
        &mut self,
        states: &std::collections::HashMap<u32, bool>,
        intern: &crate::intern::InternTable,
    ) {
        if !self.enabled {
            return;
        }
        for (&path_id, &present) in states {
            if let Some(path_str) = intern.resolve(path_id) {
                self.trace
                    .anchor_states
                    .insert(path_str.to_owned(), present);
            }
        }
    }
}
