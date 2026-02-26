//! Scoped trace recorder for pipeline debug tracing.
//!
//! All methods are no-ops when disabled (zero cost in production).

use crate::change::{
    Change, ChangeKind, PipelineTrace, SkipReason, SkippedChange, Stage, StageTrace,
};

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

    /// Push a stage trace entry. No-op when disabled.
    pub fn stage(
        &mut self,
        stage: Stage,
        accepted: Vec<String>,
        produced: Vec<[String; 2]>,
        skipped: Vec<SkippedChange>,
    ) {
        if !self.enabled {
            return;
        }
        self.trace.stages.push(StageTrace {
            stage,
            duration_us: 0,
            accepted,
            produced,
            skipped,
            followup: Vec::new(),
        });
    }

    /// Collect paths from changes. Returns empty Vec when disabled.
    pub fn collect_paths(&self, changes: &[Change]) -> Vec<String> {
        if !self.enabled {
            return Vec::new();
        }
        changes.iter().map(|c| c.path.clone()).collect()
    }

    /// Collect path-value pairs from changes. Returns empty Vec when disabled.
    pub fn collect_path_value_pairs(&self, changes: &[Change]) -> Vec<[String; 2]> {
        if !self.enabled {
            return Vec::new();
        }
        changes
            .iter()
            .map(|c| [c.path.clone(), c.value_json.clone()])
            .collect()
    }

    /// Create a SkippedChange with GuardFailed reason.
    pub fn skipped_guard(path: &str, kind: &ChangeKind) -> SkippedChange {
        SkippedChange {
            path: path.to_owned(),
            kind: kind.clone(),
            reason: SkipReason::GuardFailed,
        }
    }

    /// Compute paths in `all` but not in `kept`, as SkippedChange entries.
    /// Returns empty Vec when disabled.
    pub fn diff_skipped(&self, all: &[String], kept: &[String]) -> Vec<SkippedChange> {
        if !self.enabled {
            return Vec::new();
        }
        all.iter()
            .filter(|p| !kept.contains(p))
            .map(|p| SkippedChange {
                path: p.clone(),
                kind: ChangeKind::Real,
                reason: SkipReason::GuardFailed,
            })
            .collect()
    }
}
