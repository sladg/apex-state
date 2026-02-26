use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

pub mod aggregation;
pub mod bool_logic;
pub mod change;
mod clear_paths;
pub mod computation;
mod diff;
mod functions;
mod graphs;
mod intern;
pub mod pipeline;
mod rev_index;
pub mod router;
mod shadow;
pub mod value_logic;

use change::Change;
use pipeline::ProcessingPipeline;

/// Join two path segments with a dot separator, pre-allocating the exact capacity.
/// Zero-allocation alternative to `format!("{}.{}", a, b)` for path building.
#[inline]
pub(crate) fn join_path(prefix: &str, suffix: &str) -> String {
    let mut s = String::with_capacity(prefix.len() + 1 + suffix.len());
    s.push_str(prefix);
    s.push('.');
    s.push_str(suffix);
    s
}

/// Check if `path` is a child of `parent` (i.e. starts with `parent` + '.').
/// Zero-allocation alternative to `path.starts_with(&format!("{}.", parent))`.
#[inline]
pub(crate) fn is_child_path(path: &str, parent: &str) -> bool {
    path.len() > parent.len() && path.as_bytes()[parent.len()] == b'.' && path.starts_with(parent)
}

#[cfg(feature = "console_error_panic_hook")]
fn set_panic_hook() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen(start)]
pub fn main() {
    #[cfg(feature = "console_error_panic_hook")]
    set_panic_hook();
}

thread_local! {
    static PIPELINES: RefCell<HashMap<u32, ProcessingPipeline>> = RefCell::new(HashMap::new());
    static NEXT_ID: Cell<u32> = const { Cell::new(1) };
}

/// Helper: run a closure with a mutable reference to the pipeline for `id`.
fn with_pipeline<F, R>(id: u32, f: F) -> Result<R, JsValue>
where
    F: FnOnce(&mut ProcessingPipeline) -> Result<R, String>,
{
    PIPELINES.with(|p| {
        let mut map = p.borrow_mut();
        let pipeline = map
            .get_mut(&id)
            .ok_or_else(|| JsValue::from_str(&format!("Pipeline {} not found", id)))?;
        f(pipeline).map_err(|e| JsValue::from_str(&e))
    })
}

/// Helper: run a closure with an immutable reference to the pipeline for `id`.
fn with_pipeline_ref<F, R>(id: u32, f: F) -> Result<R, JsValue>
where
    F: FnOnce(&ProcessingPipeline) -> R,
{
    PIPELINES.with(|p| {
        let map = p.borrow();
        let pipeline = map
            .get(&id)
            .ok_or_else(|| JsValue::from_str(&format!("Pipeline {} not found", id)))?;
        Ok(f(pipeline))
    })
}

/// Helper: convert a JsValue to a Rust type via serde-wasm-bindgen.
fn from_js<T: serde::de::DeserializeOwned>(val: JsValue) -> Result<T, JsValue> {
    serde_wasm_bindgen::from_value(val).map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Helper: convert a Rust type to JsValue via serde-wasm-bindgen.
/// Uses serialize_maps_as_objects so HashMap becomes a plain JS object (not a Map).
fn to_js<T: serde::Serialize>(val: &T) -> Result<JsValue, JsValue> {
    let serializer = serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
    val.serialize(&serializer)
        .map_err(|e| JsValue::from_str(&e.to_string()))
}

// =====================================================================
// Pipeline lifecycle
// =====================================================================

/// Create a new isolated pipeline instance. Returns the pipeline ID.
///
/// Accepts an optional options object with:
/// - `debug` (bool): Enable WASM-side trace collection from creation.
#[wasm_bindgen]
pub fn pipeline_create(options: Option<JsValue>) -> u32 {
    let debug = options
        .and_then(|o| js_sys::Reflect::get(&o, &JsValue::from_str("debug")).ok())
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let id = NEXT_ID.with(|n| {
        let current = n.get();
        n.set(current + 1);
        current
    });
    PIPELINES.with(|p| {
        let mut pipeline = ProcessingPipeline::new();
        if debug {
            pipeline.set_debug(true);
        }
        p.borrow_mut().insert(id, pipeline);
    });
    id
}

/// Destroy a pipeline instance, freeing all its state.
#[wasm_bindgen]
pub fn pipeline_destroy(pipeline_id: u32) {
    PIPELINES.with(|p| {
        p.borrow_mut().remove(&pipeline_id);
    });
}

/// Initialize shadow state directly from a JS object (no JSON serialization).
#[wasm_bindgen]
pub fn shadow_init(pipeline_id: u32, state: JsValue) -> Result<(), JsValue> {
    let value: serde_json::Value = from_js(state)?;
    with_pipeline(pipeline_id, |p| p.shadow_init_value(value))
}

/// Register a BoolLogic expression. Returns logic_id for cleanup.
///
/// - `output_path`: full concern path, e.g. `_concerns.user.email.disabledWhen`
/// - `tree_json`: JSON string of the BoolLogic tree, e.g. `{"IS_EQUAL": ["user.role", "admin"]}`
#[wasm_bindgen]
pub fn register_boollogic(
    pipeline_id: u32,
    output_path: &str,
    tree_json: &str,
) -> Result<u32, JsValue> {
    with_pipeline(pipeline_id, |p| {
        p.register_boollogic(output_path, tree_json)
    })
}

/// Unregister a BoolLogic expression by its logic_id.
#[wasm_bindgen]
pub fn unregister_boollogic(pipeline_id: u32, logic_id: u32) -> Result<(), JsValue> {
    with_pipeline(pipeline_id, |p| {
        p.unregister_boollogic(logic_id);
        Ok(())
    })
}

/// Process a batch of state changes through the pipeline.
///
/// Diffs incoming changes against shadow state to filter no-ops before processing.
/// Runs: aggregation → sync → flip → BoolLogic → validator routing → listener routing.
/// Updates shadow state during processing (needed for BoolLogic evaluation).
/// Buffers BoolLogic concern results for later finalization.
///
/// Returns execution context for JS: state_changes, validators_to_run, execution_plan.
/// After JS executes listeners/validators, call pipeline_finalize() with their results.
///
/// Input: JS array of `{ path: "...", value_json: "..." }`
/// Output: JS object `{ state_changes: [...], validators_to_run: [...], execution_plan: {...}, has_work: bool }`
#[wasm_bindgen]
pub fn process_changes(pipeline_id: u32, changes: JsValue) -> Result<JsValue, JsValue> {
    let input: Vec<Change> = from_js(changes)?;
    with_pipeline(pipeline_id, |p| {
        let result = p.prepare_changes(input)?;
        to_js(&result).map_err(|e| e.as_string().unwrap_or_default())
    })
}

/// Finalize pipeline by merging JS-produced changes with buffered BoolLogic results.
///
/// Partitions js_changes by _concerns. prefix, merges with buffered concern changes,
/// diffs all changes against shadow state (filters no-ops), updates shadow for both
/// state and concern paths, returns final changes ready for valtio application.
///
/// Input: Single JS array of `{ path: "...", value_json: "..." }`
///   - Mix of listener-produced state changes + validator concern results (with _concerns. prefix)
/// Output: JS object `{ state_changes: [...], concern_changes: [...] }`
///   - concern_changes have _concerns. prefix stripped (paths relative to _concerns root)
///   - Both state_changes and concern_changes are diffed (no-ops filtered out)
#[wasm_bindgen]
pub fn pipeline_finalize(pipeline_id: u32, js_changes: JsValue) -> Result<JsValue, JsValue> {
    let changes: Vec<Change> = from_js(js_changes)?;
    with_pipeline(pipeline_id, |p| {
        let result = p.pipeline_finalize(changes)?;
        to_js(&result).map_err(|e| e.as_string().unwrap_or_default())
    })
}

// =====================================================================
// Consolidated Registration API
// =====================================================================

/// Register all side effects at once (sync, flip, aggregation, listeners).
///
/// Consolidates sync pairs, flip pairs, aggregations, and listeners into a single WASM call.
/// Computes initial changes from shadow state and returns listener IDs for cleanup tracking.
///
/// **Input JSON Format:**
/// ```json
/// {
///   "registration_id": "my-effects",
///   "sync_pairs": [["user.email", "profile.email"], ["user.name", "profile.name"]],
///   "flip_pairs": [["settings.darkMode", "settings.lightMode"]],
///   "aggregation_pairs": [["totals.sum", "items.0.price"], ["totals.sum", "items.1.price"]],
///   "listeners": [
///     {"subscriber_id": 100, "topic_path": "user", "scope_path": "user"},
///     {"subscriber_id": 101, "topic_path": "settings.darkMode", "scope_path": "settings"}
///   ]
/// }
/// ```
///
/// **Output JSON Format:**
/// ```json
/// {
///   "sync_changes": [
///     {"path": "profile.email", "value_json": "\"alice@example.com\""},
///     {"path": "profile.name", "value_json": "\"Alice\""}
///   ],
///   "aggregation_changes": [
///     {"path": "totals.sum", "value_json": "100"}
///   ],
///   "registered_listener_ids": [100, 101]
/// }
/// ```
///
/// **Example Behavior:**
/// - **sync_changes**: Computed from shadow state. If user.email="alice@test.com" and
///   profile.email is empty, returns change to set profile.email to match.
/// - **aggregation_changes**: Reads source paths from shadow state and computes initial
///   target value. If items exist, aggregates their values (e.g., sums prices).
/// - **registered_listener_ids**: Echo of the input subscriber_ids for cleanup tracking.
/// - **flip_pairs**: Registered silently, no changes returned (used for bidirectional toggling).
///
/// All registrations happen atomically in a single WASM call, reducing JS↔WASM boundary crossings.
#[wasm_bindgen]
pub fn register_side_effects(
    pipeline_id: u32,
    registration_json: &str,
) -> Result<JsValue, JsValue> {
    with_pipeline(pipeline_id, |p| {
        let result = p.register_side_effects(registration_json)?;
        to_js(&result).map_err(|e| e.as_string().unwrap_or_default())
    })
}

/// Unregister side effects by registration ID (placeholder).
///
/// Currently a no-op. In future, could track registrations for bulk cleanup.
#[wasm_bindgen]
pub fn unregister_side_effects(pipeline_id: u32, registration_id: &str) -> Result<(), JsValue> {
    with_pipeline(pipeline_id, |p| p.unregister_side_effects(registration_id))
}

/// Register all concerns at once (BoolLogic and validators).
///
/// Consolidates BoolLogic and validator registration into a single boundary crossing.
/// Returns registered logic IDs and validator IDs for cleanup.
///
/// Input: JSON string of `{ "registration_id": "...", "bool_logics": [...], "validators": [...] }`
/// Output: JS object `{ bool_logic_changes: [...], registered_logic_ids: [...], registered_validator_ids: [...] }`
#[wasm_bindgen]
pub fn register_concerns(pipeline_id: u32, registration_json: &str) -> Result<JsValue, JsValue> {
    with_pipeline(pipeline_id, |p| {
        let result = p.register_concerns(registration_json)?;
        to_js(&result).map_err(|e| e.as_string().unwrap_or_default())
    })
}

/// Unregister concerns by registration ID (placeholder).
///
/// Currently a no-op. In future, could track registrations for bulk cleanup.
#[wasm_bindgen]
pub fn unregister_concerns(pipeline_id: u32, registration_id: &str) -> Result<(), JsValue> {
    with_pipeline(pipeline_id, |p| p.unregister_concerns(registration_id))
}

/// Reset the entire pipeline to a fresh state (testing only).
///
/// Clears all internal state: shadow, registrations, graphs, router, BoolLogic registry.
/// Call this between tests to ensure isolation.
#[wasm_bindgen]
pub fn pipeline_reset(pipeline_id: u32) {
    with_pipeline(pipeline_id, |p| {
        p.reset();
        Ok(())
    })
    .ok();
}

/// Reset ALL pipelines and the ID counter (testing only).
#[wasm_bindgen]
pub fn pipeline_reset_all() {
    PIPELINES.with(|p| p.borrow_mut().clear());
    NEXT_ID.with(|n| n.set(1));
}

/// Return a snapshot of all registered graphs and registries for a pipeline.
///
/// Returns a JS object with sync_pairs, flip_pairs, listeners, bool_logics,
/// value_logics, aggregations, and computations fields.
#[wasm_bindgen]
pub fn get_graph_snapshot(pipeline_id: u32) -> Result<JsValue, JsValue> {
    with_pipeline(pipeline_id, |p| {
        to_js(&p.get_graph_snapshot()).map_err(|e| e.as_string().unwrap_or_default())
    })
}

/// Dump shadow state as JSON (debug/testing).
#[wasm_bindgen]
pub fn shadow_dump(pipeline_id: u32) -> Result<String, JsValue> {
    with_pipeline_ref(pipeline_id, |p| p.shadow_dump())
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- join_path ---

    #[test]
    fn join_path_basic() {
        assert_eq!(join_path("user", "email"), "user.email");
    }

    #[test]
    fn join_path_nested() {
        assert_eq!(join_path("a.b", "c.d"), "a.b.c.d");
    }

    #[test]
    fn join_path_single_char() {
        assert_eq!(join_path("a", "b"), "a.b");
    }

    #[test]
    fn join_path_empty_prefix() {
        assert_eq!(join_path("", "child"), ".child");
    }

    #[test]
    fn join_path_empty_suffix() {
        assert_eq!(join_path("parent", ""), "parent.");
    }

    #[test]
    fn join_path_capacity_is_exact() {
        let result = join_path("abc", "def");
        // "abc" (3) + '.' (1) + "def" (3) = 7
        assert_eq!(result.capacity(), 7);
    }

    // --- is_child_path ---

    #[test]
    fn is_child_path_direct_child() {
        assert!(is_child_path("user.email", "user"));
    }

    #[test]
    fn is_child_path_deep_child() {
        assert!(is_child_path("user.profile.email", "user"));
    }

    #[test]
    fn is_child_path_exact_match_is_not_child() {
        assert!(!is_child_path("user", "user"));
    }

    #[test]
    fn is_child_path_prefix_overlap_without_dot() {
        // "username" starts with "user" but is not a child of "user"
        assert!(!is_child_path("username", "user"));
    }

    #[test]
    fn is_child_path_shorter_path() {
        assert!(!is_child_path("u", "user"));
    }

    #[test]
    fn is_child_path_empty_parent() {
        assert!(is_child_path(".child", ""));
    }

    #[test]
    fn is_child_path_unrelated() {
        assert!(!is_child_path("settings.theme", "user"));
    }
}
