use std::cell::RefCell;
use wasm_bindgen::prelude::*;

mod aggregation;
mod bool_logic;
mod diff;
mod functions;
mod graphs;
mod intern;
mod pipeline;
mod rev_index;
mod router;
mod shadow;
mod value_logic;

use pipeline::{Change, ProcessingPipeline};

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
    static PIPELINE: RefCell<ProcessingPipeline> = RefCell::new(ProcessingPipeline::new());
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

/// Initialize shadow state directly from a JS object (no JSON serialization).
#[wasm_bindgen]
pub fn shadow_init(state: JsValue) -> Result<(), JsValue> {
    let value: serde_json::Value = from_js(state)?;
    PIPELINE.with(|p| {
        p.borrow_mut()
            .shadow_init_value(value)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Register a BoolLogic expression. Returns logic_id for cleanup.
///
/// - `output_path`: full concern path, e.g. `_concerns.user.email.disabledWhen`
/// - `tree_json`: JSON string of the BoolLogic tree, e.g. `{"IS_EQUAL": ["user.role", "admin"]}`
#[wasm_bindgen]
pub fn register_boollogic(output_path: &str, tree_json: &str) -> Result<u32, JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_boollogic(output_path, tree_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a BoolLogic expression by its logic_id.
#[wasm_bindgen]
pub fn unregister_boollogic(logic_id: u32) {
    PIPELINE.with(|p| {
        p.borrow_mut().unregister_boollogic(logic_id);
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
pub fn process_changes(changes: JsValue) -> Result<JsValue, JsValue> {
    let input: Vec<Change> = from_js(changes)?;
    PIPELINE.with(|p| {
        let result = p
            .borrow_mut()
            .prepare_changes(input)
            .map_err(|e| JsValue::from_str(&e))?;
        to_js(&result)
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
pub fn pipeline_finalize(js_changes: JsValue) -> Result<JsValue, JsValue> {
    let changes: Vec<Change> = from_js(js_changes)?;
    PIPELINE.with(|p| {
        let result = p
            .borrow_mut()
            .pipeline_finalize(changes)
            .map_err(|e| JsValue::from_str(&e))?;
        to_js(&result)
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
pub fn register_side_effects(registration_json: &str) -> Result<JsValue, JsValue> {
    PIPELINE.with(|p| {
        let result = p
            .borrow_mut()
            .register_side_effects(registration_json)
            .map_err(|e| JsValue::from_str(&e))?;
        to_js(&result)
    })
}

/// Unregister side effects by registration ID (placeholder).
///
/// Currently a no-op. In future, could track registrations for bulk cleanup.
#[wasm_bindgen]
pub fn unregister_side_effects(registration_id: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_side_effects(registration_id)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Register all concerns at once (BoolLogic and validators).
///
/// Consolidates BoolLogic and validator registration into a single boundary crossing.
/// Returns registered logic IDs and validator IDs for cleanup.
///
/// Input: JSON string of `{ "registration_id": "...", "bool_logics": [...], "validators": [...] }`
/// Output: JS object `{ bool_logic_changes: [...], registered_logic_ids: [...], registered_validator_ids: [...] }`
#[wasm_bindgen]
pub fn register_concerns(registration_json: &str) -> Result<JsValue, JsValue> {
    PIPELINE.with(|p| {
        let result = p
            .borrow_mut()
            .register_concerns(registration_json)
            .map_err(|e| JsValue::from_str(&e))?;
        to_js(&result)
    })
}

/// Unregister concerns by registration ID (placeholder).
///
/// Currently a no-op. In future, could track registrations for bulk cleanup.
#[wasm_bindgen]
pub fn unregister_concerns(registration_id: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_concerns(registration_id)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Reset the entire pipeline to a fresh state (testing only).
///
/// Clears all internal state: shadow, registrations, graphs, router, BoolLogic registry.
/// Call this between tests to ensure isolation.
#[wasm_bindgen]
pub fn pipeline_reset() {
    PIPELINE.with(|p| p.borrow_mut().reset());
}

/// Dump shadow state as JSON (debug/testing).
#[wasm_bindgen]
pub fn shadow_dump() -> String {
    PIPELINE.with(|p| p.borrow().shadow_dump())
}
