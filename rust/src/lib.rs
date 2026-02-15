use std::cell::RefCell;
use wasm_bindgen::prelude::*;

mod aggregation;
mod bool_logic;
mod diff;
mod graphs;
mod intern;
mod normalization;
mod pipeline;
mod rev_index;
mod router;
mod shadow;
mod validator;

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
fn to_js<T: serde::Serialize>(val: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(val).map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Reset the entire pipeline to a fresh state (testing only).
#[wasm_bindgen]
pub fn pipeline_reset() {
    PIPELINE.with(|p| {
        p.borrow_mut().reset();
    })
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

/// Register a batch of aggregations.
///
/// Input: JSON array of `{ "target": "...", "sources": [...] }`
/// Example: `[{ "target": "allUsers", "sources": ["user1", "user2", "user3"] }]`
#[wasm_bindgen]
pub fn register_aggregation_batch(aggregations_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_aggregation_batch(aggregations_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a batch of aggregations by target paths.
///
/// Input: JSON array of target paths
/// Example: `["allUsers", "summary.total"]`
#[wasm_bindgen]
pub fn unregister_aggregation_batch(targets_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_aggregation_batch(targets_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Register a batch of sync pairs.
///
/// Input: JSON array of path pairs that should stay synchronized
/// Example: `[["user.name", "profile.name"], ["user.email", "profile.email"]]`
#[wasm_bindgen]
pub fn register_sync_batch(pairs_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_sync_batch(pairs_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a batch of sync pairs.
///
/// Input: JSON array of path pairs to remove from sync
/// Example: `[["user.name", "profile.name"], ["user.email", "profile.email"]]`
#[wasm_bindgen]
pub fn unregister_sync_batch(pairs_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_sync_batch(pairs_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Register a batch of flip pairs.
///
/// Input: JSON array of path pairs that should stay inverted
/// Example: `[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]`
#[wasm_bindgen]
pub fn register_flip_batch(pairs_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_flip_batch(pairs_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a batch of flip pairs.
///
/// Input: JSON array of path pairs to remove from flip
/// Example: `[["checkbox1", "checkbox2"], ["toggle1", "toggle2"]]`
#[wasm_bindgen]
pub fn unregister_flip_batch(pairs_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_flip_batch(pairs_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Register a batch of validators.
///
/// Input: JSON array of `{ "validator_id": N, "output_path": "...", "dependency_paths": [...] }`
/// Example: `[{ "validator_id": 1, "output_path": "_concerns.user.email.validationState", "dependency_paths": ["user.email"] }]`
#[wasm_bindgen]
pub fn register_validators_batch(validators_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_validators_batch(validators_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a batch of validators by validator IDs.
///
/// Input: JSON array of validator IDs
/// Example: `[1, 2, 3]`
#[wasm_bindgen]
pub fn unregister_validators_batch(validator_ids_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_validators_batch(validator_ids_json)
            .map_err(|e| JsValue::from_str(&e))
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
            .process_changes_phase1(input)
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

/// Register a batch of listeners for topic-based dispatch.
///
/// Input: JSON array of `{ "subscriber_id": N, "topic_path": "...", "scope_path": "..." }`
#[wasm_bindgen]
pub fn register_listeners_batch(listeners_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .register_listeners_batch(listeners_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Unregister a batch of listeners by subscriber ID.
///
/// Input: JSON array of subscriber IDs, e.g. `[1, 2, 3]`
#[wasm_bindgen]
pub fn unregister_listeners_batch(subscriber_ids_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .unregister_listeners_batch(subscriber_ids_json)
            .map_err(|e| JsValue::from_str(&e))
    })
}

/// Create a dispatch plan for the given changes via serde-wasm-bindgen.
///
/// Input: JS array of `{ path: "...", value_json: "..." }`
/// Output: JS object `{ levels: [{ depth: N, dispatches: [...] }] }`
#[wasm_bindgen]
pub fn create_dispatch_plan(changes: JsValue) -> Result<JsValue, JsValue> {
    let input: Vec<Change> = from_js(changes)?;
    PIPELINE.with(|p| {
        let plan = p.borrow().create_dispatch_plan_vec(&input);
        to_js(&plan)
    })
}

/// Route produced changes from a depth level to downstream topics.
///
/// Input: depth level (u32) + JS array of produced changes
/// Output: JS DispatchPlan for downstream topics
#[wasm_bindgen]
pub fn route_produced_changes(depth: u32, produced_changes: JsValue) -> Result<JsValue, JsValue> {
    let input: Vec<Change> = from_js(produced_changes)?;
    PIPELINE.with(|p| {
        let plan = p.borrow().route_produced_changes_vec(depth, &input);
        to_js(&plan)
    })
}

/// Dump shadow state as JSON (debug/testing).
#[wasm_bindgen]
pub fn shadow_dump() -> String {
    PIPELINE.with(|p| p.borrow().shadow_dump())
}

/// Get a value from shadow state at a dot-separated path (debug/testing).
#[wasm_bindgen]
pub fn shadow_get(path: &str) -> Option<String> {
    PIPELINE.with(|p| p.borrow().shadow_get(path))
}

/// Number of interned paths (debug/testing).
#[wasm_bindgen]
pub fn intern_count() -> u32 {
    PIPELINE.with(|p| p.borrow().intern_count())
}
