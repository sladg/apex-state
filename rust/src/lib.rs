use std::cell::RefCell;
use wasm_bindgen::prelude::*;

mod bool_logic;
mod intern;
mod pipeline;
mod shadow;

use pipeline::ProcessingPipeline;

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

/// Reset the entire pipeline to a fresh state (testing only).
#[wasm_bindgen]
pub fn pipeline_reset() {
    PIPELINE.with(|p| {
        p.borrow_mut().reset();
    })
}

/// Initialize shadow state from a JSON string (full state snapshot).
#[wasm_bindgen]
pub fn shadow_init(state_json: &str) -> Result<(), JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .shadow_init(state_json)
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

/// Process a batch of state changes.
///
/// Input: JSON array of `{ "path": "...", "value_json": "..." }`
/// Output: JSON `{ "changes": [...] }` containing input changes + computed BoolLogic changes.
#[wasm_bindgen]
pub fn process_changes(changes_json: &str) -> Result<String, JsValue> {
    PIPELINE.with(|p| {
        p.borrow_mut()
            .process_changes(changes_json)
            .map_err(|e| JsValue::from_str(&e))
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
