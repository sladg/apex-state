use wasm_bindgen::prelude::*;

mod boollogic;
mod intern;
mod revdeps;

// Re-export interning functions
pub use intern::{intern, intern_batch, intern_clear, intern_count, resolve};

// Re-export BoolLogic functions
pub use boollogic::{evaluate_boollogic, extract_path_ids};

// Re-export reverse dependency index functions
pub use revdeps::{
    affected_by_change, clear_rev_deps, register_bool_logic, rev_deps_stats, unregister_bool_logic,
};

/// Trivial exported function to verify WASM integration works.
#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// Returns the WASM module version string.
#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
        assert_eq!(add(-1, 1), 0);
        assert_eq!(add(0, 0), 0);
    }

    #[test]
    fn test_version() {
        assert_eq!(version(), "0.1.0");
    }
}
