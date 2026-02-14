use wasm_bindgen::prelude::*;

// Module declarations
pub mod intern;

// Optional: Better panic messages in browser console
#[cfg(feature = "console_error_panic_hook")]
pub fn set_panic_hook() {
    console_error_panic_hook::set_once();
}

// Auto-execute on module load
#[wasm_bindgen(start)]
pub fn main() {
    #[cfg(feature = "console_error_panic_hook")]
    set_panic_hook();
}

// Export function to JavaScript
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

// Export intern function for path interning
#[wasm_bindgen]
pub fn intern(path: String) -> u32 {
    intern::intern_global(path)
}

// Export resolve function for path resolution
#[wasm_bindgen]
pub fn resolve(id: u32) -> String {
    intern::resolve_global(id).unwrap_or_else(|| String::new())
}

// Export batch_intern for efficient bulk interning
#[wasm_bindgen]
pub fn batch_intern(paths: Vec<String>) -> Vec<u32> {
    intern::batch_intern_global(paths)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_greet() {
        assert_eq!(greet("World"), "Hello, World!");
    }
}
