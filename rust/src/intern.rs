use std::cell::RefCell;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

/// Global string interning table
/// Maps unique u32 IDs to their corresponding path strings
thread_local! {
    static INTERN_TABLE: RefCell<InternTable> = RefCell::new(InternTable::new());
}

struct InternTable {
    /// ID → String mapping for debug/error messages
    id_to_string: HashMap<u32, String>,
    /// String → ID mapping for fast lookups
    string_to_id: HashMap<String, u32>,
    /// Next available ID
    next_id: u32,
}

impl InternTable {
    fn new() -> Self {
        Self {
            id_to_string: HashMap::new(),
            string_to_id: HashMap::new(),
            next_id: 0,
        }
    }

    fn intern(&mut self, path: &str) -> u32 {
        // Return existing ID if already interned
        if let Some(&id) = self.string_to_id.get(path) {
            return id;
        }

        // Allocate new ID
        let id = self.next_id;
        self.next_id += 1;

        // Store bidirectional mapping
        self.id_to_string.insert(id, path.to_string());
        self.string_to_id.insert(path.to_string(), id);

        id
    }

    fn resolve(&self, id: u32) -> Option<&str> {
        self.id_to_string.get(&id).map(|s| s.as_str())
    }

    fn clear(&mut self) {
        self.id_to_string.clear();
        self.string_to_id.clear();
        self.next_id = 0;
    }
}

/// Intern a single path string and return its ID
/// Same path always returns the same ID (deterministic)
#[wasm_bindgen]
pub fn intern(path: &str) -> u32 {
    INTERN_TABLE.with(|table| table.borrow_mut().intern(path))
}

/// Resolve an ID back to its original path string
/// Returns empty string if ID not found
#[wasm_bindgen]
pub fn resolve(id: u32) -> String {
    INTERN_TABLE.with(|table| {
        table
            .borrow()
            .resolve(id)
            .unwrap_or("")
            .to_string()
    })
}

/// Batch intern multiple paths for efficiency
/// Returns array of IDs in the same order as input paths
#[wasm_bindgen]
pub fn intern_batch(paths: Box<[JsValue]>) -> Vec<u32> {
    INTERN_TABLE.with(|table| {
        let mut table = table.borrow_mut();
        paths
            .iter()
            .filter_map(|v| v.as_string())
            .map(|path| table.intern(&path))
            .collect()
    })
}

/// Clear the entire interning table (useful for testing)
#[wasm_bindgen]
pub fn intern_clear() {
    INTERN_TABLE.with(|table| table.borrow_mut().clear())
}

/// Get the current number of interned strings
#[wasm_bindgen]
pub fn intern_count() -> u32 {
    INTERN_TABLE.with(|table| table.borrow().id_to_string.len() as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_deterministic() {
        intern_clear();

        let id1 = intern("user.name");
        let id2 = intern("user.email");
        let id3 = intern("user.name"); // Same path again

        assert_eq!(id1, id3, "Same path should return same ID");
        assert_ne!(id1, id2, "Different paths should return different IDs");
    }

    #[test]
    fn test_resolve() {
        intern_clear();

        let id = intern("product.price");
        let resolved = resolve(id);

        assert_eq!(resolved, "product.price");
    }

    #[test]
    fn test_resolve_unknown_id() {
        intern_clear();

        let resolved = resolve(9999);
        assert_eq!(resolved, "", "Unknown ID should return empty string");
    }

    #[test]
    fn test_round_trip() {
        intern_clear();

        let paths = vec!["a.b.c", "x.y.z", "foo", "bar.baz"];

        for path in &paths {
            let id = intern(path);
            let resolved = resolve(id);
            assert_eq!(resolved, *path, "Round-trip failed for {}", path);
        }
    }

    #[test]
    fn test_intern_clear() {
        intern_clear();

        let _id1 = intern("test.path");
        assert!(intern_count() > 0);

        intern_clear();
        assert_eq!(intern_count(), 0);

        let id2 = intern("test.path");
        assert_eq!(id2, 0, "IDs should reset after clear");
    }

    #[test]
    fn test_sequential_ids() {
        intern_clear();

        let id1 = intern("first");
        let id2 = intern("second");
        let id3 = intern("third");

        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(id3, 2);
    }
}
