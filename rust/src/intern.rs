use std::cell::RefCell;
use std::collections::HashMap;

/// Type alias for path identifiers
/// Using u32 to balance range (4B unique paths) with memory efficiency
pub type PathID = u32;

// Global interning table using thread-local storage
// WASM is single-threaded, so thread_local acts as a singleton
thread_local! {
    static INTERN_TABLE: RefCell<InternTable> = RefCell::new(InternTable::new());
}

/// Intern a string using the global interning table
///
/// This is the primary function for interning paths in WASM.
/// It uses a thread-local global table to ensure consistent
/// PathID assignments across all WASM calls.
///
/// # Arguments
/// * `path` - The string to intern
///
/// # Returns
/// The PathID for this string (existing or newly allocated)
///
/// # Example
/// ```
/// use apex_state_wasm::intern::intern_global;
///
/// let id1 = intern_global("user.name".to_string());
/// let id2 = intern_global("user.name".to_string());
/// assert_eq!(id1, id2); // Same ID returned for same string
/// ```
pub fn intern_global(path: String) -> PathID {
    INTERN_TABLE.with(|table| table.borrow_mut().intern(path))
}

/// Resolve a PathID using the global interning table
///
/// # Arguments
/// * `id` - The PathID to resolve
///
/// # Returns
/// Some(String) if the ID is valid, None if out of bounds
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, resolve_global};
///
/// let id = intern_global("user.name".to_string());
/// let path = resolve_global(id);
/// assert_eq!(path, Some("user.name".to_string()));
/// ```
pub fn resolve_global(id: PathID) -> Option<String> {
    INTERN_TABLE.with(|table| {
        table.borrow().resolve(id).map(|s| s.to_string())
    })
}

/// Get the number of interned strings in the global table
///
/// Useful for debugging and testing
pub fn global_count() -> usize {
    INTERN_TABLE.with(|table| table.borrow().count())
}

/// Clear the global interning table
///
/// Useful for testing or resetting state between WASM calls
pub fn global_clear() {
    INTERN_TABLE.with(|table| table.borrow_mut().clear())
}

/// Batch intern multiple strings using the global interning table
///
/// More efficient than calling intern_global repeatedly as it
/// only borrows the global table once.
///
/// # Arguments
/// * `paths` - Vector of strings to intern
///
/// # Returns
/// Vector of PathIDs corresponding to each input string
///
/// # Example
/// ```
/// use apex_state_wasm::intern::batch_intern_global;
///
/// let paths = vec![
///     "user.name".to_string(),
///     "user.email".to_string(),
///     "user.name".to_string(), // Duplicate
/// ];
/// let ids = batch_intern_global(paths);
/// assert_eq!(ids.len(), 3);
/// assert_eq!(ids[0], ids[2]); // Same ID for duplicates
/// ```
pub fn batch_intern_global(paths: Vec<String>) -> Vec<PathID> {
    INTERN_TABLE.with(|table| {
        let mut borrowed = table.borrow_mut();
        paths.into_iter().map(|path| borrowed.intern(path)).collect()
    })
}

/// Bidirectional string interning table for efficient path operations
///
/// Maintains both forward (string → ID) and reverse (ID → string) lookups
/// to enable fast conversions at the WASM/JS boundary while using numeric
/// IDs for internal operations.
///
/// # Example
/// ```
/// use apex_state_wasm::intern::InternTable;
///
/// let mut table = InternTable::new();
/// let id1 = table.intern("user.name".to_string());
/// let id2 = table.intern("user.name".to_string());
/// assert_eq!(id1, id2); // Same string returns same ID
/// assert_eq!(table.resolve(id1).unwrap(), "user.name");
/// ```
pub struct InternTable {
    /// Forward lookup: string → PathID
    /// Enables deduplication - same string always returns same ID
    string_to_id: HashMap<String, PathID>,

    /// Reverse lookup: PathID → String
    /// Vec index corresponds to PathID value for O(1) lookup
    id_to_string: Vec<String>,

    /// Next available PathID
    /// Increments with each new unique string
    next_id: PathID,
}

impl InternTable {
    /// Create a new empty interning table
    pub fn new() -> Self {
        Self {
            string_to_id: HashMap::new(),
            id_to_string: Vec::new(),
            next_id: 0,
        }
    }

    /// Intern a string, returning its PathID
    ///
    /// If the string has been interned before, returns the existing ID.
    /// Otherwise, allocates a new ID and stores the string.
    ///
    /// # Arguments
    /// * `path` - The string to intern
    ///
    /// # Returns
    /// The PathID for this string (existing or newly allocated)
    pub fn intern(&mut self, path: String) -> PathID {
        // Check if already interned
        if let Some(&id) = self.string_to_id.get(&path) {
            return id;
        }

        // Allocate new ID
        let id = self.next_id;
        self.next_id += 1;

        // Store bidirectional mapping
        self.id_to_string.push(path.clone());
        self.string_to_id.insert(path, id);

        id
    }

    /// Resolve a PathID back to its original string
    ///
    /// # Arguments
    /// * `id` - The PathID to resolve
    ///
    /// # Returns
    /// Some(&str) if the ID is valid, None if out of bounds
    pub fn resolve(&self, id: PathID) -> Option<&str> {
        self.id_to_string.get(id as usize).map(|s| s.as_str())
    }

    /// Get the number of unique interned strings
    pub fn count(&self) -> usize {
        self.id_to_string.len()
    }

    /// Clear all interned strings
    ///
    /// Useful for testing or resetting state
    pub fn clear(&mut self) {
        self.string_to_id.clear();
        self.id_to_string.clear();
        self.next_id = 0;
    }
}

impl Default for InternTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_table_is_empty() {
        let table = InternTable::new();
        assert_eq!(table.count(), 0);
    }

    #[test]
    fn test_intern_creates_new_id() {
        let mut table = InternTable::new();
        let id = table.intern("user.name".to_string());
        assert_eq!(id, 0);
        assert_eq!(table.count(), 1);
    }

    #[test]
    fn test_intern_deduplication() {
        let mut table = InternTable::new();
        let id1 = table.intern("user.name".to_string());
        let id2 = table.intern("user.name".to_string());
        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);
    }

    #[test]
    fn test_resolve_roundtrip() {
        let mut table = InternTable::new();
        let original = "user.profile.email";
        let id = table.intern(original.to_string());
        let resolved = table.resolve(id);
        assert_eq!(resolved, Some(original));
    }

    #[test]
    fn test_resolve_invalid_id() {
        let table = InternTable::new();
        assert_eq!(table.resolve(999), None);
    }

    #[test]
    fn test_multiple_unique_strings() {
        let mut table = InternTable::new();
        let id1 = table.intern("user.name".to_string());
        let id2 = table.intern("user.email".to_string());
        let id3 = table.intern("user.age".to_string());

        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(id3, 2);
        assert_eq!(table.count(), 3);

        assert_eq!(table.resolve(id1), Some("user.name"));
        assert_eq!(table.resolve(id2), Some("user.email"));
        assert_eq!(table.resolve(id3), Some("user.age"));
    }

    #[test]
    fn test_clear() {
        let mut table = InternTable::new();
        table.intern("user.name".to_string());
        table.intern("user.email".to_string());
        assert_eq!(table.count(), 2);

        table.clear();
        assert_eq!(table.count(), 0);

        // After clear, can intern again starting from ID 0
        let id = table.intern("new.path".to_string());
        assert_eq!(id, 0);
    }

    #[test]
    fn test_empty_string() {
        let mut table = InternTable::new();
        let id = table.intern("".to_string());
        assert_eq!(table.resolve(id), Some(""));
    }

    // Global interning table tests
    #[test]
    fn test_global_intern_table() {
        // Clear any previous state
        global_clear();

        // Test basic interning
        let id1 = intern_global("user.name".to_string());
        assert_eq!(id1, 0);
        assert_eq!(global_count(), 1);

        // Test deduplication
        let id2 = intern_global("user.name".to_string());
        assert_eq!(id1, id2);
        assert_eq!(global_count(), 1);

        // Test multiple unique strings
        let id3 = intern_global("user.email".to_string());
        assert_eq!(id3, 1);
        assert_eq!(global_count(), 2);

        // Test resolution
        assert_eq!(resolve_global(id1), Some("user.name".to_string()));
        assert_eq!(resolve_global(id3), Some("user.email".to_string()));
        assert_eq!(resolve_global(999), None);

        // Test clear
        global_clear();
        assert_eq!(global_count(), 0);
    }

    #[test]
    fn test_global_intern_table_persistence() {
        // Clear any previous state
        global_clear();

        // Intern in one "call"
        let id1 = intern_global("path1".to_string());

        // Intern in another "call" - should persist
        let id2 = intern_global("path2".to_string());
        let id1_again = intern_global("path1".to_string());

        // Verify persistence across calls
        assert_eq!(id1, id1_again);
        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        assert_eq!(global_count(), 2);

        // Clean up
        global_clear();
    }

    #[test]
    fn test_global_intern_table_resolve_all() {
        // Clear any previous state
        global_clear();

        // Intern multiple paths
        let paths = vec!["a.b.c", "x.y.z", "foo.bar"];
        let ids: Vec<PathID> = paths
            .iter()
            .map(|p| intern_global(p.to_string()))
            .collect();

        // Verify all resolve correctly
        for (i, path) in paths.iter().enumerate() {
            assert_eq!(resolve_global(ids[i]), Some(path.to_string()));
        }

        // Clean up
        global_clear();
    }

    #[test]
    fn test_batch_intern_global() {
        // Clear any previous state
        global_clear();

        // Batch intern multiple paths
        let paths = vec![
            "user.name".to_string(),
            "user.email".to_string(),
            "user.age".to_string(),
        ];
        let ids = batch_intern_global(paths);

        assert_eq!(ids.len(), 3);
        assert_eq!(ids[0], 0);
        assert_eq!(ids[1], 1);
        assert_eq!(ids[2], 2);
        assert_eq!(global_count(), 3);

        // Clean up
        global_clear();
    }

    #[test]
    fn test_batch_intern_global_with_duplicates() {
        // Clear any previous state
        global_clear();

        // Batch intern with duplicates
        let paths = vec![
            "user.name".to_string(),
            "user.email".to_string(),
            "user.name".to_string(), // Duplicate
        ];
        let ids = batch_intern_global(paths);

        assert_eq!(ids.len(), 3);
        assert_eq!(ids[0], ids[2]); // Same ID for duplicates
        assert_eq!(ids[0], 0);
        assert_eq!(ids[1], 1);
        assert_eq!(global_count(), 2); // Only 2 unique strings

        // Clean up
        global_clear();
    }

    #[test]
    fn test_batch_intern_global_empty() {
        // Clear any previous state
        global_clear();

        // Batch intern empty vector
        let paths: Vec<String> = vec![];
        let ids = batch_intern_global(paths);

        assert_eq!(ids.len(), 0);
        assert_eq!(global_count(), 0);

        // Clean up
        global_clear();
    }

    #[test]
    fn test_batch_intern_global_resolve() {
        // Clear any previous state
        global_clear();

        // Batch intern and verify resolution
        let paths = vec![
            "a.b.c".to_string(),
            "x.y.z".to_string(),
            "foo.bar".to_string(),
        ];
        let paths_clone = paths.clone();
        let ids = batch_intern_global(paths);

        // Verify all resolve correctly
        for (i, expected) in paths_clone.iter().enumerate() {
            assert_eq!(resolve_global(ids[i]), Some(expected.clone()));
        }

        // Clean up
        global_clear();
    }
}
