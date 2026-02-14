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
    INTERN_TABLE.with(|table| table.borrow().resolve(id).map(|s| s.to_string()))
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
        paths
            .into_iter()
            .map(|path| borrowed.intern(path))
            .collect()
    })
}

// ============================================================================
// Internal Path Operation Utilities
// ============================================================================
//
// These utilities demonstrate how to perform common path operations using
// PathID instead of strings. They provide the foundation for efficient
// internal WASM operations while maintaining string-based external APIs.

/// Compare two paths by their PathID
///
/// This is more efficient than string comparison for internal operations.
/// O(1) numeric comparison instead of O(n) string comparison.
///
/// # Arguments
/// * `id1` - First PathID
/// * `id2` - Second PathID
///
/// # Returns
/// true if the PathIDs are equal (represent the same path)
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, paths_equal};
///
/// let id1 = intern_global("user.name".to_string());
/// let id2 = intern_global("user.name".to_string());
/// let id3 = intern_global("user.email".to_string());
///
/// assert!(paths_equal(id1, id2)); // Same path
/// assert!(!paths_equal(id1, id3)); // Different paths
/// ```
pub fn paths_equal(id1: PathID, id2: PathID) -> bool {
    id1 == id2
}

/// Check if one path starts with another path (prefix check)
///
/// Uses PathID for efficient internal operations, resolving to strings
/// only when necessary.
///
/// # Arguments
/// * `path_id` - The PathID to check
/// * `prefix_id` - The PathID of the potential prefix
///
/// # Returns
/// Some(true) if path starts with prefix, Some(false) if not, None if either ID is invalid
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, path_starts_with};
///
/// let path = intern_global("user.profile.name".to_string());
/// let prefix = intern_global("user.profile".to_string());
/// let other = intern_global("admin".to_string());
///
/// assert_eq!(path_starts_with(path, prefix), Some(true));
/// assert_eq!(path_starts_with(path, other), Some(false));
/// ```
pub fn path_starts_with(path_id: PathID, prefix_id: PathID) -> Option<bool> {
    // Fast path: if IDs are equal, path starts with itself
    if path_id == prefix_id {
        return Some(true);
    }

    INTERN_TABLE.with(|table| {
        let borrowed = table.borrow();
        let path = borrowed.resolve(path_id)?;
        let prefix = borrowed.resolve(prefix_id)?;
        Some(path.starts_with(prefix))
    })
}

/// Check if one path ends with another path (suffix check)
///
/// Uses PathID for efficient internal operations, resolving to strings
/// only when necessary.
///
/// # Arguments
/// * `path_id` - The PathID to check
/// * `suffix_id` - The PathID of the potential suffix
///
/// # Returns
/// Some(true) if path ends with suffix, Some(false) if not, None if either ID is invalid
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, path_ends_with};
///
/// let path = intern_global("user.profile.name".to_string());
/// let suffix = intern_global("profile.name".to_string());
/// let other = intern_global("email".to_string());
///
/// assert_eq!(path_ends_with(path, suffix), Some(true));
/// assert_eq!(path_ends_with(path, other), Some(false));
/// ```
pub fn path_ends_with(path_id: PathID, suffix_id: PathID) -> Option<bool> {
    // Fast path: if IDs are equal, path ends with itself
    if path_id == suffix_id {
        return Some(true);
    }

    INTERN_TABLE.with(|table| {
        let borrowed = table.borrow();
        let path = borrowed.resolve(path_id)?;
        let suffix = borrowed.resolve(suffix_id)?;
        Some(path.ends_with(suffix))
    })
}

/// Check if a path contains a substring
///
/// Uses PathID for efficient internal operations, resolving to strings
/// only when necessary.
///
/// # Arguments
/// * `path_id` - The PathID to check
/// * `substring_id` - The PathID of the substring to search for
///
/// # Returns
/// Some(true) if path contains the substring, Some(false) if not, None if either ID is invalid
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, path_contains};
///
/// let path = intern_global("user.profile.name".to_string());
/// let substring = intern_global("profile".to_string());
/// let other = intern_global("admin".to_string());
///
/// assert_eq!(path_contains(path, substring), Some(true));
/// assert_eq!(path_contains(path, other), Some(false));
/// ```
pub fn path_contains(path_id: PathID, substring_id: PathID) -> Option<bool> {
    // Fast path: if IDs are equal, path contains itself
    if path_id == substring_id {
        return Some(true);
    }

    INTERN_TABLE.with(|table| {
        let borrowed = table.borrow();
        let path = borrowed.resolve(path_id)?;
        let substring = borrowed.resolve(substring_id)?;
        Some(path.contains(substring))
    })
}

/// Get the length of a path string
///
/// Returns the length of the path without needing to resolve it to JavaScript.
/// Useful for internal WASM operations.
///
/// # Arguments
/// * `path_id` - The PathID to get the length of
///
/// # Returns
/// Some(length) if the ID is valid, None if invalid
///
/// # Example
/// ```
/// use apex_state_wasm::intern::{intern_global, path_length};
///
/// let path = intern_global("user.name".to_string());
/// assert_eq!(path_length(path), Some(9));
/// ```
pub fn path_length(path_id: PathID) -> Option<usize> {
    INTERN_TABLE.with(|table| {
        let borrowed = table.borrow();
        borrowed.resolve(path_id).map(|s| s.len())
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
    ///
    /// Initializes an empty table with no interned strings.
    /// The first interned string will receive PathID 0.
    ///
    /// # Example
    /// ```
    /// use apex_state_wasm::intern::InternTable;
    ///
    /// let table = InternTable::new();
    /// assert_eq!(table.count(), 0);
    /// ```
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
    ///
    /// Returns the total count of unique strings stored in this table.
    /// Duplicates are not counted - each unique string is counted once.
    ///
    /// # Returns
    /// The number of unique strings in the table
    ///
    /// # Example
    /// ```
    /// use apex_state_wasm::intern::InternTable;
    ///
    /// let mut table = InternTable::new();
    /// assert_eq!(table.count(), 0);
    ///
    /// table.intern("user.name".to_string());
    /// assert_eq!(table.count(), 1);
    ///
    /// table.intern("user.name".to_string()); // Duplicate
    /// assert_eq!(table.count(), 1); // Still 1
    ///
    /// table.intern("user.email".to_string());
    /// assert_eq!(table.count(), 2);
    /// ```
    pub fn count(&self) -> usize {
        self.id_to_string.len()
    }

    /// Clear all interned strings
    ///
    /// Removes all interned strings from the table and resets the ID counter.
    /// After clearing, the next interned string will receive PathID 0.
    ///
    /// **Warning**: This invalidates all previously issued PathIDs. Attempting
    /// to resolve old PathIDs after clearing will return None.
    ///
    /// Useful for testing or resetting state between operations.
    ///
    /// # Example
    /// ```
    /// use apex_state_wasm::intern::InternTable;
    ///
    /// let mut table = InternTable::new();
    /// let old_id = table.intern("user.name".to_string());
    /// assert_eq!(table.count(), 1);
    ///
    /// table.clear();
    /// assert_eq!(table.count(), 0);
    ///
    /// // Old ID is now invalid
    /// assert_eq!(table.resolve(old_id), None);
    ///
    /// // New string starts from ID 0 again
    /// let new_id = table.intern("new.path".to_string());
    /// assert_eq!(new_id, 0);
    /// ```
    pub fn clear(&mut self) {
        self.string_to_id.clear();
        self.id_to_string.clear();
        self.next_id = 0;
    }
}

impl Default for InternTable {
    /// Creates an empty InternTable (same as `InternTable::new()`)
    ///
    /// # Example
    /// ```
    /// use apex_state_wasm::intern::InternTable;
    ///
    /// let table: InternTable = Default::default();
    /// assert_eq!(table.count(), 0);
    /// ```
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
        let ids: Vec<PathID> = paths.iter().map(|p| intern_global(p.to_string())).collect();

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

    // Path operations tests
    #[test]
    fn test_path_operations_equality() {
        global_clear();

        let id1 = intern_global("user.name".to_string());
        let id2 = intern_global("user.name".to_string());
        let id3 = intern_global("user.email".to_string());

        // Same path IDs are equal
        assert!(paths_equal(id1, id2));

        // Different path IDs are not equal
        assert!(!paths_equal(id1, id3));

        global_clear();
    }

    #[test]
    fn test_path_operations_starts_with() {
        global_clear();

        let path = intern_global("user.profile.name".to_string());
        let prefix = intern_global("user.profile".to_string());
        let other = intern_global("admin".to_string());
        let self_path = intern_global("user.profile.name".to_string());

        // Path starts with prefix
        assert_eq!(path_starts_with(path, prefix), Some(true));

        // Path doesn't start with unrelated path
        assert_eq!(path_starts_with(path, other), Some(false));

        // Path starts with itself (fast path)
        assert_eq!(path_starts_with(path, self_path), Some(true));

        // Invalid ID returns None
        assert_eq!(path_starts_with(999, prefix), None);
        assert_eq!(path_starts_with(path, 999), None);

        global_clear();
    }

    #[test]
    fn test_path_operations_ends_with() {
        global_clear();

        let path = intern_global("user.profile.name".to_string());
        let suffix = intern_global("profile.name".to_string());
        let other = intern_global("email".to_string());
        let self_path = intern_global("user.profile.name".to_string());

        // Path ends with suffix
        assert_eq!(path_ends_with(path, suffix), Some(true));

        // Path doesn't end with unrelated path
        assert_eq!(path_ends_with(path, other), Some(false));

        // Path ends with itself (fast path)
        assert_eq!(path_ends_with(path, self_path), Some(true));

        // Invalid ID returns None
        assert_eq!(path_ends_with(999, suffix), None);
        assert_eq!(path_ends_with(path, 999), None);

        global_clear();
    }

    #[test]
    fn test_path_operations_contains() {
        global_clear();

        let path = intern_global("user.profile.name".to_string());
        let substring = intern_global("profile".to_string());
        let other = intern_global("admin".to_string());
        let self_path = intern_global("user.profile.name".to_string());

        // Path contains substring
        assert_eq!(path_contains(path, substring), Some(true));

        // Path doesn't contain unrelated string
        assert_eq!(path_contains(path, other), Some(false));

        // Path contains itself (fast path)
        assert_eq!(path_contains(path, self_path), Some(true));

        // Invalid ID returns None
        assert_eq!(path_contains(999, substring), None);
        assert_eq!(path_contains(path, 999), None);

        global_clear();
    }

    #[test]
    fn test_path_operations_length() {
        global_clear();

        let path1 = intern_global("user.name".to_string());
        let path2 = intern_global("a".to_string());
        let path3 = intern_global("".to_string());

        // Various path lengths
        assert_eq!(path_length(path1), Some(9)); // "user.name"
        assert_eq!(path_length(path2), Some(1)); // "a"
        assert_eq!(path_length(path3), Some(0)); // ""

        // Invalid ID returns None
        assert_eq!(path_length(999), None);

        global_clear();
    }

    #[test]
    fn test_path_operations_efficiency() {
        global_clear();

        // Intern the same paths multiple times
        let id1 = intern_global("user.profile.settings.theme".to_string());
        let id2 = intern_global("user.profile.settings.theme".to_string());
        let id3 = intern_global("user.profile".to_string());

        // PathID comparison is O(1) - just numeric comparison
        assert!(paths_equal(id1, id2));

        // Prefix check with PathID is efficient
        assert_eq!(path_starts_with(id1, id3), Some(true));

        // Verify that deduplication happened (same ID for same string)
        assert_eq!(id1, id2);

        global_clear();
    }

    // ============================================================================
    // Additional comprehensive tests for deduplication, bounds, and edge cases
    // ============================================================================

    // Deduplication tests
    #[test]
    fn test_deduplication_unicode_strings() {
        let mut table = InternTable::new();

        // Test unicode emoji deduplication
        let id1 = table.intern("user.🎨.theme".to_string());
        let id2 = table.intern("user.🎨.theme".to_string());
        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);

        // Test unicode characters from different languages
        let id3 = table.intern("用户.名称".to_string());
        let id4 = table.intern("用户.名称".to_string());
        assert_eq!(id3, id4);
        assert_eq!(table.count(), 2);

        // Verify resolution works correctly
        assert_eq!(table.resolve(id1), Some("user.🎨.theme"));
        assert_eq!(table.resolve(id3), Some("用户.名称"));
    }

    #[test]
    fn test_deduplication_special_characters() {
        let mut table = InternTable::new();

        // Test paths with special characters
        let id1 = table.intern("user.name[0]".to_string());
        let id2 = table.intern("user.name[0]".to_string());
        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);

        // Test paths with dots, slashes, and other special chars
        let id3 = table.intern("config/settings.json".to_string());
        let id4 = table.intern("config/settings.json".to_string());
        assert_eq!(id3, id4);
        assert_eq!(table.count(), 2);

        // Test paths with spaces
        let id5 = table.intern("user name".to_string());
        let id6 = table.intern("user name".to_string());
        assert_eq!(id5, id6);
        assert_eq!(table.count(), 3);
    }

    #[test]
    fn test_deduplication_very_long_strings() {
        let mut table = InternTable::new();

        // Create a very long path string
        let long_path = "a.".repeat(500) + "b";
        let id1 = table.intern(long_path.clone());
        let id2 = table.intern(long_path.clone());

        // Should deduplicate even for long strings
        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);
        assert_eq!(table.resolve(id1), Some(long_path.as_str()));
    }

    #[test]
    fn test_deduplication_whitespace_variations() {
        let mut table = InternTable::new();

        // Strings with different whitespace are treated as different
        let id1 = table.intern("user.name".to_string());
        let id2 = table.intern("user. name".to_string());
        let id3 = table.intern("user .name".to_string());

        // Each variation gets a unique ID
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);
        assert_eq!(table.count(), 3);

        // But duplicates of each variation are deduplicated
        let id1_dup = table.intern("user.name".to_string());
        let id2_dup = table.intern("user. name".to_string());
        assert_eq!(id1, id1_dup);
        assert_eq!(id2, id2_dup);
        assert_eq!(table.count(), 3);
    }

    // Bounds tests
    #[test]
    fn test_bounds_empty_table_resolve() {
        let table = InternTable::new();

        // Resolving any ID on empty table should return None
        assert_eq!(table.resolve(0), None);
        assert_eq!(table.resolve(1), None);
        assert_eq!(table.resolve(100), None);
        assert_eq!(table.resolve(u32::MAX), None);
    }

    #[test]
    fn test_bounds_valid_id_boundary() {
        let mut table = InternTable::new();

        // Intern a few strings
        let id0 = table.intern("path0".to_string());
        let id1 = table.intern("path1".to_string());
        let id2 = table.intern("path2".to_string());

        // Verify IDs are sequential
        assert_eq!(id0, 0);
        assert_eq!(id1, 1);
        assert_eq!(id2, 2);

        // Resolve valid IDs at boundaries
        assert_eq!(table.resolve(0), Some("path0"));
        assert_eq!(table.resolve(2), Some("path2"));

        // Resolve just beyond valid range
        assert_eq!(table.resolve(3), None);
        assert_eq!(table.resolve(100), None);
    }

    #[test]
    fn test_bounds_after_clear() {
        let mut table = InternTable::new();

        // Intern some strings
        let id1 = table.intern("path1".to_string());
        assert_eq!(table.resolve(id1), Some("path1"));

        // Clear the table
        table.clear();

        // Previously valid IDs are now invalid
        assert_eq!(table.resolve(id1), None);
        assert_eq!(table.resolve(0), None);

        // New strings start from ID 0 again
        let new_id = table.intern("new_path".to_string());
        assert_eq!(new_id, 0);
        assert_eq!(table.resolve(new_id), Some("new_path"));
    }

    #[test]
    fn test_bounds_large_id_values() {
        let table = InternTable::new();

        // Test with large ID values that are definitely out of bounds
        assert_eq!(table.resolve(1000), None);
        assert_eq!(table.resolve(1_000_000), None);
        assert_eq!(table.resolve(u32::MAX), None);
        assert_eq!(table.resolve(u32::MAX - 1), None);
    }

    // Edge case tests
    #[test]
    fn test_edge_case_empty_string() {
        let mut table = InternTable::new();

        // Empty strings should be interned normally
        let id1 = table.intern("".to_string());
        let id2 = table.intern("".to_string());

        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);
        assert_eq!(table.resolve(id1), Some(""));
    }

    #[test]
    fn test_edge_case_single_character() {
        let mut table = InternTable::new();

        let id = table.intern("a".to_string());
        assert_eq!(table.resolve(id), Some("a"));
        assert_eq!(table.count(), 1);

        // Single unicode character
        let id_unicode = table.intern("😀".to_string());
        assert_eq!(table.resolve(id_unicode), Some("😀"));
        assert_eq!(table.count(), 2);
    }

    #[test]
    fn test_edge_case_only_special_characters() {
        let mut table = InternTable::new();

        let id1 = table.intern("...".to_string());
        let id2 = table.intern("///".to_string());
        let id3 = table.intern("---".to_string());
        let id4 = table.intern("___".to_string());

        assert_eq!(table.count(), 4);
        assert_eq!(table.resolve(id1), Some("..."));
        assert_eq!(table.resolve(id2), Some("///"));
        assert_eq!(table.resolve(id3), Some("---"));
        assert_eq!(table.resolve(id4), Some("___"));
    }

    #[test]
    fn test_edge_case_whitespace_only() {
        let mut table = InternTable::new();

        // Various whitespace-only strings
        let id1 = table.intern(" ".to_string());
        let id2 = table.intern("  ".to_string());
        let id3 = table.intern("\t".to_string());
        let id4 = table.intern("\n".to_string());

        assert_eq!(table.count(), 4);
        assert_eq!(table.resolve(id1), Some(" "));
        assert_eq!(table.resolve(id2), Some("  "));
        assert_eq!(table.resolve(id3), Some("\t"));
        assert_eq!(table.resolve(id4), Some("\n"));
    }

    #[test]
    fn test_edge_case_mixed_unicode() {
        let mut table = InternTable::new();

        // Mix of ASCII, emoji, and multi-byte characters
        let mixed = "user.👤.profile.名前.settings.🎨";
        let id = table.intern(mixed.to_string());

        assert_eq!(table.resolve(id), Some(mixed));
        assert_eq!(table.count(), 1);

        // Verify deduplication works
        let id2 = table.intern(mixed.to_string());
        assert_eq!(id, id2);
        assert_eq!(table.count(), 1);
    }

    #[test]
    fn test_edge_case_similar_strings() {
        let mut table = InternTable::new();

        // Strings that differ by only one character
        let id1 = table.intern("user.name".to_string());
        let id2 = table.intern("user.names".to_string());
        let id3 = table.intern("user.nam".to_string());
        let id4 = table.intern("users.name".to_string());

        // Each should get a unique ID
        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id1, id4);
        assert_ne!(id2, id3);
        assert_ne!(id2, id4);
        assert_ne!(id3, id4);
        assert_eq!(table.count(), 4);

        // Verify each resolves correctly
        assert_eq!(table.resolve(id1), Some("user.name"));
        assert_eq!(table.resolve(id2), Some("user.names"));
        assert_eq!(table.resolve(id3), Some("user.nam"));
        assert_eq!(table.resolve(id4), Some("users.name"));
    }

    #[test]
    fn test_edge_case_paths_with_newlines() {
        let mut table = InternTable::new();

        let id1 = table.intern("line1\nline2".to_string());
        let id2 = table.intern("line1\r\nline2".to_string());

        // Different newline styles are different strings
        assert_ne!(id1, id2);
        assert_eq!(table.count(), 2);

        assert_eq!(table.resolve(id1), Some("line1\nline2"));
        assert_eq!(table.resolve(id2), Some("line1\r\nline2"));
    }

    #[test]
    fn test_stress_many_unique_strings() {
        let mut table = InternTable::new();

        // Intern many unique strings
        let count = 1000;
        let mut ids = Vec::new();

        for i in 0..count {
            let path = format!("path.{}", i);
            let id = table.intern(path);
            ids.push(id);
        }

        // Verify count
        assert_eq!(table.count(), count);

        // Verify all IDs are unique and sequential
        for (i, &id) in ids.iter().enumerate() {
            assert_eq!(id as usize, i);
        }

        // Verify all resolve correctly
        for (i, &id) in ids.iter().enumerate() {
            assert_eq!(table.resolve(id), Some(format!("path.{}", i).as_str()));
        }

        // Verify deduplication still works
        let dup_id = table.intern("path.500".to_string());
        assert_eq!(dup_id, ids[500]);
        assert_eq!(table.count(), count); // No new entry
    }

    #[test]
    fn test_stress_many_duplicate_strings() {
        let mut table = InternTable::new();

        // Intern the same string many times
        let path = "user.profile.name";
        let count = 1000;
        let mut ids = Vec::new();

        for _ in 0..count {
            let id = table.intern(path.to_string());
            ids.push(id);
        }

        // Should only have one entry despite many interns
        assert_eq!(table.count(), 1);

        // All IDs should be the same
        let first_id = ids[0];
        for &id in &ids {
            assert_eq!(id, first_id);
        }

        // Should resolve correctly
        assert_eq!(table.resolve(first_id), Some(path));
    }

    #[test]
    fn test_default_trait_implementation() {
        let table: InternTable = Default::default();
        assert_eq!(table.count(), 0);
    }

    #[test]
    fn test_interleaved_intern_and_resolve() {
        let mut table = InternTable::new();

        // Interleave intern and resolve operations
        let id1 = table.intern("path1".to_string());
        assert_eq!(table.resolve(id1), Some("path1"));

        let id2 = table.intern("path2".to_string());
        assert_eq!(table.resolve(id1), Some("path1"));
        assert_eq!(table.resolve(id2), Some("path2"));

        let id1_dup = table.intern("path1".to_string());
        assert_eq!(id1, id1_dup);
        assert_eq!(table.resolve(id1), Some("path1"));

        let id3 = table.intern("path3".to_string());
        assert_eq!(table.resolve(id1), Some("path1"));
        assert_eq!(table.resolve(id2), Some("path2"));
        assert_eq!(table.resolve(id3), Some("path3"));

        assert_eq!(table.count(), 3);
    }

    #[test]
    fn test_case_sensitivity() {
        let mut table = InternTable::new();

        // Different cases should be treated as different strings
        let id1 = table.intern("User.Name".to_string());
        let id2 = table.intern("user.name".to_string());
        let id3 = table.intern("USER.NAME".to_string());

        assert_ne!(id1, id2);
        assert_ne!(id1, id3);
        assert_ne!(id2, id3);
        assert_eq!(table.count(), 3);

        assert_eq!(table.resolve(id1), Some("User.Name"));
        assert_eq!(table.resolve(id2), Some("user.name"));
        assert_eq!(table.resolve(id3), Some("USER.NAME"));
    }
}
