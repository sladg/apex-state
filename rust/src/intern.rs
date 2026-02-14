use std::collections::HashMap;

/// Type alias for path identifiers
/// Using u32 to balance range (4B unique paths) with memory efficiency
pub type PathID = u32;

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
}
