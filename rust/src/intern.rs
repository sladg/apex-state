use std::collections::HashMap;

/// Bidirectional string interning table
///
/// Maps string paths to u32 IDs for efficient internal lookups.
/// All interning is WASM-internal — JS always passes string paths.
pub(crate) struct InternTable {
    string_to_id: HashMap<String, u32>,
    id_to_string: Vec<String>,
}

impl InternTable {
    pub(crate) fn new() -> Self {
        Self {
            string_to_id: HashMap::new(),
            id_to_string: Vec::new(),
        }
    }

    /// Intern a path string, returning its ID.
    /// Same path always returns same ID.
    pub(crate) fn intern(&mut self, path: &str) -> u32 {
        if let Some(&id) = self.string_to_id.get(path) {
            return id;
        }

        let id = self.id_to_string.len() as u32;
        self.id_to_string.push(path.to_owned());
        self.string_to_id.insert(path.to_owned(), id);
        id
    }

    /// Resolve a path ID back to its string.
    pub(crate) fn resolve(&self, id: u32) -> Option<&str> {
        self.id_to_string.get(id as usize).map(|s| s.as_str())
    }

    /// Number of unique interned strings.
    pub(crate) fn count(&self) -> u32 {
        self.id_to_string.len() as u32
    }

    /// Clear all interned strings. Invalidates all previously issued IDs.
    pub(crate) fn clear(&mut self) {
        self.string_to_id.clear();
        self.id_to_string.clear();
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
    fn empty_table() {
        let table = InternTable::new();
        assert_eq!(table.count(), 0);
        assert_eq!(table.resolve(0), None);
        assert_eq!(table.resolve(u32::MAX), None);
    }

    #[test]
    fn intern_and_resolve() {
        let mut table = InternTable::new();
        let id = table.intern("user.name");
        assert_eq!(id, 0);
        assert_eq!(table.resolve(id), Some("user.name"));
        assert_eq!(table.count(), 1);
    }

    #[test]
    fn deduplication() {
        let mut table = InternTable::new();
        let id1 = table.intern("user.name");
        let id2 = table.intern("user.name");
        assert_eq!(id1, id2);
        assert_eq!(table.count(), 1);
    }

    #[test]
    fn sequential_ids() {
        let mut table = InternTable::new();
        assert_eq!(table.intern("a"), 0);
        assert_eq!(table.intern("b"), 1);
        assert_eq!(table.intern("c"), 2);
        assert_eq!(table.count(), 3);
    }

    #[test]
    fn clear_resets() {
        let mut table = InternTable::new();
        let old_id = table.intern("path");
        assert_eq!(table.count(), 1);

        table.clear();
        assert_eq!(table.count(), 0);
        assert_eq!(table.resolve(old_id), None);

        // IDs restart from 0
        assert_eq!(table.intern("new"), 0);
    }

    #[test]
    fn empty_string() {
        let mut table = InternTable::new();
        let id = table.intern("");
        assert_eq!(table.resolve(id), Some(""));
    }

    #[test]
    fn case_sensitive() {
        let mut table = InternTable::new();
        let id1 = table.intern("User.Name");
        let id2 = table.intern("user.name");
        assert_ne!(id1, id2);
        assert_eq!(table.count(), 2);
    }

    #[test]
    fn stress_1000_paths() {
        let mut table = InternTable::new();
        for i in 0..1000 {
            let path = format!("path.{}", i);
            let id = table.intern(&path);
            assert_eq!(id, i as u32);
        }
        assert_eq!(table.count(), 1000);

        // Verify dedup still works
        let dup = table.intern("path.500");
        assert_eq!(dup, 500);
        assert_eq!(table.count(), 1000);

        // Verify resolve
        assert_eq!(table.resolve(0), Some("path.0"));
        assert_eq!(table.resolve(999), Some("path.999"));
        assert_eq!(table.resolve(1000), None);
    }

    #[test]
    fn default_trait() {
        let table: InternTable = Default::default();
        assert_eq!(table.count(), 0);
    }
}
