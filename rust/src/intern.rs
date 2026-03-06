use lasso::{Key, Rodeo, Spur};

/// Bidirectional string interning table
///
/// Maps string paths to u32 IDs for efficient internal lookups.
/// All interning is WASM-internal — JS always passes string paths.
pub(crate) struct InternTable {
    rodeo: Rodeo<Spur, ahash::RandomState>,
}

impl InternTable {
    pub(crate) fn new() -> Self {
        Self {
            rodeo: Rodeo::with_hasher(ahash::RandomState::default()),
        }
    }

    /// Intern a path string, returning its ID.
    /// Same path always returns same ID.
    pub(crate) fn intern(&mut self, path: &str) -> u32 {
        self.rodeo.get_or_intern(path).into_usize() as u32
    }

    /// Resolve a path ID back to its string.
    pub(crate) fn resolve(&self, id: u32) -> Option<&str> {
        Spur::try_from_usize(id as usize).and_then(|s| self.rodeo.try_resolve(&s))
    }

    /// Get the ID for a path without interning (read-only lookup).
    /// Returns None if the path hasn't been interned yet.
    pub(crate) fn get_id(&self, path: &str) -> Option<u32> {
        self.rodeo.get(path).map(|s| s.into_usize() as u32)
    }

    /// Get the path for an ID (alias for resolve, for API consistency).
    pub(crate) fn get_path(&self, id: u32) -> Option<&str> {
        self.resolve(id)
    }

    /// Number of unique interned strings.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn count(&self) -> u32 {
        self.rodeo.len() as u32
    }

    /// Return all interned IDs whose paths are direct or transitive children of `prefix`.
    /// A path is a child of `prefix` if it starts with `prefix + "."`.
    #[allow(dead_code)] // Kept as general utility; hot-path callers use graph-scoped filters
    pub(crate) fn ids_with_prefix(&self, prefix: &str) -> Vec<u32> {
        let needle = format!("{}.", prefix);
        self.rodeo
            .iter()
            .filter(|(_, s)| s.starts_with(needle.as_str()))
            .map(|(k, _)| k.into_usize() as u32)
            .collect()
    }

    /// Clear all interned strings. Invalidates all previously issued IDs.
    #[allow(dead_code)] // Called via WASM export chain
    pub(crate) fn clear(&mut self) {
        self.rodeo = Rodeo::with_hasher(ahash::RandomState::default());
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
