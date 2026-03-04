/// Crate-wide type aliases for fast hashing.
///
/// All keys are internal (path strings, u32 IDs) — no external-input DoS risk,
/// so we use ahash (non-cryptographic, ~30% faster than SipHash).
/// Boundary types that derive TS use `#[ts(type = "Record<string, ...>")]` overrides.
pub(crate) type HashMap<K, V> = ahash::AHashMap<K, V>;
pub(crate) type HashSet<T> = ahash::AHashSet<T>;
