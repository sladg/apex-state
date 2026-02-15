# WASM-EP1: Foundation & Toolchain (REVISED)

**Type**: Epic
**Priority**: P0
**Goal**: Validate WASM integration and implement BoolLogic evaluation with the complete `processChanges()` function.

**Key Architectural Decisions**:
- String paths at JS↔WASM boundary (path IDs are internal to WASM)
- No `serializeBoolLogic()` - direct `JSON.stringify(tree)`
- WASM is concern-agnostic - output paths include full path (`_concerns.user.email.disabledWhen`)
- Single entry point: `processChanges(changes) → changes`
- Shadow state is nested tree structure, not flat HashMap

---

## WASM-001: Rust toolchain setup ✅

**Status**: COMPLETE (2026-02-14)
**Type**: Story | **Points**: 3 | **Priority**: P0

See `tasks/WASM-006-COMPLETION.md` for details.

---

## WASM-002: String interning (INTERNAL)

**Type**: Story | **Points**: 2 | **Priority**: P0
**Depends on**: WASM-001

### Description

Implement string interning INTERNAL to WASM. Path IDs are used internally for efficient lookups (reverse deps, graph nodes), but **JS always passes string paths**.

### Key changes from original design

- ❌ NO JS-side `Map<string, number>` cache
- ❌ NO `intern_batch()` called from JS
- ✅ WASM interns paths automatically when received
- ✅ WASM resolves path IDs back to strings for output

### Interface

```rust
// Internal-only (not exposed to JS boundary)
fn intern(path: &str) -> u32  // Auto-called when receiving paths
fn resolve(id: u32) -> String  // Auto-called for output

// Debug/testing only (not used in production)
fn intern_count() -> u32
fn intern_clear()
```

### Acceptance criteria

- [ ] Interning is deterministic (same path → same ID)
- [ ] Paths are automatically interned when WASM receives them
- [ ] Path IDs resolved back to strings for output
- [ ] Performance: 1000 paths interned in < 1ms
- [ ] NO JS-side interning required

---

## WASM-003: Shadow state as nested tree

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-002

### Description

Implement shadow state as a **nested object tree** that mirrors the valtio state structure. NOT a flat `HashMap<path_id, ValueRepr>`.

### Data model

```rust
struct ShadowState {
    root: ValueRepr
}

enum ValueRepr {
    Object(HashMap<String, Box<ValueRepr>>),  // Nested objects
    Array(Vec<Box<ValueRepr>>),               // Arrays
    String(String),
    Number(f64),
    Bool(bool),
    Null,
}
```

### Key capabilities

- Deep path traversal: `"user.profile.email"` → `root["user"]["profile"]["email"]`
- Partial updates: Setting `"user.profile.email"` preserves `"user.profile.name"`
- Full subtree replacement: Setting `"user.profile"` replaces entire profile object
- Affected path calculation: When updating nested object, all descendant paths are affected

### Interface

```rust
fn shadow_init(state_json: &str) -> Result<(), JsValue>
// Input: Nested JSON object (valtio state structure)
// Builds nested tree + interns all discovered paths

fn shadow_get(path: &str) -> Option<String>  // Debug only
fn shadow_dump() -> String  // Debug only
```

### Acceptance criteria

- [ ] Nested structure mirrors valtio state
- [ ] Path traversal for reads/writes
- [ ] Handles leaf updates (single value)
- [ ] Handles nested object replacement (entire subtree)
- [ ] Calculates affected descendant paths correctly
- [ ] Performance: 1000 nested updates in < 100µs

---

## WASM-004: BoolLogic evaluator with tuple variants

**Type**: Story | **Points**: 4 | **Priority**: P0
**Depends on**: WASM-003

### Description

Implement BoolLogic evaluator that **accepts JS format directly** (no transformation). Uses serde with tuple variants to deserialize from JS array format.

### Data model

```rust
#[derive(Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum BoolLogicNode {
    IsEqual(String, ValueRepr),      // ("user.role", "admin")
    Exists(String),                  // "user.email"
    IsEmpty(String),
    And(Vec<BoolLogicNode>),
    Or(Vec<BoolLogicNode>),
    Not(Box<BoolLogicNode>),
    Gt(String, f64),                 // ("user.age", 18)
    Lt(String, f64),
    Gte(String, f64),
    Lte(String, f64),
    In(String, Vec<ValueRepr>),      // ("user.role", ["admin", "mod"])
}

impl BoolLogicNode {
    fn evaluate(&self, shadow: &ShadowState) -> bool {
        // Evaluate against nested shadow state
    }

    fn extract_paths(&self) -> Vec<String> {
        // Extract all input path strings
    }
}
```

### Key changes

- ❌ NO transformation in JS - just `JSON.stringify(tree)`
- ✅ Serde deserializes JS format directly
- ✅ Paths are strings, not path_ids (interned internally when needed)
- ✅ Evaluation traverses nested shadow state

### Acceptance criteria

- [ ] Deserializes from JS format: `{ "IS_EQUAL": ["user.role", "admin"] }`
- [ ] All operators work (IS_EQUAL, EXISTS, IS_EMPTY, AND, OR, NOT, GT, LT, GTE, LTE, IN)
- [ ] Evaluates against nested shadow state
- [ ] Extracts all input paths from tree
- [ ] Parity with JS evaluator
- [ ] Performance: 1000 evaluations in < 100µs

---

## WASM-005: Reverse dependency index

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-004

### Description

Build reverse dependency index mapping input paths to logic IDs. Used to find which BoolLogic expressions need re-evaluation when a path changes.

### Data model

```rust
struct ReverseDependencyIndex {
    // path_id → logic_ids (internal - uses interned path IDs)
    path_to_logic: HashMap<u32, HashSet<u32>>,
    logic_to_paths: HashMap<u32, HashSet<u32>>,
}

struct BoolLogicRegistry {
    // logic_id → metadata
    logics: HashMap<u32, BoolLogicMetadata>,
}

struct BoolLogicMetadata {
    output_path: String,      // "_concerns.user.email.disabledWhen"
    tree: BoolLogicNode,       // The expression
}
```

### Interface

```rust
fn register_boollogic(output_path: &str, tree_json: &str) -> Result<u32, JsValue>
// Parse tree, extract input paths, intern all paths, update reverse index
// Returns logic_id for cleanup

fn unregister_boollogic(logic_id: u32)
// Clean up reverse index

// Internal helper (not exposed)
fn affected_by_path(path: &str) -> Vec<u32>  // Returns affected logic IDs
```

### Acceptance criteria

- [ ] Registration extracts all input paths from tree
- [ ] Paths are interned automatically
- [ ] Reverse index maps input path → logic IDs
- [ ] Unregistration cleans up completely
- [ ] Multi-path BoolLogic (AND/OR) indexed correctly
- [ ] Performance: lookup in O(1) average case

---

## WASM-006: processChanges() - Phase 1

**Type**: Story | **Points**: 5 | **Priority**: P0
**Depends on**: WASM-005

### Description

Implement the complete `processChanges()` function - the single entry point for all state changes. Phase 1 includes shadow state updates and BoolLogic evaluation only.

### Interface

```rust
fn process_changes(changes_json: &str) -> Result<String, JsValue>
// Input: [{ "path": "user.role", "value_json": "\"admin\"" }]
// Output: { "changes": [
//   { "path": "user.role", "value_json": "\"admin\"" },  // Echo input
//   { "path": "_concerns.user.email.disabledWhen", "value_json": "true" }  // Computed
// ]}
```

### Algorithm

1. Parse input changes (path strings + values)
2. Intern paths (if not already interned)
3. Update nested shadow state via path traversal
4. Calculate affected input paths (including descendant paths for nested updates)
5. Find affected BoolLogic expressions via reverse index
6. Evaluate each affected expression against updated shadow state
7. Collect ALL changes (input + BoolLogic outputs)
8. Return flat list of changes (paths resolved to strings)

### Acceptance criteria

- [ ] Single function processes complete change batch
- [ ] Updates nested shadow state correctly
- [ ] Evaluates affected BoolLogic expressions
- [ ] Returns flat list: input + computed changes
- [ ] Output paths are strings (not IDs)
- [ ] Handles nested object updates (calculates affected descendants)
- [ ] Performance: 10 changes with 50 BoolLogic expressions in < 200µs

---

## WASM-007: JS bridge implementation

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-006

### Description

Implement thin JS bridge in `src/wasm/bridge.ts`. The bridge is minimal - just serialization/deserialization.

### Implementation

```typescript
// src/wasm/bridge.ts

export interface Change {
  path: string    // Full path including prefixes
  value: unknown
}

export const shadowInit = (state: Record<string, unknown>): void => {
  wasm.shadow_init(JSON.stringify(state))
}

export const registerBoolLogic = (
  outputPath: string,   // "_concerns.user.email.disabledWhen"
  tree: BoolLogic<any>
): number => {
  // NO transformation - direct JSON.stringify
  return wasm.register_boollogic(outputPath, JSON.stringify(tree))
}

export const processChanges = (changes: Change[]): Change[] => {
  const changesJson = JSON.stringify(
    changes.map(c => ({
      path: c.path,
      value_json: JSON.stringify(c.value)
    }))
  )

  const resultJson = wasm.process_changes(changesJson)
  const result = JSON.parse(resultJson)

  return result.changes.map((c: any) => ({
    path: c.path,
    value: JSON.parse(c.value_json)
  }))
}

export const applyChanges = (store: any, changes: Change[]): void => {
  for (const change of changes) {
    dot.set(store, change.path, change.value)
  }
}
```

### Acceptance criteria

- [ ] `shadowInit()` sends nested state to WASM (no return value)
- [ ] `registerBoolLogic()` uses full output path, no tree transformation
- [ ] `processChanges()` is the single entry point
- [ ] `applyChanges()` uses dot-notation for all paths
- [ ] NO path ID caching in JS
- [ ] NO serializeBoolLogic transformation
- [ ] Total bridge code < 100 lines

---

## WASM-008: Integration with useConcerns

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-007

### Description

Integrate WASM processChanges into the existing concerns system. Replace `effect()` wrapping for BoolLogic concerns.

### Changes to existing code

**`src/_internal/concerns/registration.ts`**:
- Detect BoolLogic concerns (have `condition` field)
- For BoolLogic: Call `registerBoolLogic()`, store logic_id, NO effect()
- For custom concerns: Keep existing `effect()` wrapping (no changes)

**Concern implementations** (disabledWhen, visibleWhen, etc.):
- Pass full output path: `_concerns.${path}.${concernName}`
- Pass condition directly (no transformation)

### Key files

- `src/_internal/concerns/registration.ts`
- `src/concerns/prebuilts/disabledWhen.ts`
- `src/concerns/prebuilts/visibleWhen.ts`
- `src/concerns/prebuilts/readonlyWhen.ts`

### Acceptance criteria

- [ ] BoolLogic concerns use WASM (no `effect()`)
- [ ] Custom concerns still use `effect()` (no regression)
- [ ] Cleanup calls `unregisterBoolLogic(logic_id)`
- [ ] All existing concern tests pass
- [ ] No public API changes

---

## WASM-009: Phase 1 integration tests

**Type**: Story | **Points**: 3 | **Priority**: P0
**Depends on**: WASM-008

### Description

End-to-end tests for the complete Phase 1 implementation.

### Test scenarios

1. **Single BoolLogic concern**
   - Register `disabledWhen` with `{ IS_EQUAL: ["user.role", "admin"] }`
   - Change `user.role` to `"admin"`
   - Verify `_concerns.user.email.disabledWhen` becomes `true`

2. **Multiple concerns on same field**
   - Register `disabledWhen`, `visibleWhen`, `readonlyWhen` on same field
   - Change dependency path
   - Verify all three evaluate correctly

3. **Nested BoolLogic (AND/OR/NOT)**
   - Complex logic with multiple operators
   - Verify evaluation matches JS evaluator

4. **Nested object update**
   - Update `user.profile` with new object
   - Verify concerns depending on `user.profile.email` are re-evaluated

5. **Registration/cleanup lifecycle**
   - Mount/unmount components
   - Verify no memory leaks

6. **Custom concerns still work**
   - Mix BoolLogic and custom concerns
   - Verify both work correctly

### Acceptance criteria

- [ ] All scenarios pass
- [ ] Tests run in Node (vitest)
- [ ] Performance: WASM path is faster than JS for > 10 fields
- [ ] No regressions in existing tests
- [ ] WASM module initialization is async-safe

---

## Summary

**Phase 1 Stories**:
- WASM-001: ✅ Toolchain (DONE)
- WASM-002: String interning (internal)
- WASM-003: Nested shadow state
- WASM-004: BoolLogic evaluator (tuple variants)
- WASM-005: Reverse dependency index
- WASM-006: processChanges() implementation
- WASM-007: JS bridge (minimal)
- WASM-008: useConcerns integration
- WASM-009: Integration tests

**Total Points**: 28 (was 26 in original, but more accurate scope)

**Key Simplifications**:
- No serializeBoolLogic (saves ~60 lines JS)
- No path ID cache in JS (saves state management)
- Single processChanges entry point (clearer API)
- Nested shadow state (matches reality)
- Concern-agnostic WASM (more flexible)
