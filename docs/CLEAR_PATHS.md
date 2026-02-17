# ClearPaths — Side-Effect Architecture & Implementation

## Overview

`clearPaths` is a new side-effect alongside sync, flip, and aggregations. It allows declarative "when X changes, clear Y" rules with wildcard support for dynamic hashmap keys.

**Test placeholders**: `tests/side-effects/clear-paths.test.ts` (19 vitest `it.todo()` tests)
**Rust test stubs**: `rust/src/clear_paths.rs` (35 `todo!()` unit tests)

## Public API

```typescript
type ClearPathRule<T> = [
  DeepKey<T>[],                    // triggers — paths that activate the rule
  DeepKey<T>[],                    // targets — paths to set to null
  { expandMatch?: boolean }?       // options (optional, defaults to false)
]
```

### `expandMatch` Flag

- **`false`** (default) — `[*]` is **correlated**: the key matched in the trigger is substituted into the target at the same positional `[*]`
- **`true`** — `[*]` in target **expands** to all keys at that level (independent of trigger match)

### Wildcard Semantics

- `[*]` in triggers: matches any single key, captures it positionally
- `[*]` in targets: substitutes the captured key from trigger (correlated)
- `[**]` in targets: expands to ALL keys at that object level in shadow state (independent)
- TypeScript only exposes `[*]` in `DeepKey<T>`. The `expandMatch: true` flag causes TS to rewrite target `[*]` → `[**]` before sending to WASM

### Examples

```typescript
clearPaths: [
  // Concrete — no wildcards
  [["form.email"], ["form.errors"]],

  // Correlated (default): changing email's value clears only email's error
  [["form.fields.[*].value"], ["form.fields.[*].error"]],

  // Expand: changing any field's value clears ALL field errors
  [["form.fields.[*].value"], ["form.fields.[*].error"], { expandMatch: true }],

  // Multiple triggers: any of these changing clears the targets
  [["form.email", "form.name"], ["form.errors", "form.touched"]],

  // Multi-level correlated
  [["form.[*].[*].value"], ["form.[*].[*].error"]],

  // Mixed: same section (correlated), all fields within it (expanded)
  // expandMatch applies to ALL [*] in targets, so use concrete+wildcard mix:
  [["form.[*].fields.[*].value"], ["form.[*].fields.[*].error"], { expandMatch: true }],
]
```

## TypeScript → WASM Boundary

`DeepKey<T>` only generates `[*]` variants (one per object level — manageable union size).

Before sending to WASM, TypeScript converts based on `expandMatch`:

```
expandMatch: false (default) → targets sent as-is with [*]
expandMatch: true            → targets rewritten: [*] → [**]
```

WASM receives two distinct tokens internally, no flag needed.

## WASM Internal Representation

### Path Segments

```rust
enum PathSegment {
    Literal(String),
    WildcardBind,    // [*]  — capture in trigger, substitute in target
    WildcardAll,     // [**] — expand to all keys from shadow state
}
```

### Clear Target

```rust
enum ClearTarget {
    /// No wildcards — interned path ID, O(1) resolve
    Direct(u32),
    /// Contains [*] or [**] — resolved at runtime against shadow + bindings
    Wildcard(Vec<PathSegment>),
}
```

### Registry — Dual Index

```rust
pub(crate) struct ClearPathsRegistry {
    /// Fast path: concrete trigger path_id → targets
    direct_triggers: HashMap<u32, Vec<ClearTarget>>,

    /// Slow path: wildcard trigger patterns, matched against each changed path
    wildcard_triggers: Vec<WildcardTrigger>,
}

struct WildcardTrigger {
    segments: Vec<PathSegment>,   // only Literal + WildcardBind
    targets: Vec<ClearTarget>,
}
```

Dual index ensures concrete triggers (the common case) are O(1) lookups. Wildcard triggers are scanned only when `wildcard_triggers` is non-empty.

## Pipeline Position: Step 3.5

Current pipeline order:

```
Step 0:   Diff pre-pass (eliminate no-ops)
Step 1-2: Aggregation writes (target → sources distribution)
Step 3:   Apply to shadow + mark affected logic
>>> Step 3.5: CLEAR PATHS (new) <<<
Step 4-5: Sync processing
Step 6-7: Flip processing
Step 7.5: Aggregation reads (sources → target recomputation)
Step 8-9: BoolLogic evaluation
Step 10:  Validator collection
Step 11:  Listener execution plan
```

### Why After Step 3 (shadow update)

Clear paths should only fire on **genuine** changes — the diff pre-pass (Step 0) and shadow comparison (Step 3) eliminate no-ops. We need confirmed changes before triggering clears.

### Why Before Sync/Flip (Steps 4-7)

Cleared paths produce new changes (value → null). These must propagate through:

- **Sync**: if a cleared path is synced, peers should also become null
- **Flip**: if a cleared boolean path is flipped, peer should become the inverse
- **Aggregation reads**: if a cleared path is an aggregation source, target recomputes
- **BoolLogic**: `EXISTS` on a cleared path should evaluate false

### No Self-Cascading

ClearPaths does NOT trigger other clearPaths rules. Only the original genuine changes from Step 3 feed into clear path processing. This prevents infinite loops.

Downstream side-effects (sync, flip, aggregation, BoolLogic) DO see cleared paths as changes.

## Processing Logic

### Step 3.5: process_clear_paths

```rust
fn process_clear_paths(&mut self, changed_path_ids: &[u32]) -> Vec<Change> {
    let mut clears = Vec::new();
    let mut seen = HashSet::new(); // deduplicate targets

    for &path_id in changed_path_ids {
        // --- Fast path: direct triggers ---
        if let Some(targets) = self.clear_registry.direct_triggers.get(&path_id) {
            self.resolve_and_clear(targets, &HashMap::new(), &mut clears, &mut seen);
        }

        // --- Slow path: wildcard triggers ---
        if !self.clear_registry.wildcard_triggers.is_empty() {
            let path = self.intern.resolve(path_id);
            for wt in &self.clear_registry.wildcard_triggers {
                if let Some(bindings) = match_trigger(&path, &wt.segments) {
                    self.resolve_and_clear(&wt.targets, &bindings, &mut clears, &mut seen);
                }
            }
        }
    }
    clears
}
```

### Trigger Matching (captures positional bindings)

```rust
fn match_trigger(path: &str, pattern: &[PathSegment]) -> Option<Vec<String>> {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.len() != pattern.len() { return None; }

    let mut captures = Vec::new();
    for (part, seg) in parts.iter().zip(pattern) {
        match seg {
            PathSegment::Literal(s) => { if part != s { return None; } }
            PathSegment::WildcardBind => { captures.push(part.to_string()); }
            PathSegment::WildcardAll => unreachable!("triggers never have [**]"),
        }
    }
    Some(captures)
}
```

### Target Resolution

```rust
fn resolve_and_clear(
    &mut self,
    targets: &[ClearTarget],
    captures: &Vec<String>,  // positional bindings from trigger [*] matches
    clears: &mut Vec<Change>,
    seen: &mut HashSet<u32>,
) {
    for target in targets {
        match target {
            ClearTarget::Direct(target_id) => {
                if seen.insert(*target_id) {
                    let path = self.intern.resolve(*target_id);
                    if !self.shadow.is_null(&path) {
                        self.shadow.set(&path, "null");
                        self.mark_affected_logic(&path);
                        clears.push(Change { path, value_json: "null".into() });
                    }
                }
            }
            ClearTarget::Wildcard(segments) => {
                let concrete_paths = resolve_wildcard_target(
                    &self.shadow, segments, captures
                );
                for path in concrete_paths {
                    let id = self.intern.intern(&path);
                    if seen.insert(id) && !self.shadow.is_null(&path) {
                        self.shadow.set(&path, "null");
                        self.mark_affected_logic(&path);
                        clears.push(Change { path, value_json: "null".into() });
                    }
                }
            }
        }
    }
}
```

### Wildcard Target Expansion

```rust
fn resolve_wildcard_target(
    shadow: &ShadowState,
    segments: &[PathSegment],
    captures: &[String],
) -> Vec<String> {
    // Walk segments left to right, building concrete paths:
    //
    // Literal("form")  → append "form"
    // WildcardBind      → substitute captures[capture_index++]
    // WildcardAll       → enumerate all Object keys from shadow at current prefix,
    //                     branch into multiple paths
    //
    // Returns all fully-resolved concrete paths.
}
```

`WildcardBind` consumes captures positionally (1st bind → 1st capture, 2nd → 2nd).
`WildcardAll` reads shadow state at the current prefix and branches for each key.

## Registration

### Via `register_side_effects` (consolidated API)

```typescript
wasm.registerSideEffects({
  registration_id: "my-effects",
  sync_pairs: [...],
  flip_pairs: [...],
  aggregation_pairs: [...],
  clear_paths: [
    { triggers: ["form.fields.[*].value"], targets: ["form.fields.[*].error"] },
    { triggers: ["form.email"], targets: ["form.errors", "form.touched"] },
  ],
  listeners: [...]
})
```

TypeScript performs the `[*]` → `[**]` rewrite on targets when `expandMatch: true` before serializing to WASM.

### Registration-Time Validation

| Rule | Error |
|------|-------|
| Target has `[*]` (WildcardBind) but trigger has fewer `[*]` | "Target has more positional wildcards than trigger" |
| Trigger has `[**]` (WildcardAll) | "Triggers cannot use expand-all wildcards" |
| Empty triggers or targets array | "ClearPaths rule must have at least one trigger and one target" |

## Clear Semantics

- **Clear = set to `null`** — path still exists in shadow tree, value becomes `ValueRepr::Null`
- **Subtree clearing** — if target is an object/array, the entire subtree is replaced with `null` (one change entry, simple and fast)
- **Diff check** — if target is already `null`, skip (no-op, no downstream propagation)

## Performance Characteristics

| Scenario | Runtime per changed path |
|----------|--------------------------|
| No clear rules registered | Zero overhead (empty registry check) |
| Concrete trigger → concrete target | O(1) lookup + O(1) resolve |
| Concrete trigger → wildcard target | O(1) lookup + O(keys at [**] level) |
| Wildcard trigger → concrete target | O(wildcard_trigger_count x path_segments) |
| Wildcard trigger → wildcard target | O(wildcard_trigger_count x path_segments x keys at [**] level) |

The `!wildcard_triggers.is_empty()` guard skips the slow path entirely when no wildcard triggers are registered.

---

## Implementation Steps

### Step 1: Rust — `rust/src/clear_paths.rs`

Create the module with the types and logic described above. Implement:

1. **`parse_segments(path: &str) -> Vec<PathSegment>`** — split by `.`, map `[*]` → WildcardBind, `[**]` → WildcardAll, else → Literal
2. **`match_trigger(path: &str, pattern: &[PathSegment]) -> Option<Vec<String>>`** — segment count must match, Literal must equal, WildcardBind captures. Returns positional captures or None
3. **`resolve_wildcard_target(shadow, segments, captures) -> Vec<String>`** — walk segments: Literal appends, WildcardBind substitutes from captures (positional), WildcardAll enumerates object keys from shadow at current prefix
4. **`register(triggers, targets, intern) -> Result`** — parse each path, route concrete triggers to `direct_triggers`, wildcard triggers to `wildcard_triggers`. Validate: no `[**]` in triggers, target `[*]` count ≤ trigger `[*]` count
5. **`unregister(registration_id)`** — remove all rules for a given registration
6. **`process(changed_path_ids, intern, shadow) -> Vec<Change>`** — the hot-path function called at Step 3.5

The `process` function:
- For each changed path ID: check `direct_triggers` (O(1)), then scan `wildcard_triggers` if non-empty
- Resolve each target (Direct → shadow null-check, Wildcard → resolve + null-check)
- Deduplicate via `HashSet<u32>` on target path IDs
- For each genuine clear: `shadow.set(path, "null")`, push `Change { path, value_json: "null" }`
- Return all clear changes

### Step 2: Rust — Wire into `rust/src/pipeline.rs`

1. Add `clear_registry: ClearPathsRegistry` to `ProcessingPipeline`
2. Add Step 3.5 in `prepare_changes`, after Step 3 (shadow update + mark_affected_logic), before Step 4 (sync):

```
// Step 3.5: Clear paths
let clear_changes = self.process_clear_paths(&changed_path_ids);
for change in &clear_changes {
    self.mark_affected_logic(&change.path);  // feed into BoolLogic + validators
}
// Include clear changes in inputs for sync/flip
// Extend buf_output with clear_changes
```

**Critical**: clear changes must be included in:
- `buf_output` (final state changes)
- Input to sync processing (Step 4-5)
- Input to flip processing (Step 6-7) — combine with aggregated + sync + clear changes
- `mark_affected_logic` calls (so BoolLogic and validators react)

**Critical**: clear changes must NOT cascade — only original Step 3 changed paths feed into `process_clear_paths`. Clear output does not re-enter clear processing.

### Step 3: Rust — Registration via `rust/src/lib.rs`

Add `clear_paths` to the `register_side_effects` JSON input struct:

```rust
struct SideEffectsInput {
    registration_id: String,
    sync_pairs: Vec<[String; 2]>,
    flip_pairs: Vec<[String; 2]>,
    aggregation_pairs: Vec<[String; 2]>,
    clear_paths: Vec<ClearPathInput>,  // NEW
    listeners: Vec<ListenerInput>,
}

struct ClearPathInput {
    triggers: Vec<String>,
    targets: Vec<String>,
}
```

Wire registration into `register_side_effects` function, after flip registration and before listener registration. Use `#[serde(default)]` on `clear_paths` for backward compatibility.

### Step 4: TypeScript — `src/wasm/bridge.ts`

Add `clear_paths` to the `registerSideEffects` payload construction. Transform from the public API format:

```typescript
// Public API: [triggers[], targets[], { expandMatch?: boolean }?]
// WASM API:   { triggers: string[], targets: string[] }

// When expandMatch is true, rewrite [*] → [**] in target paths
const transformClearPaths = (rules) => rules.map(([triggers, targets, opts]) => ({
  triggers,
  targets: opts?.expandMatch
    ? targets.map(t => t.replace(/\[\*\]/g, '[**]'))
    : targets,
}))
```

### Step 5: TypeScript — Types

In `src/types/side-effects.ts` (or wherever `SideEffects<T>` is defined), add:

```typescript
type ClearPathRule<T> = [
  DeepKey<T>[],
  DeepKey<T>[],
  { expandMatch?: boolean }?,
]

// Add to SideEffects<T>:
clearPaths?: ClearPathRule<T>[]
```

### Step 6: TypeScript — Registration wiring

In the registration file that calls `wasm.registerSideEffects`, include `clear_paths` in the payload.

### Step 7: Tests

1. Implement all 35 Rust unit tests in `rust/src/clear_paths.rs` (replace `todo!()` with actual test logic)
2. Implement all 19 vitest integration tests in `tests/side-effects/clear-paths.test.ts` (replace `it.todo()` with actual test logic)

---

## Files to Modify

### Rust (new + modified)

| File | Change |
|------|--------|
| `rust/src/clear_paths.rs` | **New** — `ClearPathsRegistry`, `ClearTarget`, `PathSegment`, matching + resolution logic |
| `rust/src/pipeline.rs` | Add `clear_registry: ClearPathsRegistry` to `ProcessingPipeline`, add Step 3.5 in `prepare_changes` |
| `rust/src/lib.rs` | Registration via `register_side_effects`, add clear_paths to the JSON input struct |

### TypeScript (modified)

| File | Change |
|------|--------|
| `src/wasm/bridge.ts` | Add `clear_paths` to `registerSideEffects` payload, `[*]` → `[**]` rewrite logic |
| `src/types/side-effects.ts` | Add `ClearPathRule<T>` type definition |
| Registration file | Wire clearPaths from config into `registerSideEffects` call |

### Tests

| File | Change |
|------|--------|
| `rust/src/clear_paths.rs` | 35 Rust unit tests — parsing, matching, resolution, pipeline integration |
| `tests/side-effects/clear-paths.test.ts` | 19 vitest tests — e2e concrete, correlated wildcards, expanded wildcards, pipeline interactions |

---

## Key Reference Files

| File | Why |
|------|-----|
| `rust/src/pipeline.rs` | Pipeline stages, understand where Step 3.5 goes |
| `rust/src/graphs.rs` | Reference for similar data structure patterns (Graph) |
| `rust/src/aggregation.rs` | Reference for similar registration/processing pattern |
| `rust/src/shadow.rs` | How `set(path, "null")` and `get(path)` work |
| `rust/src/intern.rs` | String interning (path ↔ u32 ID) |
| `rust/src/lib.rs` | WASM exports, `register_side_effects` JSON parsing |
| `src/wasm/bridge.ts` | JS → WASM boundary, how side effects are registered |

---

## Constraints

- **No allocations in hot paths** — `process_clear_paths` is called every pipeline tick. Pre-allocate buffers where possible, reuse `Vec` via `.clear()`, use interned IDs not strings for comparisons
- **No panics in exported functions** — use `Result<T, JsValue>`
- **Backward compatible** — `clear_paths` field in JSON uses `#[serde(default)]` so existing registrations without it still work
- **No self-cascading** — only original Step 3 changes trigger clears, clear output does NOT re-enter clear processing
- **Functional style in TypeScript** — arrow functions only, no classes
- **bridge.ts exports only `wasm`** — all WASM calls go through the single namespace

---

## Validation Commands

After Rust changes:
```bash
npm run wasm:fmt && npm run wasm:lint && npm run wasm:check
```

After TypeScript changes:
```bash
npm run code:fix && npm run code:check
```

Run Rust tests:
```bash
cd rust && cargo test
```

Run vitest:
```bash
npx vitest run tests/side-effects/clear-paths.test.ts
```

---

## Definition of Done

- [ ] `rust/src/clear_paths.rs` — module implemented with all types, parsing, matching, resolution, registration, processing
- [ ] `rust/src/pipeline.rs` — Step 3.5 integrated, clear changes feed into sync/flip/BoolLogic
- [ ] `rust/src/lib.rs` — `register_side_effects` accepts `clear_paths`
- [ ] `src/wasm/bridge.ts` — `clear_paths` included in registration payload with `expandMatch` → `[**]` rewrite
- [ ] `src/types/side-effects.ts` — `ClearPathRule<T>` type added
- [ ] All 35 Rust unit tests pass (`cargo test`)
- [ ] All 19 vitest integration tests pass
- [ ] `npm run wasm:fmt && npm run wasm:lint && npm run wasm:check` clean
- [ ] `npm run code:fix && npm run code:check` clean
- [ ] No changes to existing public API signatures
