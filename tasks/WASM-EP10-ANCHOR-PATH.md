# WASM-EP10: Listener Multi-Path + anchorPath

**Epic Key**: WASM-EP10
**Status**: ⏳ Ready
**Depends On**: —
**Total Points**: 5pts

Two related improvements to registration expressiveness:

1. **Multi-path listeners** — `path: DeepKey<DATA>[]` lets a single listener watch multiple paths
   and fire once per run when any of them change.
2. **`anchorPath`** — an optional guard path on any registration; all resources in the group are
   silently skipped when the anchor is structurally absent from shadow state.

---

## Feature 1: Multi-Path Listeners (`path: DeepKey<DATA>[]`)

### Problem

A listener today accepts exactly one `path` (or `null` for all changes). Watching multiple
unrelated paths requires registering separate listeners, each with its own handler function and
subscriber ID, even when the logical intent is one reaction to any of several paths.

### Solution

Allow `path` to be an array of paths. The listener fires **once per pipeline run** when any of
the listed paths has a change, receiving all matching changes merged into a single input array.

`scope` remains independent — it controls what state snapshot is passed to the handler, not what
triggers it.

### Semantics

| `path` value | Trigger condition |
|---|---|
| `null` | Any change in the store |
| `'user.email'` | Change at or under `user.email` (existing) |
| `['user.email', 'user.name']` | Change at or under **either** path |

**Deduplication**: if both `user.email` and `user.name` change in the same batch, the listener
fires **once**, receiving both changes in its input array. It never fires more than once per
`processChanges` call regardless of how many paths in the array matched.

**Scope is independent**: `path: ['user.email', 'user.name'], scope: null` fires on either
change and receives the full state object. `scope: 'user'` fires on either and receives
`state.user`.

### Architecture

**Registration**: each path in the array is registered as a separate topic in the router, all
pointing to the same `subscriber_id`. The router already maps `topic_path → [subscriber_ids]`
— no structural change needed, just multiple insertions.

**Execution plan deduplication**: the plan builder collects matched dispatches by
`subscriber_id`, merging `input_change_ids` from all matched topics before emitting a single
`DispatchEntry` per subscriber per run.

```
path: ['user.email', 'user.name']
  → router.register(subscriber_id=5, topic='user.email')
  → router.register(subscriber_id=5, topic='user.name')

processChanges([user.email changed, user.name changed]):
  router finds subscriber 5 via 'user.email' → input_change_ids: [0]
  router finds subscriber 5 via 'user.name'  → input_change_ids: [1]
  plan builder: subscriber 5 appears twice → merge → one DispatchEntry, input_change_ids: [0, 1]
  handler fires once with changes: [['email', ..., {}], ['name', ..., {}]]
```

### WASM Registration Format

`topic_path: String` (single) changes to `topic_paths: Vec<String>` (always an array). Single
paths are normalized to a one-element vec on the TypeScript side before crossing the boundary.

```rust
// ListenerRegistration in pipeline.rs
pub topic_paths: Vec<String>,   // replaces topic_path: String
pub scope_path: String,
```

TypeScript normalizes before sending:

```typescript
const topicPaths = Array.isArray(listener.path)
  ? listener.path.map((p) => p as string)
  : [(listener.path ?? '') as string]

// → { subscriber_id, topic_paths: topicPaths, scope_path }
```

### Public API

```typescript
useSideEffects({
  listeners: [
    {
      // Single path — unchanged
      path: 'user.email',
      fn: (changes, state) => { ... },
    },
    {
      // Multi-path — new
      path: ['user.email', 'user.name'],
      scope: null,   // full state; scope is still independent
      fn: (changes, state) => {
        // changes may contain entries for both paths if both changed in same batch
        // fires exactly once regardless
      },
    },
  ],
})
```

### Implementation Plan — Multi-Path

**Rust (`rust/src/pipeline.rs`)**:

- `ListenerRegistration`: `topic_path: String` → `topic_paths: Vec<String>`
- `register_side_effects`: for each listener, iterate `topic_paths`, call
  `router.register(subscriber_id, topic)` once per path
- Router plan builder (`router.rs`): deduplicate `subscriber_id` across matched topics;
  merge `input_change_ids` into a single `DispatchEntry` per subscriber

**TypeScript**:

- `ListenerRegistration<DATA>`: `path: DeepKey<DATA> | DeepKey<DATA>[] | null`
- `registerSideEffects`: normalize `path` to array before serializing to WASM
- Bridge type: `topic_paths: string[]` on the listener input object

### Files Touched — Multi-Path

| File | Change |
|---|---|
| `rust/src/pipeline.rs` | `ListenerRegistration.topic_paths: Vec<String>`; iterate in `register_side_effects` |
| `rust/src/router.rs` | Dedup by `subscriber_id` in plan builder; merge `input_change_ids` |
| `src/core/types.ts` | `path: DeepKey<DATA> \| DeepKey<DATA>[] \| null` on `ListenerRegistration` |
| `src/sideEffects/registration.wasm-impl.ts` | Normalize path to array before sending |
| `src/wasm/bridge.ts` | `topic_paths: string[]` on listener input type |

---

## Feature 2: `anchorPath` — Conditional Registration Skipping

### Problem

Registrations (side effects and concerns) are tied to React's lifecycle. If the data they depend on
**structurally disappears** mid-session — e.g. `state.user.profile` is removed because the user
navigates away, or a dynamic section collapses — the registration stays fully active until React
unmounts the owning component. During that gap:

- BoolLogic expressions re-evaluate against absent paths → wrong results applied to `_concerns`
- Validators run against absent data → stale validation state written
- Listeners fire with changes to paths that no longer exist → user handler receives garbage input

React is async. The gap can span multiple `processChanges` calls.

---

### Solution: `anchorPath`

An optional `anchorPath` field on any registration. When set, WASM checks that path in shadow
state before each pipeline run. If the path is absent (`None`), **all resources in that
registration are silently skipped** for that run. No unregistration, no cleanup — the
registration is dormant and resumes as soon as the anchor is present again.

React cleanup still removes the registration when the component unmounts, as normal.

---

### Semantics

| Anchor state | Behaviour |
|---|---|
| Path present in shadow (`Some`) | All resources fire normally |
| Path absent in shadow (`None`) | All resources are skipped for this pipeline run |
| No `anchorPath` set | No change — resource always runs |

**"Absent" is structural**: `shadow.get(path)` returns `None`. This catches:

- Parent object replaced: `state.user = {}` (removing `user.profile`)
- Key explicitly deleted

Setting a path to `null` or `undefined` is **not** treated as absent — those are values, not
structural removals. If you want `null` to act as an anchor, use a BoolLogic `EXISTS` check instead.

---

### Architecture

#### Anchor State Map (per pipeline run)

The pipeline maintains two fields on `ProcessingPipeline`:

```rust
/// All unique anchor path IDs across all active registrations.
/// One entry per distinct anchor path — not per resource.
anchor_path_ids: HashSet<u32>,

/// Rebuilt once at the start of each processChanges call, after initial
/// shadow state update. Maps anchor_path_id → is_present.
anchor_states: HashMap<u32, bool>,
```

**Why a two-field design:**

- `anchor_path_ids` accumulates at registration time — deduplicated across all registrations
  that share the same anchor path
- `anchor_states` is rebuilt once per pipeline run — one `shadow.get()` call per unique anchor
  path, regardless of how many resources reference it
- During processing, each resource does one `HashMap` lookup — O(1), no shadow access

#### Resource-Level Embedding

Each resource entry in its own registry stores `anchor_path_id: Option<u32>`:

```rust
// BoolLogicMeta, ValueLogicMeta, FunctionEntry (validators), listener entry in TopicRouter
anchor_path_id: Option<u32>,
```

This is the "on-premise" anchor reference. No cross-cutting skip-set computation, no separate
`RegistrationGroup` indirection. Each resource owns its own constraint.

#### Lifecycle

```
Registration:
  1. intern(anchor_path) → anchor_path_id
  2. anchor_path_ids.insert(anchor_path_id)      ← register to check set
  3. resource.anchor_path_id = Some(anchor_path_id)  ← embed in entry

processChanges (after Step 3 — shadow updated for user's initial changes):
  update_anchor_states():
    anchor_states.clear()
    for id in anchor_path_ids:
      anchor_states[id] = shadow.get(intern.resolve(id)).is_some()
    ← O(n_unique_anchors) shadow lookups, then frozen for rest of run

BoolLogic evaluation:
  if !is_anchor_enabled(meta.anchor_path_id) { continue }  ← O(1) map lookup

Validator collection:
  if !is_anchor_enabled(entry.anchor_path_id) { continue }

Listener routing:
  filter execution plan dispatches where !is_anchor_enabled(subscriber.anchor_path_id)

Unregistration (React unmount):
  anchor_path_ids.remove(anchor_path_id)   ← ref-counted, remove when last reference gone
  anchor_states entry cleaned up next run automatically (key no longer in set)
```

---

### What Gets Skipped

| Resource type | Skip mechanism |
|---|---|
| **BoolLogic** | Skipped during `buf_affected_ids` evaluation loop |
| **ValueLogic** | Skipped during `buf_affected_value_logics` evaluation loop |
| **Validators** | Skipped during `validators_to_run` collection |
| **Listeners** | Filtered from execution plan dispatches |
| **Sync pairs** | Not explicitly skipped — existing `parent_exists` guard handles absent paths |
| **Flip pairs** | Same — existing diff pre-pass produces no-op for absent paths |
| **Aggregation** | Same — source reads return `None` for absent paths, target unchanged |
| **Computation** | Same — natural no-op |

The last four categories are covered by existing guards in the pipeline and do not require
explicit anchor tracking.

---

### Public API

#### Side Effects

```typescript
useSideEffects({
  anchorPath: 'user.profile',   // skip all effects when user.profile is absent
  listeners: [{ path: 'user.profile.email', fn: handleEmailChange }],
  syncPaths: [['user.profile.theme', 'ui.theme']],
})
```

#### Concerns (via `useConcerns` / `registerConcernEffects`)

```typescript
useConcerns(
  'user.profile',
  { validationState: { schema: profileSchema } },
  { anchorPath: 'user.profile' },  // third argument — options
)
```

---

### Implementation Plan — anchorPath

#### Rust (`rust/src/pipeline.rs`)

1. Add `anchor_path_ids: HashSet<u32>` and `anchor_states: HashMap<u32, bool>` to
   `ProcessingPipeline`.

2. Add `is_anchor_enabled(anchor_path_id: Option<u32>) -> bool` helper:

   ```rust
   fn is_anchor_enabled(&self, id: Option<u32>) -> bool {
       match id {
           None => true,
           Some(id) => self.anchor_states.get(&id).copied().unwrap_or(true),
       }
   }
   ```

3. Add `update_anchor_states()` — called in `run_pipeline_core` after Step 3 (shadow updated):

   ```rust
   fn update_anchor_states(&mut self) {
       self.anchor_states.clear();
       for &id in &self.anchor_path_ids {
           if let Some(path) = self.intern.resolve(id) {
               self.anchor_states.insert(id, self.shadow.get(path).is_some());
           }
       }
   }
   ```

4. Update `SideEffectsRegistration`:

   ```rust
   #[serde(default)]
   pub anchor_path: Option<String>,
   ```

5. Update `ConcernsRegistration` (currently missing `registration_id` — add both):

   ```rust
   pub registration_id: String,
   #[serde(default)]
   pub anchor_path: Option<String>,
   ```

6. Update `register_side_effects`: if `anchor_path` is set, intern it → pass `anchor_path_id`
   to each listener registration call.

7. Update `register_concerns`: if `anchor_path` is set, intern it → pass `anchor_path_id` to
   each BoolLogic, ValueLogic, and validator registration call.

8. Wire skip checks into `run_pipeline_core` (after `update_anchor_states()`):
   - BoolLogic loop: `if !self.is_anchor_enabled(meta.anchor_path_id) { continue }`
   - ValueLogic loop: same
   - Validator collection: same
   - Execution plan: filter dispatches whose subscriber's `anchor_path_id` is disabled

#### Rust — Subsystems

| File | Change |
|---|---|
| `rust/src/bool_logic.rs` | `anchor_path_id: Option<u32>` on `BoolLogicMeta`; `register()` accepts it |
| `rust/src/value_logic.rs` | Same |
| `rust/src/functions.rs` | `anchor_path_id: Option<u32>` on `FunctionEntry`; `register()` accepts it |
| `rust/src/router.rs` | `anchor_path_id: Option<u32>` on listener entry; expose in dispatch info |

#### TypeScript

| File | Change |
|---|---|
| `src/types/side-effects.ts` | `anchorPath?: DeepKey<DATA>` on `SideEffects<DATA>` |
| `src/wasm/bridge.ts` | `anchor_path?: string` on `RegisterSideEffectsInput` and `RegisterConcernsInput` |
| `src/sideEffects/registration.wasm-impl.ts` | Pass `anchor_path: effects.anchorPath` through to WASM |
| `src/concerns/registration.wasm-impl.ts` | Accept `options?: { anchorPath?: string }`; pass through |

No changes to `processChangesWasm`, `Registrations` type, or cleanup functions — WASM handles
the skip entirely.

---

### Example: Dynamic Form Section

```typescript
// This section only exists when the user opts into advanced settings
useSideEffects({
  anchorPath: 'form.advancedSection',
  listeners: [{
    path: 'form.advancedSection',
    fn: (changes) => { /* validate section */ },
  }],
  syncPaths: [
    ['form.advancedSection.email', 'ui.lastKnownEmail'],
  ],
})

// When state.form.advancedSection is removed:
//   → listener does not fire
//   → sync pair is a no-op (existing parent_exists guard)
//   → registration stays registered
//   → when form.advancedSection is re-added, everything resumes
```

---

### Non-Goals

- **Auto-unregistration**: the registration is never removed by WASM. React lifecycle owns cleanup.
- **Null as "absent"**: setting a path to `null` is a value, not removal. Use BoolLogic `EXISTS`
  for value-based gating.
- **Per-resource anchors**: one anchor per registration block (`useSideEffects` call or
  `useConcerns` call). Individual sync pairs, flip pairs, or listeners cannot have independent
  anchors.
- **Wildcard anchors**: anchor path must be a concrete path, not a pattern.
