# WASM Pipeline Architecture

Proposed architecture for moving apex-state's pipeline and evaluation engine to Rust/WASM while keeping React integration and valtio reactivity in JS.

---

## Design Principles

1. **WASM owns pipeline orchestration** — graphs, routing, dependency tracking, evaluation order
2. **JS owns execution** — valtio proxies, React rendering, listener handlers, validation
3. **Simple primary function** — `processChanges(changes) → changes` - one function does everything
4. **Minimize boundary crossings** — batch data transfer, dispatch plans instead of per-item callbacks
5. **Zero consumer impact** — WASM ships pre-compiled and base64-inlined; consumers see a normal npm package

**Core Concept:** WASM is the **orchestrator**. It decides what to evaluate, when to call listeners, and what order to process things. JS is the **executor** - it runs handlers and applies changes to proxies.

---

## WASM API Specification

### Core Principles

1. **WASM owns state** — All computational state lives in WASM (shadow state, graphs, registries)
2. **JS owns reactivity** — Valtio proxies, React rendering, effect subscriptions
3. **Minimal serialization** — Primitives passed directly, complex values via slot indices
4. **Batch operations** — Single pipeline call processes all changes, returns all results

### WASM Exports (Rust → JS)

#### Initialization

```rust
/// Initialize WASM module and internal state
/// Called once when Provider mounts
fn init() -> Result<(), JsValue>
```

#### String Interning

```rust
/// Intern a single path string, returns unique u32 ID
/// IDs are stable across calls (same string → same ID)
fn intern(path: &str) -> u32

/// Intern multiple paths in batch (more efficient)
/// Returns array of IDs in same order as input
fn intern_batch(paths: Vec<String>) -> Vec<u32>

/// Resolve path ID back to string (debug/error messages only)
fn resolve(path_id: u32) -> Option<String>

/// Get total number of interned paths
fn intern_count() -> u32

/// Clear all interned paths (testing only)
fn intern_clear()
```

#### Shadow State Management

```rust
/// Initialize shadow state with the nested state tree
/// Called ONCE during Provider mount after extracting getters
/// Input: JSON object with nested structure (matches valtio state shape)
/// Example:
/// {
///   "user": {
///     "profile": { "name": "Alice", "email": "alice@example.com" },
///     "role": "admin"
///   },
///   "count": 42
/// }
///
/// WASM internally:
///   1. Builds nested shadow state tree (ValueRepr::Object recursively)
///   2. Interns all discovered paths during traversal
///      ("user", "user.profile", "user.profile.name", "user.profile.email", etc.)
///   3. Stores path → path_id mapping internally for reverse dependency lookups
///
/// Returns: nothing (void)
/// Path IDs are internal to WASM - JS uses string paths
fn shadow_init(state_json: &str) -> Result<(), JsValue>

/// Get value from shadow state by path (debug/testing only)
/// Traverses the nested structure to find the value
/// Path: "user.profile.email" → walks root["user"]["profile"]["email"]
fn shadow_get(path: &str) -> Option<String> // Returns JSON value

/// Dump entire shadow state as nested JSON (testing/debug only)
/// Returns the full tree structure
fn shadow_dump() -> String
```

**Note:** After initialization, shadow state is kept in sync AUTOMATICALLY by the pipeline processing. Updates can happen at ANY level:
- Leaf value: `{ path: "user.profile.email", value: "new@example.com" }`
- Nested object: `{ path: "user.profile", value: { name: "Bob", email: "bob@example.com" } }`
- Root level: `{ path: "user", value: { profile: {...}, role: "..." } }`

The shadow state tree structure supports all these update patterns.

#### BoolLogic Registration

```rust
/// Register a BoolLogic expression
/// WASM has NO concept of "concerns" - it just evaluates boolean expressions
/// and writes results to output paths
///
/// Arguments:
///   output_path: &str   - Where to write the result (e.g., "_concerns.user.email.disabledWhen")
///   tree_json: &str     - Serialized BoolLogicNode tree with STRING paths
///
/// WASM internally:
///   1. Parses the tree and extracts all input paths (dependencies)
///   2. Interns all paths (input + output)
///   3. Updates reverse dependency index: input_path_id → [logic_ids]
///   4. Stores logic: { output_path_id, tree }
///
/// Returns: u32 (logic_id) for cleanup
///
/// Example:
///   output_path = "_concerns.user.email.disabledWhen"
///   tree = { IS_EQUAL: ["user.role", "admin"] }
///
///   → When "user.role" changes, evaluate tree, write result to output_path
fn register_boollogic(
    output_path: &str,
    tree_json: &str
) -> Result<u32, JsValue>

/// Unregister a BoolLogic concern
/// Removes from registry and cleans up reverse index
fn unregister_boollogic(logic_id: u32)

/// Get all logic IDs affected by a path change (for testing/debug)
/// Takes STRING path, not path_id
fn affected_by_change(path: &str) -> Vec<u32>

/// Clear all BoolLogic registrations (testing only)
fn clear_boollogic()
```

#### Change Processing - THE MAIN FUNCTION

```rust
/// Process state changes through the complete pipeline
/// This is the SINGLE ENTRY POINT for ALL state changes
///
/// Signature: processChanges(changes) → changes
///
/// Input: Array of changes
/// [
///   { "path": "user.role", "value_json": "\"admin\"" },
///   { "path": "user.active", "value_json": "true" }
/// ]
///
/// Output: Array of ALL changes (input + computed)
/// [
///   { "path": "user.role", "value_json": "\"admin\"" },                      // Input (echo)
///   { "path": "user.active", "value_json": "true" },                         // Input (echo)
///   { "path": "user.email", "value_json": "\"admin@example.com\"" },        // Sync result
///   { "path": "user.settings.emailNotifications", "value_json": "false" },   // Flip result
///   { "path": "user.stats.lastActive", "value_json": "\"2026-02-14\"" },    // Listener result
///   { "path": "_concerns.user.email.disabledWhen", "value_json": "true" },  // BoolLogic result
/// ]
///
/// WASM Pipeline (Phase-dependent):
///   Phase 1 (BoolLogic only):
///     1. Update shadow state
///     2. Evaluate BoolLogic
///     3. Return input + BoolLogic results
///
///   Phase 2 (+ Sync/Flip):
///     1. Update shadow state
///     2. Process sync pairs → generate sync changes
///     3. Process flip pairs → generate flip changes
///     4. Evaluate BoolLogic on ALL affected paths
///     5. Return input + sync + flip + BoolLogic results
///
///   Phase 3 (+ Listeners):
///     1. Update shadow state
///     2. Process sync/flip
///     3. Seed listener dispatch (find matching topics)
///     4. FOR EACH depth level:
///        a. Generate dispatch plan → return to JS
///        b. JS executes handlers → returns produced changes
///        c. WASM routes produced changes to downstream topics
///     5. Evaluate BoolLogic on ALL affected paths
///     6. Return complete change list
///
/// JS receives a FLAT LIST and applies ALL changes:
///   for (const change of result) {
///     dot.set(store, change.path, change.value)
///   }
///
fn process_changes(changes_json: &str) -> Result<String, JsValue>
```

**Key Points:**
- **Same function signature across all phases** - complexity is internal
- **WASM orchestrates** - decides what to evaluate and when
- **JS executes** - runs listener handlers, applies changes
- **Flat output** - all changes treated uniformly

#### Side Effect Registration (Phase 2)

```rust
/// Register all side effects in a single batch call
/// Called once when useSideEffects() mounts
/// WASM builds sync/flip graphs and aggregation mappings
///
/// Input JSON format:
/// {
///   "sync_pairs": [[path_id_1, path_id_2], [path_id_3, path_id_4], ...],
///   "flip_pairs": [[path_id_5, path_id_6], ...],
///   "aggregations": [
///     { "target": path_id_7, "sources": [path_id_8, path_id_9, ...] },
///     ...
///   ]
/// }
///
/// Returns: JSON with initial sync changes to apply (if values differ)
/// {
///   "sync_changes": [{ "path_id": u32, "value_json": "..." }, ...]
/// }
fn register_side_effects(config_json: &str) -> Result<String, JsValue>

/// Clear all side effects (when useSideEffects() unmounts or for testing)
fn clear_side_effects()
```

#### Listener Registration (Phase 3)

```rust
/// Register multiple listeners in a single batch call
/// Called when useListeners() mounts
/// WASM builds topic router and pre-computes routes
///
/// Input JSON format:
/// {
///   "listeners": [
///     { "id": u32, "topic_path": "user.profile", "scope_path": "user" },
///     ...
///   ]
/// }
///
/// Listener IDs are provided by JS for mapping to handler functions
/// Handler functions stay in JS - WASM just orchestrates when to call them
fn register_listeners(config_json: &str) -> Result<(), JsValue>

/// Clear all listeners (when useListeners() unmounts or for testing)
fn clear_listeners()
```

#### Listener Dispatch Protocol (Phase 3)

Listener execution requires collaboration between WASM and JS:

**Step 1: WASM prepares dispatch plan**
```rust
/// Called internally during process_changes()
/// Returns which listeners to call and what changes to pass them
/// Grouped by depth level (deepest first)
fn prepare_listener_dispatch(affected_paths: Vec<u32>) -> DispatchPlan

struct DispatchPlan {
    levels: Vec<DispatchLevel>
}

struct DispatchLevel {
    depth: u32,
    dispatches: Vec<ListenerDispatch>
}

struct ListenerDispatch {
    listener_id: u32,           // Maps to JS handler function
    scope_path: String,          // For resolving scoped state
    changes: Vec<Change>,        // Relativized to topic prefix
}
```

**Step 2: JS executes handlers**
```typescript
// JS receives dispatch plan from WASM
for (const level of dispatchPlan.levels) {
  const produced = []

  for (const dispatch of level.dispatches) {
    const handler = listenerHandlers.get(dispatch.listener_id)
    const scopedState = resolveScopedState(dispatch.scope_path)
    const result = handler(dispatch.changes, scopedState)

    if (result) {
      produced.push(...result)  // Collect produced changes
    }
  }

  // Send produced changes back to WASM for routing
  wasm.route_listener_results(JSON.stringify(produced))
}
```

**Step 3: WASM routes results**
```rust
/// Routes listener-produced changes to downstream topics
/// Called by JS after executing each depth level
/// Updates internal change queue for next depth level
fn route_listener_results(produced_json: &str) -> Result<(), JsValue>
```

#### Pipeline Processing (Phase 2+3)

```rust
/// Process state changes through the full pipeline
/// This is the MAIN HOT PATH after Phase 2
///
/// Input: JSON array of Change objects:
/// [
///   { path_id: 5, value_json: "\"new value\"", meta: 0 },
///   { path_id: 12, value_json: "42", meta: 0 }
/// ]
///
/// Output: PipelineResult (JSON)
/// {
///   "final_changes": [{ path_id, value_json, meta }],
///   "boollogic_results": [[path_id, concern_name, bool]],
///   "listener_dispatch": { /* depth-grouped dispatch plan */ },
///   "validation_plan": [[path_id, validator_id]]
/// }
fn process_changes(changes_json: &str) -> Result<String, JsValue>

/// Route listener-produced changes to downstream topics
/// Called after each depth level of listener execution
/// Returns next batch of listener dispatches
fn route_listener_results(produced_json: &str) -> String // JSON: next dispatch level
```

#### Validation Registration (Phase 4)

```rust
/// Register a validator (Zod schema stays in JS)
/// WASM only tracks dependencies for batching
fn register_validator(
    path_id: u32,
    validator_id: u32,
    dependency_path_ids: Vec<u32>
)

/// Unregister a validator
fn unregister_validator(validator_id: u32)
```

#### Diagnostics

```rust
/// Get statistics about internal state (for debugging)
/// Returns JSON with counts, memory usage, etc.
fn get_stats() -> String

/// Get reverse dependency stats
fn rev_deps_stats() -> String

/// Dump shadow state as JSON (testing/debug only)
fn shadow_dump() -> String
```

---

### JS Bridge Layer

The JS bridge provides a thin TypeScript wrapper over WASM exports for type safety and convenience.

#### `src/wasm/bridge.ts`

```typescript
/**
 * Thin bridge over WASM exports
 * Handles serialization/deserialization only
 * NO state management, NO caching, NO business logic
 */

import * as wasm from '../../rust/pkg/apex_state_wasm'
import type { BoolLogic } from '~/types'

// ============================================================================
// Types
// ============================================================================

export interface Change {
  path: string    // Full path including prefix (e.g., "_concerns.user.email.disabledWhen")
  value: unknown
}

export interface ProcessResult {
  changes: Change[]  // ALL changes (state + concerns + anything else)
  // Phase 3+4 fields added later:
  // listenerDispatch?: DispatchPlan
  // validationPlan?: ValidationTask[]
}

// ============================================================================
// Initialization
// ============================================================================

export const initWasm = (): void => {
  wasm.init()
}

// ============================================================================
// String Interning (Internal/Debug Only)
// ============================================================================

// Note: Path interning is handled internally by WASM
// These functions are for testing/debugging only

export const internPath = (path: string): number => {
  return wasm.intern(path)
}

export const resolvePath = (pathId: number): string | null => {
  return wasm.resolve(pathId) ?? null
}

// ============================================================================
// Shadow State (Initialization Only)
// ============================================================================

/**
 * Initialize shadow state with the nested state object
 * Called ONCE in Provider during mount
 */
export const shadowInit = (state: Record<string, unknown>): void => {
  // Pass the nested state directly to WASM
  // WASM will:
  //   1. Build nested shadow state tree
  //   2. Intern all paths internally (not exposed to JS)
  wasm.shadow_init(JSON.stringify(state))
}

/**
 * NOTE: There is NO shadowSet() function!
 * Shadow state is kept in sync automatically by evaluate_affected() and process_changes()
 * They update shadow state internally as part of their processing.
 *
 * Updates can happen at any level:
 *   - Leaf: { path: "user.email", value: "new value" }
 *   - Nested: { path: "user.profile", value: { name: "...", email: "..." } }
 *   - Root: { path: "user", value: { entire tree } }
 */

// ============================================================================
// BoolLogic
// ============================================================================

export const registerBoolLogic = (
  outputPath: string,   // Full output path: "_concerns.user.email.disabledWhen"
  tree: BoolLogic<any>
): number => {
  // WASM doesn't know about concerns - just paths
  // The concern name is part of the output path
  const treeJson = JSON.stringify(tree)
  return wasm.register_boollogic(outputPath, treeJson)
}

export const unregisterBoolLogic = (logicId: number): void => {
  wasm.unregister_boollogic(logicId)
}

/**
 * Process state changes - THE MAIN ENTRY POINT
 * Handles the complete pipeline including listener execution
 *
 * Phase 1: Just BoolLogic
 * Phase 2: + Sync/Flip
 * Phase 3: + Listeners (requires multiple WASM↔JS calls)
 */
export const processChanges = async (
  changes: Change[],
  listenerHandlers?: Map<number, ListenerHandler>
): Promise<Change[]> => {
  const changesJson = JSON.stringify(
    changes.map(c => ({
      path: c.path,
      value_json: JSON.stringify(c.value)
    }))
  )

  // Phase 1 & 2: Single WASM call returns everything
  if (!listenerHandlers || listenerHandlers.size === 0) {
    const resultJson = wasm.process_changes(changesJson)
    const result = JSON.parse(resultJson)

    return result.changes.map((c: any) => ({
      path: c.path,
      value: JSON.parse(c.value_json)
    }))
  }

  // Phase 3: Multiple WASM↔JS calls for listener dispatch
  const resultJson = wasm.process_changes(changesJson)
  const result = JSON.parse(resultJson)

  // Execute listener dispatch if present
  if (result.listener_dispatch) {
    for (const level of result.listener_dispatch.levels) {
      const produced: Change[] = []

      // Execute handlers for this depth level
      for (const dispatch of level.dispatches) {
        const handler = listenerHandlers.get(dispatch.listener_id)
        if (!handler) continue

        const scopedState = resolveScopedState(dispatch.scope_path)
        const handlerResult = handler(dispatch.changes, scopedState)

        if (handlerResult && handlerResult.length > 0) {
          produced.push(...handlerResult)
        }
      }

      // Route produced changes back to WASM
      if (produced.length > 0) {
        const producedJson = JSON.stringify(
          produced.map(c => ({
            path: c.path,
            value_json: JSON.stringify(c.value)
          }))
        )
        wasm.route_listener_results(producedJson)
      }
    }

    // Get final results after all listener dispatch
    const finalJson = wasm.get_final_changes()
    const final = JSON.parse(finalJson)

    return final.changes.map((c: any) => ({
      path: c.path,
      value: JSON.parse(c.value_json)
    }))
  }

  // No listeners, just return changes
  return result.changes.map((c: any) => ({
    path: c.path,
    value: JSON.parse(c.value_json)
  }))
}

/**
 * Apply changes returned from WASM
 * Uses dot-notation to set values on the store object
 */
export const applyChanges = (store: any, changes: Change[]): void => {
  for (const change of changes) {
    // Paths like "_concerns.user.email.disabledWhen" automatically
    // route to store._concerns.user.email.disabledWhen
    dot.set(store, change.path, change.value)
  }
}

// ============================================================================
// Phase 2: Side Effects
// ============================================================================

export interface SideEffectsConfig {
  syncPairs: Array<[number, number]>
  flipPairs: Array<[number, number]>
  aggregations: Array<{
    target: number
    sources: number[]
  }>
}

export interface SyncChanges {
  pathId: number
  value: unknown
}

export const registerSideEffects = (config: SideEffectsConfig): SyncChanges[] => {
  const configJson = JSON.stringify({
    sync_pairs: config.syncPairs,
    flip_pairs: config.flipPairs,
    aggregations: config.aggregations
  })

  const resultJson = wasm.register_side_effects(configJson)
  const result = JSON.parse(resultJson)

  return result.sync_changes.map((c: any) => ({
    pathId: c.path_id,
    value: JSON.parse(c.value_json)
  }))
}

export const clearSideEffects = (): void => {
  wasm.clear_side_effects()
}

// ============================================================================
// Phase 3: Listeners
// ============================================================================

export interface ListenerConfig {
  id: number
  topicPathId: number
  scopePathId: number
}

export const registerListeners = (listeners: ListenerConfig[]): void => {
  const configJson = JSON.stringify({
    listeners: listeners.map(l => ({
      id: l.id,
      topic_path_id: l.topicPathId,
      scope_path_id: l.scopePathId
    }))
  })

  wasm.register_listeners(configJson)
}

export const clearListeners = (): void => {
  wasm.clear_listeners()
}

// ============================================================================
// Phase 2+: Pipeline (stub for now)
// ============================================================================

export const processChanges = (changes: Change[]): PipelineResult => {
  const changesJson = JSON.stringify(
    changes.map(c => ({
      path_id: c.pathId,
      value_json: serializeValue(c.value),
      meta: c.meta ?? 0
    }))
  )

  const resultJson = wasm.process_changes(changesJson)
  const raw = JSON.parse(resultJson)

  return {
    finalChanges: raw.final_changes.map((c: any) => ({
      pathId: c.path_id,
      value: JSON.parse(c.value_json),
      meta: c.meta
    })),
    boolLogicResults: raw.boollogic_results.map((r: any) => ({
      pathId: r[0],
      concernName: r[1],
      value: r[2]
    }))
  }
}

// ============================================================================
// Cleanup
// ============================================================================

export const clearAll = (): void => {
  wasm.clear_boollogic()
  wasm.clear_rev_deps()
  wasm.intern_clear()
}
```

---

### Communication Patterns

#### Phase 1: BoolLogic Evaluation (Current)

**Registration Flow:**
```
JS: useConcerns() mounts with BoolLogic concern
 ├─ Build output path: "_concerns.user.email.disabledWhen"
 ├─ Extract condition from concern config
 ├─ Register: logicId = registerBoolLogic("_concerns.user.email.disabledWhen", condition)
 │
 │   WASM (register_boollogic):
 │   ├─ Parse tree, extract input paths: ["user.role"]
 │   ├─ Intern all paths:
 │   │   "user.role" → pathId 42 (input)
 │   │   "_concerns.user.email.disabledWhen" → pathId 103 (output)
 │   ├─ Store logic: { output_path_id: 103, tree }
 │   ├─ Update reverse index: rev_index[42] = [..., logicId]
 │   └─ Return logicId
 │
 └─ Store logicId for cleanup

Note: WASM has NO concept of "concerns" - just paths
```

**Change Processing Flow:**
```
JS: User changes state via setValue("user.role", "admin")
 ├─ Call WASM: result = processChanges([{ path: "user.role", value: "admin" }])
 │
 │   WASM (single call, does EVERYTHING):
 │   ├─ Intern path "user.role" → pathId 42 (cached from init)
 │   ├─ Traverse shadow state: root["user"]["role"]
 │   ├─ Update shadow value: root["user"]["role"] = "admin"
 │   ├─ Reverse index lookup: rev_index[42] = [101, 205]
 │   ├─ For each affected logic (e.g., logic 101):
 │   │   ├─ Evaluate tree against updated shadow state
 │   │   │   Tree: IS_EQUAL("user.role", "admin")
 │   │   │   Result: true
 │   │   ├─ Resolve output_path_id 103 → "_concerns.user.email.disabledWhen"
 │   │   └─ Add to output changes
 │   └─ Return ALL changes (input + computed):
 │       {
 │         changes: [
 │           { path: "user.role", value: "admin" },
 │           { path: "_concerns.user.email.disabledWhen", value: true }
 │         ]
 │       }
 │
 └─ Apply ALL changes using dot notation:
     for (const change of result.changes) {
       dot.set(store, change.path, change.value)
       // "user.role" → store.user.role = "admin"
       // "_concerns.user.email.disabledWhen" → store._concerns.user.email.disabledWhen = true
     }
```

**Example: Setting a nested object**
```
JS: setValue("user.profile", { name: "Bob", email: "bob@example.com" })
 ├─ result = processChanges([{
     path: "user.profile",
     value: { name: "Bob", email: "bob@example.com" }
   }])
 │
 │   WASM:
 │   ├─ Update shadow state at "user.profile"
 │   ├─ Calculate affected input paths: ["user.profile", "user.profile.name", "user.profile.email"]
 │   ├─ Find all logics depending on these paths
 │   ├─ Evaluate each logic, collect output changes
 │   ├─ Return: { changes: [...input..., ...outputs...] }
 │
 └─ Apply ALL changes:
     dot.set(store, "user.profile", { name: "Bob", email: "..." })
     dot.set(store, "_concerns.user.profile.visibleWhen", true)
     // etc.
```

**Data Flow Diagram:**
```
┌─────────────────────────────────────────────────────────────┐
│ JS Layer                                                    │
├─────────────────────────────────────────────────────────────┤
│ setValue("user.role", "admin")                             │
│   ↓                                                         │
│ result = processChanges([{                                 │
│   path: "user.role",                                       │
│   value: "admin"                                           │
│ }])                                            ← 1 boundary │
│   ↓                                                         │
│ result = {                                                 │
│   changes: [                                               │
│     { path: "user.role", value: "admin" },                │
│     { path: "_concerns.user.email.disabledWhen",          │
│       value: true }                                        │
│   ]                                                         │
│ }                                                           │
│   ↓                                                         │
│ Apply ALL using dot notation:                              │
│   for (change of result.changes) {                         │
│     dot.set(store, change.path, change.value)             │
│   }                                                         │
│   → store.user.role = "admin"                             │
│   → store._concerns.user.email.disabledWhen = true       │
└─────────────────────────────────────────────────────────────┘
                            ↕
┌─────────────────────────────────────────────────────────────┐
│ WASM Layer (AGNOSTIC - just paths and values)              │
├─────────────────────────────────────────────────────────────┤
│ process_changes([{ path: "user.role", value: "admin" }])  │
│                                                             │
│ 1. Intern path: "user.role" → pathId 42                   │
│                                                             │
│ 2. Update shadow state:                                    │
│    root["user"]["role"] = "admin"                          │
│                                                             │
│ 3. Find affected expressions:                              │
│    rev_index[42] = [101, 205]  (logic IDs)                │
│                                                             │
│ 4. Evaluate each expression:                               │
│    logic_registry[101]:                                    │
│      output_path_id: 103                                   │
│      tree: IS_EQUAL("user.role", "admin")                  │
│    → Evaluate → true                                       │
│    → Resolve 103 → "_concerns.user.email.disabledWhen"   │
│                                                             │
│ 5. Build flat change list:                                 │
│    {                                                        │
│      changes: [                                            │
│        { path: "user.role", value_json: "\"admin\"" },    │
│        { path: "_concerns.user.email.disabledWhen",       │
│          value_json: "true" }                              │
│      ]                                                      │
│    }                                                        │
│                                                             │
│ Return → JS applies via dot notation                       │
│                                                             │
│ Note: WASM has NO knowledge of "state" vs "_concerns"     │
│       It just knows: input paths → evaluate → output paths │
└─────────────────────────────────────────────────────────────┘
```

**Boundary Crossings: 1 per state change** (processChanges - single entry point)

**Key Principle: WASM is a pure evaluation engine**
- Input: paths + values
- Processing: evaluate expressions
- Output: paths + values
- NO knowledge of state structure, concerns, React, or any JS concepts

**Key Points:**
- **Path IDs are INTERNAL to WASM** - used for efficient lookups
- **JS ↔ WASM boundary uses STRING paths** - simpler, self-documenting
- Shadow state is a **nested tree structure**, not a flat map
- Updates use **path traversal** to find the target location
- Setting a nested object **replaces the entire subtree**
- BoolLogic evaluation **traverses paths** to get values

---

#### Phase 2+: Full Pipeline (Future)

**Single Pipeline Call:**
```
JS: processChanges([{ pathId: 42, value: "admin" }])
│
│   WASM (all in one call):
│   ├─ Update shadow state
│   ├─ Process aggregation writes
│   ├─ Process sync paths
│   ├─ Process flip paths
│   ├─ Evaluate affected BoolLogics
│   ├─ Prepare listener dispatch plan
│   └─ Return PipelineResult
│
└─ JS: Apply final changes + BoolLogic results + dispatch listeners
```

**Boundary Crossings: 1 for pipeline + 2-4 for listener dispatch rounds**

---

## Ownership Split

### Lives in WASM (Rust)

| Component | Description |
|---|---|
| **Shadow State** | Getter-free copy of all state values, keyed by interned path IDs |
| **String Interning Table** | Bidirectional map: path string <-> u32 path ID |
| **Sync Graph** | Connected components for synchronized paths |
| **Flip Graph** | Connected components for inverted boolean paths |
| **Topic Router** | Listener topic hierarchy with pre-computed routes |
| **Pipeline Engine** | Aggregation writes, sync, flip, listener routing |
| **BoolLogic Evaluator** | Recursive tree walker over declarative logic definitions |
| **Reverse Dependency Index** | Maps path IDs to affected BoolLogic IDs and validator IDs |
| **Value Slot Registry** | Tracks which JS-side value slots are in use |

### Lives in JS

| Component | Description |
|---|---|
| **Valtio Proxy (`state`)** | Reactive state for React — the "notification layer" |
| **Valtio Proxy (`_concerns`)** | Computed concern results consumed by React components |
| **Listener Handler Functions** | User-defined JS functions that react to state changes |
| **Zod Schema Validators** | Schema instances, called in batch when Rust requests validation |
| **Getter Functions** | JS getters on state objects (`get total() { ... }`) |
| **Custom Concern `evaluate()`** | User-defined concern logic that can't be expressed as BoolLogic |
| **React Hooks** | `useStore`, `useJitStore`, `useConcerns`, `withConcerns`, etc. |
| **`applyBatch`** | Writes final changes to valtio proxy (triggers React re-renders) |

---

## Data Structures

### Shared Between JS and WASM

```
String Interning Table
  JS:   Map<string, u32>     (path string -> path ID)
  WASM: HashMap<u32, String>  (path ID -> path string, for debug only)

  Populated at registration time. Both sides reference paths by u32 ID.

Value Slot Array (JS-owned, WASM-referenced by index)
  JS:   unknown[]             valueSlots[slotIndex] = actualJsValue
  WASM: refers to slots as u32 indices, never touches actual JS values

  WASM moves values by shuffling slot indices. JS resolves indices to
  real values only at the boundary (applyBatch, listener dispatch).
```

### WASM-Internal

```
Shadow State:       Nested object structure mirroring the state tree

  struct ShadowState {
      root: ValueRepr
  }

  enum ValueRepr {
      Object(HashMap<String, Box<ValueRepr>>),  // Nested object - supports deep nesting
      Array(Vec<Box<ValueRepr>>),               // Array
      String(String),                           // Primitive string
      Number(f64),                              // Primitive number
      Bool(bool),                               // Primitive boolean
      Null,                                     // null/undefined
  }

  Key capabilities:
  - Deep path traversal: "user.profile.email" → root["user"]["profile"]["email"]
  - Partial updates: Setting "user.profile.email" preserves "user.profile.name"
  - Full subtree replacement: Setting "user.profile" replaces entire profile object
  - Root updates: Setting "user" replaces entire user subtree

  Note: NOT a flat HashMap<path_id, value>. This is a tree structure that must
  support both leaf value updates AND nested object replacements.

BoolLogic Tree:     Deserializes directly from JS format

  // JS format (what we receive)
  { "IS_EQUAL": ["user.role", "admin"] }
  { "EXISTS": "user.email" }
  { "AND": [{ "IS_EQUAL": [...] }, { "EXISTS": "..." }] }

  // Rust deserialization (uses serde)
  #[derive(Deserialize)]
  #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
  enum BoolLogicNode {
      IsEqual(String, ValueRepr),      // Tuple: (path, expected)
      Exists(String),                  // Just path
      IsEmpty(String),
      And(Vec<BoolLogicNode>),         // Nested trees
      Or(Vec<BoolLogicNode>),
      Not(Box<BoolLogicNode>),
      Gt(String, f64),                 // (path, threshold)
      Lt(String, f64),
      Gte(String, f64),
      Lte(String, f64),
      In(String, Vec<ValueRepr>),      // (path, allowed_values)
  }

  Serde handles the deserialization automatically from JS array format.

  Example Rust implementation:
  ```rust
  use serde::Deserialize;

  #[derive(Deserialize)]
  #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
  enum BoolLogicNode {
      IsEqual(String, ValueRepr),      // Deserializes ["user.role", "admin"]
      Exists(String),                  // Deserializes "user.email"
      IsEmpty(String),
      And(Vec<BoolLogicNode>),         // Deserializes [tree1, tree2, ...]
      Or(Vec<BoolLogicNode>),
      Not(Box<BoolLogicNode>),         // Deserializes tree
      Gt(String, f64),                 // Deserializes ["user.age", 18]
      Lt(String, f64),
      Gte(String, f64),
      Lte(String, f64),
      In(String, Vec<ValueRepr>),      // Deserializes ["user.role", ["admin", "moderator"]]
  }

  impl BoolLogicNode {
      fn extract_paths(&self) -> Vec<String> {
          match self {
              BoolLogicNode::IsEqual(path, _) |
              BoolLogicNode::Exists(path) |
              BoolLogicNode::IsEmpty(path) |
              BoolLogicNode::Gt(path, _) |
              BoolLogicNode::Lt(path, _) |
              BoolLogicNode::Gte(path, _) |
              BoolLogicNode::Lte(path, _) |
              BoolLogicNode::In(path, _) => vec![path.clone()],
              BoolLogicNode::And(children) | BoolLogicNode::Or(children) => {
                  children.iter().flat_map(|c| c.extract_paths()).collect()
              }
              BoolLogicNode::Not(child) => child.extract_paths(),
          }
      }

      fn evaluate(&self, shadow: &ShadowState) -> bool {
          match self {
              BoolLogicNode::IsEqual(path, expected) => {
                  shadow.get(path) == Some(expected)
              }
              BoolLogicNode::Exists(path) => {
                  shadow.get(path).map(|v| v.exists()).unwrap_or(false)
              }
              // ... other operators
          }
      }
  }
  ```

  No transformation needed - Rust deserializes the JS format directly!

Path Interning:     HashMap<String, u32> and HashMap<u32, String>
  Bidirectional map for path strings ↔ path IDs
  Used for reverse dependency lookups (which BoolLogics depend on which paths)

Sync Graph:         Graph { node_to_component, component_to_nodes, adjacency, edges }
Flip Graph:         Graph { ... } (same structure)
Topic Router:       { topics, topic_meta, subscribers, routes, subscriber_meta }
BoolLogic Registry: HashMap<u32, BoolLogicMetadata>
  struct BoolLogicMetadata {
      logic_id: u32,
      target_path_id: u32,      // Where result is written (e.g., "user.email")
      concern_name: String,      // "disabledWhen", "visibleWhen", etc.
      tree: BoolLogicNode,       // Compiled tree with interned path IDs
  }

Reverse Deps:       HashMap<u32, Vec<u32>>         (path ID -> [logic IDs / validator IDs])
```

---

## Shadow State Path Traversal & Updates

### Path Structure

Shadow state is a **nested object tree** that mirrors the valtio state structure:

```rust
// Example shadow state
root: Object {
  "user": Object {
    "profile": Object {
      "name": "Alice",
      "email": "alice@example.com"
    },
    "role": "admin",
    "settings": Object {
      "theme": "dark"
    }
  },
  "count": 42
}
```

### Path Interning

All paths (including intermediate paths) are interned:

```
Path                    → Path ID
"user"                  → 1
"user.profile"          → 2
"user.profile.name"     → 3
"user.profile.email"    → 4
"user.role"             → 5
"user.settings"         → 6
"user.settings.theme"   → 7
"count"                 → 8
```

### Update Patterns

**1. Leaf Value Update**
```
setValue("user.profile.email", "bob@example.com")

WASM:
  1. Resolve path: "user.profile.email"
  2. Traverse: root["user"]["profile"]["email"]
  3. Update: root["user"]["profile"]["email"] = "bob@example.com"
  4. Affected paths: [4]  (just "user.profile.email")
```

**2. Nested Object Update**
```
setValue("user.profile", { name: "Bob", email: "bob@example.com" })

WASM:
  1. Resolve path: "user.profile"
  2. Traverse: root["user"]["profile"]
  3. Replace subtree:
     root["user"]["profile"] = Object {
       "name": "Bob",
       "email": "bob@example.com"
     }
  4. Affected paths: [2, 3, 4]  ("user.profile", "user.profile.name", "user.profile.email")
     → All paths under the replaced subtree are considered changed
```

**3. Root Level Update**
```
setValue("user", { profile: {...}, role: "user", settings: {...} })

WASM:
  1. Resolve path: "user"
  2. Traverse: root["user"]
  3. Replace entire subtree
  4. Affected paths: [1, 2, 3, 4, 5, 6, 7]  (all paths under "user")
```

### Affected Path Calculation

When a nested object is updated, **all descendant paths are considered affected**:

```rust
fn get_affected_paths(updated_path: &str) -> Vec<u32> {
    let mut affected = vec![path_to_id(updated_path)];

    // Add all paths that start with updated_path + "."
    for (path, path_id) in path_registry.iter() {
        if path.starts_with(&format!("{}.", updated_path)) {
            affected.push(*path_id);
        }
    }

    affected
}
```

This ensures that BoolLogic trees depending on `"user.profile.email"` are re-evaluated when `"user.profile"` is replaced.

---

## Flows

### 1. Initialization

```
JS: Provider mounts
  │
  ├─ initWasm()                          Load and instantiate WASM module
  │
  ├─ extractGetters(initialState)        Separate base data from getter functions
  │   ├─ base    → send to WASM
  │   └─ getters → keep in JS, reattach to proxy later
  │
  ├─ pathIdCache = shadowInit(base)      WASM initializes shadow state
  │   │                                  Returns path → pathId mapping
  │   │
  │   │   WASM (shadow_init):
  │   │   ├─ Intern all paths (generates stable u32 IDs)
  │   │   ├─ Store values in shadow_state HashMap
  │   │   └─ Return path → pathId map
  │   │
  │   └─ Store pathIdCache (bidirectional) for lookups
  │
  └─ proxy(initialState)                 Valtio proxy created (getters included)
```

**Key Points:**
- Shadow state is initialized ONCE with the full initial state
- After init, it's kept in sync automatically by `evaluate_affected()` (Phase 1) or `process_changes()` (Phase 2+)
- There is NO separate `shadowSet()` call
- Path interning happens during `shadow_init()` and is reused thereafter
- **JS doesn't need to cache path IDs** - WASM handles interning internally

### 2. Side Effect Registration

```
JS: useSideEffects() mounts
  │
  ├─ Sync pairs:   wasm.registerSyncPair(pathId1, pathId2)
  │                 WASM adds edge to sync graph, returns initial sync changes
  │
  ├─ Flip pairs:   wasm.registerFlipPair(pathId1, pathId2)
  │                 WASM adds edge to flip graph
  │
  ├─ Listeners:    wasm.registerListener(id, topicPathId, scopePathId)
  │                 WASM adds topic + subscriber to router
  │                 JS stores handler function in local map: handlerMap[id] = fn
  │
  └─ Aggregations: wasm.registerAggregation(targetPathId, sourcePathIds[])
                    WASM stores aggregation mapping
```

### 3. Concern Registration

```
JS: useConcerns() mounts
  │
  ├─ BoolLogic concerns (disabledWhen, visibleWhen, readonlyWhen, etc.):
  │   │
  │   ├─ Serialize BoolLogic tree to WASM: wasm.registerBoolLogic(pathId, logicTree)
  │   │   WASM compiles tree, extracts dependency paths, updates reverse index
  │   │
  │   └─ No effect() created — Rust handles dependency tracking + evaluation
  │
  ├─ Validation concerns (validationState with Zod schema):
  │   │
  │   ├─ wasm.registerValidator(pathId, validatorId, depPaths[])
  │   │   WASM records validator ID + updates reverse index
  │   │
  │   └─ JS stores schema: schemaMap[validatorId] = zodSchema
  │       Zod stays in JS. Rust only knows "validator 7 depends on paths [3, 17, 42]"
  │
  └─ Custom concerns (user-defined evaluate functions):
      │
      └─ Keep in effect() as today — valtio-reactive tracks deps at runtime
         These are the escape hatch for logic that can't be expressed declaratively
```

### 4. State Change (Hot Path)

```
JS: User calls setValue("user.email", "alice@example.com")
  │
  ├─ Resolve pathId from interning table
  ├─ Write value to valueSlots[nextSlot]
  ├─ Call wasm.processChanges([(pathId, slotIndex, metaFlags)])
  │
  ╔══════════════════════════════════════════════════════════════╗
  ║  WASM PIPELINE (single call, no boundary crossings)         ║
  ║                                                              ║
  ║  1. Update shadow state                                      ║
  ║     shadow[pathId] = valueSlots[slotIndex]                   ║
  ║                                                              ║
  ║  2. Aggregation writes                                       ║
  ║     If pathId is an aggregation target:                      ║
  ║       replace change with distributed source path changes    ║
  ║                                                              ║
  ║  3. Sync paths                                               ║
  ║     For each sync component containing a changed path:       ║
  ║       generate changes for all other paths in component      ║
  ║     (normalizeChangesForGroups runs entirely in Rust)        ║
  ║                                                              ║
  ║  4. Flip paths                                               ║
  ║     For each flip component containing a changed path:       ║
  ║       generate inverted boolean changes for peers            ║
  ║                                                              ║
  ║  5. Listener routing (compute dispatch plan)                 ║
  ║     Seed: walk changed paths upward, match to topics         ║
  ║     For each depth level: prepare [listenerId, changes[]]    ║
  ║                                                              ║
  ║  6. BoolLogic evaluation                                     ║
  ║     Reverse index: find all BoolLogics depending on changed  ║
  ║     paths. Batch-evaluate affected BoolLogics.               ║
  ║     Write results to concern output slots.                   ║
  ║                                                              ║
  ║  7. Identify affected validators                             ║
  ║     Reverse index: find all validators depending on changed  ║
  ║     paths. Prepare validation dispatch plan.                 ║
  ║                                                              ║
  ║  Return to JS:                                               ║
  ║    - finalChanges: [(pathId, slotIndex, metaFlags)]          ║
  ║    - listenerDispatch: [(listenerId, changes[])] per depth   ║
  ║    - boolLogicResults: [(pathId, concernName, bool)]         ║
  ║    - validationPlan: [(pathId, validatorId)]                 ║
  ╚══════════════════════════════════════════════════════════════╝
  │
  ├─ Apply BoolLogic results to _concerns proxy
  │   for (pathId, name, value) of boolLogicResults:
  │     store._concerns[paths[pathId]][name] = value
  │
  ├─ Execute listener dispatch (2-4 round trips for depth levels)
  │   for each depth level:
  │     for (listenerId, changes) of dispatch:
  │       scopedState = resolveScopedState(listenerId)
  │       produced = handlerMap[listenerId](changes, scopedState)
  │     wasm.routeListenerResults(produced)  →  WASM routes downstream
  │
  ├─ Execute validation dispatch (single batch)
  │   results = []
  │   for (pathId, validatorId) of validationPlan:
  │     result = schemaMap[validatorId].safeParse(valueSlots[pathId])
  │     results.push((pathId, result))
  │   Apply validation results to _concerns proxy
  │
  └─ Apply final changes to valtio proxy (triggers React re-renders)
      for (pathId, slotIndex) of finalChanges:
        dot.set(store.state, paths[pathId], valueSlots[slotIndex])
```

### 5. Change Processing (Phase 3 - With Listeners)

```
JS: setValue("user.profile.email", "bob@example.com")
  │
  ├─ changes = await processChanges([{
  │      path: "user.profile.email",
  │      value: "bob@example.com"
  │    }], listenerHandlers)
  │
  ╔══════════════════════════════════════════════════════════════╗
  ║  WASM: process_changes() - ORCHESTRATES EVERYTHING          ║
  ╠══════════════════════════════════════════════════════════════╣
  ║                                                              ║
  ║  1. Update shadow state                                      ║
  ║     root["user"]["profile"]["email"] = "bob@example.com"     ║
  ║                                                              ║
  ║  2. Process sync/flip (Phase 2)                              ║
  ║     Generate sync/flip changes                               ║
  ║                                                              ║
  ║  3. Seed listener dispatch                                   ║
  ║     Changed path: "user.profile.email"                       ║
  ║     Walk upward: "user.profile.email" → "user.profile" → "user"║
  ║     Match topics: topic("user.profile") matched              ║
  ║                                                              ║
  ║  4. Prepare dispatch plan (depth-first)                      ║
  ║     {                                                        ║
  ║       levels: [                                              ║
  ║         {                                                    ║
  ║           depth: 3,                                          ║
  ║           dispatches: [                                      ║
  ║             {                                                ║
  ║               listener_id: 101,                              ║
  ║               scope_path: "user",                            ║
  ║               changes: [{                                    ║
  ║                 path: "profile.email",  // Relativized       ║
  ║                 value: "bob@example.com"                     ║
  ║               }]                                             ║
  ║             }                                                ║
  ║           ]                                                  ║
  ║         }                                                    ║
  ║       ]                                                      ║
  ║     }                                                        ║
  ║                                                              ║
  ║  5. Return to JS with dispatch plan                          ║
  ╚══════════════════════════════════════════════════════════════╝
  │
  ├─ Execute listener dispatch (for each depth level)
  │   │
  │   ├─ For depth 3:
  │   │   ├─ handler = listenerHandlers.get(101)
  │   │   ├─ scopedState = store.user  // From scope_path
  │   │   ├─ produced = handler(changes, scopedState)
  │   │   │   // User handler returns:
  │   │   │   // [{ path: "stats.lastUpdated", value: "2026-02-14" }]
  │   │   └─ Send produced changes back to WASM
  │
  ╔══════════════════════════════════════════════════════════════╗
  ║  WASM: route_listener_results()                             ║
  ╠══════════════════════════════════════════════════════════════╣
  ║                                                              ║
  ║  1. Receive produced changes                                 ║
  ║     [{ path: "stats.lastUpdated", value: "2026-02-14" }]    ║
  ║                                                              ║
  ║  2. Route to downstream topics                               ║
  ║     Topic "stats" receives change                            ║
  ║                                                              ║
  ║  3. Prepare next depth level dispatch (if any)               ║
  ║                                                              ║
  ║  4. Return next dispatch plan OR done signal                 ║
  ╚══════════════════════════════════════════════════════════════╝
  │
  ├─ Continue until all depth levels processed
  │
  ╔══════════════════════════════════════════════════════════════╗
  ║  WASM: get_final_changes()                                  ║
  ╠══════════════════════════════════════════════════════════════╣
  ║                                                              ║
  ║  1. Collect ALL changes:                                     ║
  ║     - Original input                                         ║
  ║     - Sync/flip results                                      ║
  ║     - Listener-produced changes                              ║
  ║                                                              ║
  ║  2. Evaluate BoolLogic on ALL affected paths                 ║
  ║                                                              ║
  ║  3. Return complete change list:                             ║
  ║     [                                                        ║
  ║       { path: "user.profile.email", value: "bob..." },      ║
  ║       { path: "user.stats.lastUpdated", value: "2026..." }, ║
  ║       { path: "_concerns.user.email.disabledWhen",          ║
  ║         value: true }                                        ║
  ║     ]                                                        ║
  ╚══════════════════════════════════════════════════════════════╝
  │
  └─ Apply all changes to store
      for (const change of changes) {
        dot.set(store, change.path, change.value)
      }
```

**Key Points:**
- **WASM orchestrates** - decides when to call listeners, what to pass them
- **JS executes** - runs handler functions, returns produced changes
- **Multiple round trips** - 1 per depth level (typically 2-4)
- **Final result is flat** - all changes (input + sync + flip + listeners + BoolLogic)

---

### 6. Cleanup (Unmount)

```
JS: Component unmounts
  │
  ├─ wasm.unregisterSyncPair(pathId1, pathId2)    WASM removes graph edge
  ├─ wasm.unregisterListener(listenerId)           WASM removes from router
  │   JS: delete handlerMap[listenerId]
  ├─ wasm.unregisterBoolLogic(logicId)             WASM removes from registry + reverse index
  ├─ wasm.unregisterValidator(validatorId)          WASM removes from reverse index
  │   JS: delete schemaMap[validatorId]
  └─ Custom concern effect().dispose()              Standard valtio-reactive cleanup
```

---

## Boundary Crossings Per State Change

| Crossing | Direction | Data | Count |
|---|---|---|---|
| Pipeline call | JS -> WASM | `[(pathId, slotIndex, metaFlags)]` | 1 |
| Pipeline return | WASM -> JS | Final changes + dispatch plans + BoolLogic results | 1 |
| Listener depth dispatch | JS <-> WASM | Changes per depth level | 2-4 round trips |
| Validation batch | JS only | Zod `.safeParse()` calls | 1 (batch, no WASM crossing) |
| Apply to valtio | JS only | `dot.set` on proxy | 1 loop |
| Getter evaluation | JS only | Lazy on React render | 0 (not in pipeline) |

**Total: 4-8 boundary crossings**, regardless of number of fields, concerns, or listeners.

**Current architecture (all JS)**: 0 WASM crossings but N `effect()` re-evaluations, N Proxy trap interceptions, N individual concern evaluations.

---

## Performance Expectations

### By Graph/Pipeline Scale

| Scenario | Current (all JS) | With WASM pipeline | Improvement |
|---|---|---|---|
| 5 changes, 3 sync groups, 0 listeners | ~15us | ~9us | 1.7x |
| 20 changes, 10 sync groups, 5 listeners | ~80us | ~20us | 4x |
| 50 changes, 20 sync groups, 15 listeners | ~300us | ~50us | 6x |
| 100 changes, 50 groups, 30 listeners | ~1.2ms | ~100us | 12x |

### By Concern Evaluation Scale

| Scenario | Current (effect per field) | With WASM BoolLogic | Improvement |
|---|---|---|---|
| 50 fields, 2 BoolLogic concerns each | ~400us | ~25us | 16x |
| 200 fields, 3 BoolLogic concerns each | ~2.4ms | ~80us | 30x |
| 500 fields, 3 BoolLogic + validation | ~8ms | ~200us + validation | 10-20x |

### Where WASM Does NOT Help

| Component | Why it stays in JS | Performance impact |
|---|---|---|
| Zod validation | JS functions, can't serialize | Same speed as today (batched, not per-effect) |
| Custom concerns | Arbitrary user evaluate() | Same speed as today |
| Getter evaluation | JS `this` binding, Proxy tracking | Same speed as today (render-time only) |
| React rendering | DOM, virtual DOM diffing | Unchanged |

---

## Consumer Experience

### Before (current)

```bash
npm install @sladg/apex-state
```

```tsx
import { createGenericStore } from '@sladg/apex-state'
// Just works. No config.
```

### After (with WASM)

```bash
npm install @sladg/apex-state
```

```tsx
import { createGenericStore } from '@sladg/apex-state'
// Just works. No config. WASM is base64-inlined in the JS bundle.
```

**No change for consumers.** WASM binary (~3-8KB) is inlined as base64 in the published JS via `@rollup/plugin-wasm` with `sync: true` during the library build. Consumers never see `.wasm` files, never need plugins, never need Rust.

The only visible difference: Provider may show a brief loading state on first mount (WASM compilation). After first load, the compiled module is cached by the browser.

---

## Build Pipeline

```
Source:
  rust/src/lib.rs              Rust pipeline + graphs + BoolLogic
  src/**/*.ts                  TypeScript (hooks, concerns, types)

Build steps:
  1. wasm-pack build --target bundler    Compile Rust -> .wasm + .js glue + .d.ts
  2. tsup (with @rollup/plugin-wasm)     Bundle TS + inline .wasm as base64
  3. Output: dist/index.js + dist/index.cjs + dist/index.d.ts

CI requirements:
  - Rust toolchain (rustup + wasm-pack)   Adds ~60s to CI builds
  - Node.js (existing)
```

---

## Migration Path

### Phase 1: Foundation (BoolLogic in Rust)

- Set up Rust/wasm-pack/wasm-bindgen toolchain
- Implement string interning table (shared)
- Implement BoolLogic evaluator in Rust
- Implement reverse dependency index
- Replace `effect()` for BoolLogic concerns with Rust evaluation
- Inline WASM in tsup build
- **Validates**: WASM integration, build pipeline, consumer DX

### Phase 2: Shadow State + Pipeline

- Implement shadow state in Rust (getter-free base data)
- Move `processAggregationWrites` to Rust
- Move `processSyncPaths` + sync graph to Rust
- Move `processFlipPaths` + flip graph to Rust
- Move `normalizeChangesForGroups` to Rust
- Implement value slot array for complex values
- **Validates**: Full pipeline in WASM, shadow state synchronization

### Phase 3: Listener Routing

- Move TopicRouter to Rust
- Implement dispatch plan pattern (batch by depth level)
- Move listener seeding + routing to Rust
- Listener handlers remain JS; Rust orchestrates dispatch
- **Validates**: Complete pipeline including listener coordination

### Phase 4: Validation Batching

- Implement validator reverse dependency index in Rust
- Rust produces validation dispatch plans
- JS executes Zod schemas in batch (no per-field effect())
- **Validates**: Full concern evaluation without effect() for declarative concerns

---

## Streaming Data Gateway

The shadow state architecture enables an additional optimization for applications that receive high-frequency external updates (e.g., GraphQL subscriptions, WebSocket feeds, polling). WASM can act as a **change detection gateway** that filters out no-op updates before they enter the pipeline or touch valtio proxies.

### Problem

When external data arrives at high frequency, many updates carry values identical to the current state. Without filtering, each update:

1. Writes to valtio proxy (Proxy set trap)
2. Triggers all `effect()` subscriptions tracking that path
3. Each effect re-evaluates (even though nothing meaningful changed)
4. Concern results are compared, found identical, no write — but the work is done

For a store with 500+ fields and 1000+ concern subscriptions, even a few redundant updates per second compound into significant wasted work.

### Solution

Route incoming data through WASM before it reaches JS state:

```
External data source → JS receives payload → passes to WASM

WASM:
  1. For each field in the update:
     - Resolve path to interned u32 ID
     - Compare value against shadow state
     - Primitive equal → DROP (no-op)
     - Primitive changed → KEEP
     - Complex value (object/array) → KEEP (can't deep-compare cheaply)
  2. Only genuine changes enter the pipeline
  3. Pipeline processes the reduced set
  4. Return minimal changeset to JS

JS:
  applyBatch with only actual changes
  → fewer Proxy set traps
  → fewer React re-renders
```

### API Surface

```typescript
// New export from the library
import { createStreamGateway } from '@sladg/apex-state'

const gateway = createStreamGateway(store)

// In subscription handler:
subscription.onData((payload) => {
  // gateway diffs against shadow state, runs pipeline, applies only real changes
  gateway.ingest(payload)
})

// Cleanup
gateway.dispose()
```

### Why Shadow State Makes This Free

The shadow state (Phase 2) already maintains a getter-free copy of all values keyed by interned path IDs. Diffing a primitive against shadow state is a single comparison on a `ValueRepr` enum — no Proxy traps, no JS object allocation, no GC pressure. WASM can diff hundreds of fields in **< 10µs**.

### Integration with Pipeline

The gateway is not a separate system — it feeds directly into the existing WASM pipeline:

```
gateway.ingest(payload)
  └─ WASM: diff payload against shadow state
       └─ genuine changes enter processChanges pipeline (already in WASM)
            └─ aggregation → sync → flip → listeners → BoolLogic
                 └─ return finalChanges + dispatch plans to JS
                      └─ applyBatch (only real changes touch proxy)
```

This eliminates redundant work at the earliest possible point. Changes that don't survive the diff never trigger Proxy traps, effect evaluations, concern re-calculations, or React re-renders.

### Performance Impact

| Scenario | Without gateway | With gateway | Reduction |
|---|---|---|---|
| 100 fields updated, 10% actually changed | 100 proxy writes + cascading effects | 10 proxy writes | 90% less work |
| 500 fields updated, 30% changed | 500 proxy writes | 150 proxy writes | 70% less work |
| High-frequency feed, stable data | N pipeline runs/sec | 0.1-0.3N pipeline runs/sec | 70-90% fewer pipeline invocations |

The exact reduction depends on data volatility. High-frequency feeds with low entropy (most values unchanged between updates) benefit the most.

### Migration

This is an additive feature that can ship independently after Phase 2 (shadow state). It requires:

- Shadow state in WASM (Phase 2)
- A thin JS wrapper (`createStreamGateway`) that accepts raw payloads and calls `wasm.diffAndProcess`
- No changes to existing store API or consumer code

---

## What Stays Unchanged

- All public API surfaces (`useStore`, `useConcerns`, `withConcerns`, etc.)
- All TypeScript types (`DeepKey`, `DeepValue`, `BoolLogic`, etc.)
- Valtio as the reactivity layer for React
- Zod as the validation library
- Getter support in initial state objects
- Custom concern `evaluate()` functions
- Test suite (behavior is identical, only internals change)
