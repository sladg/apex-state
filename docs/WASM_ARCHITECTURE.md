# WASM Pipeline Architecture

The Rust/WASM layer handles pipeline orchestration, dependency tracking, and declarative evaluation. JavaScript/React handles reactivity, rendering, user-defined functions, and Zod validation.

---

## Design Principles

1. **WASM orchestrates, JS executes** — WASM decides what to evaluate and when; JS runs handlers and applies changes to proxies
2. **Per-store isolation** — Each store gets its own `WasmPipeline` instance via `createWasmPipeline()`
3. **Two-phase processing** — `processChanges()` prepares a plan, JS executes listeners/validators, `pipelineFinalize()` merges results
4. **Minimize boundary crossings** — Batch registration, batch results, dispatch plans instead of per-item callbacks
5. **Zero consumer impact** — WASM ships pre-compiled and base64-inlined; consumers see a normal npm package

---

## Ownership Split

### Lives in WASM (Rust)

| Component | Description |
|---|---|
| **Shadow State** | Getter-free nested tree mirroring state values |
| **String Interning** | Bidirectional path string ↔ u32 ID map (internal) |
| **Sync/Flip Graphs** | Connected components for synchronized/inverted boolean paths |
| **Topic Router** | Listener topic hierarchy with pre-computed dispatch routes |
| **Pipeline Engine** | Aggregation, sync, flip, clear, computation processing |
| **BoolLogic Evaluator** | Declarative boolean expression tree walker |
| **ValueLogic Evaluator** | Conditional value selection (IF/THEN/ELSE, MATCH) |
| **Reverse Dependency Index** | Maps path IDs to affected BoolLogics, ValueLogics, and functions |
| **Function Registry** | Unified registry for validators + listeners (EP6) |
| **ClearPaths Registry** | Trigger → target nullification rules |

### Lives in JS

| Component | Description |
|---|---|
| **Valtio Proxy (`state`)** | Reactive state for React rendering |
| **Valtio Proxy (`_concerns`)** | Computed concern results consumed by React |
| **Listener Handlers** | User-defined JS functions that react to state changes |
| **Zod Schemas** | Validation schema instances (can't cross WASM boundary) |
| **Custom Concern `evaluate()`** | User-defined concern logic using `effect()` |
| **React Hooks** | `useStore`, `useFieldStore`, `useConcerns`, etc. |
| **`applyBatch`** | Writes final changes to valtio proxy |

### What Crosses the Boundary

- **JS → WASM**: String paths, JSON-serialized values, registration configs
- **WASM → JS**: Change plans, execution plans, validator dispatch lists
- String paths cross as strings. Values cross as JSON. WASM never touches valtio, React, or Zod.

---

## Pipeline Instance Lifecycle

Each store gets an isolated WASM pipeline via `createWasmPipeline()`:

```typescript
// src/wasm/bridge.ts exports
export interface WasmPipeline {
  readonly id: number
  shadowInit: (state: object) => void
  processChanges: (changes: Change[]) => ProcessChangesResult
  pipelineFinalize: (jsChanges: Change[]) => { state_changes: Change[] }
  registerSideEffects: (reg: SideEffectsRegistration) => SideEffectsResult
  unregisterSideEffects: (registrationId: string) => void
  registerConcerns: (reg: ConcernsRegistration) => ConcernsResult
  unregisterConcerns: (registrationId: string) => void
  registerBoolLogic: (outputPath: string, tree: unknown) => number
  unregisterBoolLogic: (logicId: number) => void
  pipelineReset: () => void
  destroy: () => void
  validatorSchemas: Map<number, ValidationSchema>
}
```

Lifecycle:

1. Provider mounts → `createWasmPipeline()` → stores pipeline instance
2. `pipeline.shadowInit(state)` — initializes shadow state tree
3. Registration calls (side effects, concerns) — configures pipeline
4. State changes → `pipeline.processChanges()` / `pipeline.pipelineFinalize()`
5. Provider unmounts → `pipeline.destroy()`

---

## Consolidated Registration

### Side Effects

Single call registers all side effect types:

```typescript
interface SideEffectsRegistration {
  registration_id: string
  sync_pairs?: [source: string, target: string][]           // bidirectional sync
  directed_sync_pairs?: [source: string, target: string][]  // one-way sync (source → target)
  flip_pairs?: [source: string, target: string][]
  aggregation_pairs?: [target: string, source: string, condition?: string][]
  computation_pairs?: [op: string, target: string, source: string, condition?: string][]
  clear_paths?: { triggers: string[]; targets: string[] }[]
  listeners?: { subscriber_id: number; topic_path: string; scope_path: string }[]
}

interface SideEffectsResult {
  sync_changes: Change[]
  aggregation_changes: Change[]
  computation_changes: Change[]
  registered_listener_ids: number[]
}
```

### Concerns

Single call registers all declarative concerns:

```typescript
interface ConcernsRegistration {
  registration_id: string
  bool_logics?: { output_path: string; tree_json: string }[]
  validators?: { validator_id: number; output_path: string; dependency_paths: string[]; scope: string }[]
  value_logics?: { output_path: string; tree_json: string }[]
}

interface ConcernsResult {
  bool_logic_changes: Change[]
  registered_logic_ids: number[]
  registered_validator_ids: number[]
  value_logic_changes: Change[]
  registered_value_logic_ids: number[]
}
```

### Concern Classification (TypeScript Layer)

`src/concerns/registration.wasm-impl.ts` classifies each concern config and routes to the right registration:

| Config shape | Where it goes | Evaluated by |
|---|---|---|
| `{ boolLogic: {...} }` | `registerConcerns()` → BoolLogic | WASM (static) |
| `{ value_logic: {...} }` | `registerConcerns()` → ValueLogic | WASM (static) |
| `{ schema: ZodSchema }` | `registerConcerns()` → validators | JS (Zod), dispatched by WASM |
| Custom `evaluate()` function | `effect()` in JS | JS (valtio-reactive) |

---

## Two-Phase Processing

### Phase 1: `processChanges(changes)` → `ProcessChangesResult`

WASM runs the full pipeline internally and returns a plan:

```typescript
interface ProcessChangesResult {
  state_changes: Change[]         // Changes to apply to state proxy
  validators_to_run: ValidatorDispatch[]  // Validators JS needs to execute
  execution_plan: FullExecutionPlan | null  // Listener dispatch plan
  has_work: boolean               // Whether JS has listeners/validators to execute
}
```

### JS Executes Work

If `has_work` is true, JS:

1. Executes listener handlers per the execution plan (depth-ordered)
2. Runs Zod validators per the validator dispatch list
3. Collects all produced changes

### Phase 2: `pipelineFinalize(jsChanges)` → `{ state_changes }`

JS sends back all changes produced by listeners and validators. WASM:

1. Partitions changes by `_concerns.` prefix
2. Merges concern changes with buffered BoolLogic/ValueLogic results
3. Diffs all changes against shadow state (filters no-ops)
4. Updates shadow state
5. Returns final `state_changes`

---

## Pipeline Processing Order

Within `processChanges()`, WASM runs these steps:

```
1. Diff incoming changes against shadow state (filter no-ops)
2. Update shadow state
3. Process aggregation writes (target → sources distribution)
4. Process aggregation reads (compute target values from sources)
5. Process sync pairs (bidirectional sync via connected components)
6. Process flip pairs (inverted boolean sync)
7. Process clear-path rules (trigger paths → null target paths)
8. Process computation (SUM, AVG reductions)
9. Identify affected BoolLogics/ValueLogics/validators via reverse index
10. Evaluate affected BoolLogics and ValueLogics
11. Build listener execution plan (depth-ordered dispatch groups)
12. Return: state_changes + validators_to_run + execution_plan
```

Within `pipelineFinalize()`:

```
1. Partition JS-produced changes (state vs _concerns prefix)
2. Merge _concerns changes with buffered BoolLogic/ValueLogic results
3. Diff all changes against shadow state (filter no-ops)
4. Update shadow state for both state and _concerns paths
5. Return: final state_changes
```

---

## Shadow State

Nested tree structure mirroring the valtio state:

```rust
pub(crate) enum ValueRepr {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<ValueRepr>),
    Object(HashMap<String, ValueRepr>),
}

pub(crate) struct ShadowState {
    root: ValueRepr,
}
```

Key properties:

- Initialized once via `shadowInit()`, then kept in sync automatically by the pipeline
- There is NO separate `shadowSet()` call
- Supports updates at any level: leaf values, nested objects, or entire subtrees
- Used for no-op filtering (diff incoming against current shadow)
- Used by BoolLogic/ValueLogic evaluators to read state values

---

## BoolLogic

Declarative boolean expressions evaluated entirely in WASM:

```rust
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub(crate) enum BoolLogicNode {
    IsEqual(String, Value),
    Exists(String),
    IsEmpty(String),
    And(Vec<BoolLogicNode>),
    Or(Vec<BoolLogicNode>),
    Not(Box<BoolLogicNode>),
    Gt(String, f64),
    Lt(String, f64),
    Gte(String, f64),
    Lte(String, f64),
    In(String, Vec<Value>),
    ContainsAny(String, Vec<Value>),  // array contains at least one of the given elements
    ContainsAll(String, Vec<Value>),  // array contains all of the given elements
}
```

Used by concerns: `disabledWhen`, `visibleWhen`, `readonlyWhen`. Input paths are extracted at registration and added to the reverse dependency index.

---

## Listener Execution Plan

Listeners are dispatched in depth-ordered groups:

```typescript
interface FullExecutionPlan {
  groups: DispatchGroup[]
  propagation_map: PropagationTarget[][] // Pre-computed routing for produced changes
}

interface DispatchGroup {
  dispatches: DispatchEntry[]
}

interface DispatchEntry {
  dispatch_id: number
  subscriber_id: number    // Maps to JS handler function
  scope_path: string       // For resolving scoped state
  input_change_ids: number[] // Indexes into state_changes array
}
```

The propagation map enables routing listener-produced changes to parent dispatches without additional WASM boundary crossings.

---

## Two-Proxy Pattern

The store uses two independent valtio proxies:

```typescript
interface StoreInstance {
  state: DATA              // Application state (user data)
  _concerns: ConcernValues // Computed concern results
}
```

**Why separate?**

- Independent tracking: React subscribes to `state` OR `_concerns` independently
- Read/write separation: Concerns READ from `state`, WRITE to `_concerns` (prevents infinite loops)
- WASM returns a unified change stream; JS partitions by `_concerns.` prefix and routes to the correct proxy

---

## WASM Exports (Rust)

Actual `#[wasm_bindgen]` exports in `rust/src/lib.rs`:

| Export | Description |
|---|---|
| `pipeline_create()` | Create isolated pipeline instance, returns ID |
| `pipeline_destroy(id)` | Destroy pipeline and free resources |
| `shadow_init(id, state)` | Initialize shadow state tree |
| `shadow_dump(id)` | Dump shadow state as JSON (debug only) |
| `process_changes(id, changes)` | Run pipeline, return changes + execution plan |
| `pipeline_finalize(id, js_changes)` | Merge JS results, return final changes |
| `register_side_effects(id, json)` | Register sync/flip/aggregation/computation/clear/listeners |
| `unregister_side_effects(id, reg_id)` | Remove side effects by registration ID |
| `register_concerns(id, json)` | Register BoolLogic/validators/ValueLogic |
| `unregister_concerns(id, reg_id)` | Remove concerns by registration ID |
| `register_boollogic(id, output_path, tree_json)` | Register single BoolLogic expression |
| `unregister_boollogic(id, logic_id)` | Remove single BoolLogic |
| `pipeline_reset(id)` | Reset pipeline state |
| `pipeline_reset_all()` | Reset all pipelines |

String interning is internal to WASM — not exposed via public API.

---

## Data Flow: One State Change

```
User calls: setValue("user.email", "alice@example.com")

JS Layer:
  ├─ Queue change: { path: "user.email", value: "alice@example.com" }
  └─ Call: pipeline.processChanges([...])

  ┌──────────────────────────────────────────────────────┐
  │          WASM: processChanges()                      │
  ├──────────────────────────────────────────────────────┤
  │ 1. Diff against shadow state (filter no-ops)         │
  │ 2. Update shadow state                               │
  │ 3-8. Process side effects (sync/flip/clear/compute)  │
  │ 9. Find affected BoolLogics/ValueLogics (rev index)  │
  │ 10. Evaluate affected expressions                    │
  │ 11. Build listener execution plan                    │
  │ 12. Return: state_changes + execution_plan           │
  └──────────────────────────────────────────────────────┘

JS Layer (continued):
  ├─ If has_work:
  │   ├─ Execute listener handlers per execution plan
  │   ├─ Run Zod validators per dispatch list
  │   ├─ Collect produced changes
  │   └─ Call: pipeline.pipelineFinalize(producedChanges)
  │
  │     ┌──────────────────────────────────────────────┐
  │     │ WASM: pipelineFinalize()                     │
  │     ├──────────────────────────────────────────────┤
  │     │ Merge JS changes with buffered BoolLogic     │
  │     │ Diff against shadow, update shadow           │
  │     │ Return: final state_changes                  │
  │     └──────────────────────────────────────────────┘
  │
  ├─ Partition final changes by _concerns. prefix
  ├─ Apply state changes to store.state proxy
  ├─ Apply concern changes to store._concerns proxy
  └─ React re-renders via valtio subscription
```

---

## Side Effect Types

| Type | Config | Processing |
|---|---|---|
| **Sync pairs** | `[pathA, pathB]` | Connected component graph; changing one syncs all |
| **Flip pairs** | `[pathA, pathB]` | Inverted boolean sync |
| **Aggregation** | `[target, source, condition?]` | Multi-source convergence; target = common value or undefined |
| **Computation** | `[SUM\|AVG, target, source, condition?]` | Numeric reduction across sources |
| **ClearPaths** | `{ triggers, targets }` | When trigger paths change, set target paths to null |
| **Listeners** | `{ subscriber_id, topic_path, scope_path }` | Depth-ordered dispatch with scoped state |

---

## Build Pipeline

```
1. npm run wasm:build         Compile rust/ → .wasm + JS glue + .d.ts
2. npm run build              Bundle src/ + inline .wasm as base64
3. Output: dist/index.js + dist/index.d.ts

Consumer: npm install @sladg/apex-state → just works, no WASM config needed
```

---

## Key Source Files

### Rust (`rust/src/`)

| File | What it does |
|---|---|
| `lib.rs` | WASM entry point, all `#[wasm_bindgen]` exports |
| `pipeline.rs` | Main `processChanges` / `pipelineFinalize` orchestration |
| `shadow.rs` | Shadow state tree, path traversal, ValueRepr |
| `intern.rs` | String ↔ u32 bidirectional interning |
| `bool_logic.rs` | BoolLogic tree evaluation |
| `value_logic.rs` | ValueLogic IF/THEN/ELSE and MATCH evaluation |
| `graphs.rs` | Sync/flip graph structures (connected components) |
| `router.rs` | Topic router for listener dispatch |
| `functions.rs` | Unified function registry (validators + listeners) |
| `aggregation.rs` | Aggregation write/read processing |
| `computation.rs` | SUM/AVG computation processing |
| `clear_paths.rs` | ClearPaths trigger/target processing |

### TypeScript (`src/`)

| File | What it does |
|---|---|
| `wasm/bridge.ts` | `createWasmPipeline()`, all WASM types |
| `wasm/lifecycle.ts` | WASM loading and initialization |
| `concerns/registration.wasm-impl.ts` | Concern classification and WASM registration |
| `pipeline/process-changes.wasm-impl.ts` | Two-phase change processing orchestration |
| `store/create-store.ts` | Store factory, React hooks |
