# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

### WASM-032: Missing Test Coverage (Critical)

- **[Rust/Tests]** Zero integration tests for `prepare_changes()` + `pipeline_finalize()` round-trip refactor. All 40+ existing tests still use old `process_changes_vec()` returning `ProcessResult`. `rust/src/pipeline.rs:617-818`
- **[Rust/Tests]** No tests for finalize edge cases: empty validators/listeners, multiple finalize calls without prepare, `_concerns.*` path routing to concern bucket. `rust/src/pipeline.rs:761-818`
- **[Rust/Tests]** No verification that concern paths have `_concerns.` prefix stripped in finalize output. `rust/src/pipeline.rs:790-802`
- **[Rust/Tests]** No round-trip behavioral tests: BoolLogic evaluation → finalize → concern_changes applied, validator results → finalize → concern_changes applied. `rust/src/pipeline.rs:617-818`
- **[Rust/Legacy API]** `ProcessResult` marked "deprecated" (line 34) but still actively used by all tests and returned by `process_changes_vec()`. No migration path documented. `rust/src/pipeline.rs:36-47, 420`

### Code Quality (from npm run code:check)

- **[TypeScript]** `executeFullExecutionPlan()` has Cognitive Complexity 27 (limit: 20). Refactor to reduce nesting/branching. `src/pipeline/processChanges.ts:47-125`
- **[TypeScript]** Generated WASM glue code has Cognitive Complexity 23 and forbidden `require()` import. Consider build tool configuration. `rust/pkg-node/apex_state_wasm.js:616,800`
- **[Tests]** Integration tests use destructured `render()` instead of `screen.getByTestId()` (8 violations). `tests/integration/deeply-nested-pipeline.test.tsx`

### Rust Cleanup

- **[Rust]** Remove `#[allow(dead_code)]` annotations — audit usages and either wire up the dead code or delete it. Grep for all occurrences across `rust/src/`.

### WASM Single-Pipeline Limitation (Critical Architecture)

- **[WASM/Architecture]** Single global `thread_local! PIPELINE` in Rust means only ONE WASM-mode store can exist at a time. `shadowInit()` overwrites the entire shadow state — a second Provider mount clobbers the first. Multiple concurrent WASM stores are NOT supported. `rust/src/lib.rs:30-32`, `src/store/Provider.tsx:101`
- **[WASM/Architecture]** No store ID or namespace in any WASM API call. All registrations (boollogic, listeners, validators, sync/flip graphs) go into the single global pipeline. Multi-store would require per-store pipeline instances or namespaced paths.

### concern_changes Removal Fallout

- **[Rust/Tests]** `ProcessResult.concern_changes` field was removed (all changes now in single `changes` vec), but 26 tests still reference it. Being fixed. `rust/src/pipeline.rs`
- **[Rust/Tests]** `FinalizeResult` also lost `concern_changes` — concern changes now keep `_concerns.` prefix in `state_changes`. Tests and docs referencing the old split need updating. `rust/src/pipeline.rs:68-71`
- **[JS/Tests]** Some WASM tests referenced `result.concern_changes` — fixed in validation-batching.test.ts. Pattern: filter `state_changes` by `_concerns.` prefix.

### WASM Bridge Sentinel Collision Risk

- **[JS/WASM boundary]** `wasmChangesToJs` uses raw string `"undefined"` as sentinel for JS `undefined`. If a user sets a field to the literal string `"undefined"` or `"null"`, it would be misinterpreted. Should use escaped sentinels (e.g., `@undefined`, `@null`) with a proper encode/decode protocol at the boundary. `src/wasm/bridge.ts:238`, `rust/src/aggregation.rs:268`

### Other

- **[JS]** Old WASM test files (`tests/wasm/bool_logic.test.ts`, `tests/wasm/shadow.test.ts`) import removed APIs (`evaluateBoolLogic`, `initShadowState`, `getShadowValue`, `updateShadowValue`, `dumpShadowState`). Rewrite to use current `registerBoolLogic()` + `processChanges()` API.
- **[JS]** JS BoolLogic evaluator (`src/utils/boolLogic.ts`) is redundant (WASM handles evaluation), but keeping for now as reference.

---

## Format

Each item follows this pattern:

```
- **[Component/Layer]** Brief description. `file.ts:line`
```

**Examples:**

- **[WASM]** Shadow state diffing could use deeper comparison for arrays. `rust/src/shadow.rs:42`
- **[JS/WASM boundary]** Multiple roundtrips for listener dispatch could be batch-optimized. `src/wasm/bridge.ts:105`
- **[Types]** Generic constraint on DeepKey could be tightened for better inference. `src/types/deepKey.ts:8`
- **[JS]** BoolLogic evaluation in JS (Phase 1) becomes redundant after WASM migration. `src/utils/boolLogic.ts:1`

---

## Completed Items

- **[WASM Architecture]** `extractBoolLogicInputPaths` moved to Rust — RESOLVED. Function no longer needed; registration now directly passes `config.condition` to `registerBoolLogic()`.
- **[JS/WASM boundary]** Concern results from `processChanges()` applied to `_concerns` — RESOLVED. Implementation at `src/pipeline/processChanges.ts:189-193`.
- **[JS/Reactivity]** Valtio snapshot timing issue — RESOLVED. Pre-initialization pattern in `registration.ts:34-40` + `useLayoutEffect` ordering prevents race condition.
- **[JS/Concerns]** Concern integration deferred — RESOLVED. Both WASM BoolLogic and JS custom concerns fully integrated in `registerConcernEffects()`.
- **[JS]** Duplicate debt item about old test files — REMOVED. Consolidated into single pending item.
