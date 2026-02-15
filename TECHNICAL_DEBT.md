# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

### WASM-032: Missing Test Coverage (Critical)

- **[Rust/Tests]** Zero integration tests for `process_changes_phase1()` + `pipeline_finalize()` round-trip refactor. All 40+ existing tests still use old `process_changes_vec()` returning `ProcessResult`. `rust/src/pipeline.rs:617-818`
- **[Rust/Tests]** No tests for finalize edge cases: empty validators/listeners, multiple finalize calls without phase 1, `_concerns.*` path routing to concern bucket. `rust/src/pipeline.rs:761-818`
- **[Rust/Tests]** No verification that concern paths have `_concerns.` prefix stripped in finalize output. `rust/src/pipeline.rs:790-802`
- **[Rust/Tests]** No round-trip behavioral tests: BoolLogic evaluation → finalize → concern_changes applied, validator results → finalize → concern_changes applied. `rust/src/pipeline.rs:617-818`
- **[Rust/Legacy API]** `ProcessResult` marked "deprecated" (line 34) but still actively used by all tests and returned by `process_changes_vec()`. No migration path documented. `rust/src/pipeline.rs:36-47, 420`

### Code Quality (from npm run code:check)

- **[TypeScript]** `executeFullExecutionPlan()` has Cognitive Complexity 27 (limit: 20). Refactor to reduce nesting/branching. `src/pipeline/processChanges.ts:47-125`
- **[TypeScript]** Generated WASM glue code has Cognitive Complexity 23 and forbidden `require()` import. Consider build tool configuration. `rust/pkg-node/apex_state_wasm.js:616,800`
- **[Tests]** Integration tests use destructured `render()` instead of `screen.getByTestId()` (8 violations). `tests/integration/deeply-nested-pipeline.test.tsx`

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
