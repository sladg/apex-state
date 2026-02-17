# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

### WASM-032: Legacy API Migration (Low Priority)

- **[Rust/Legacy API]** `ProcessResult` / `process_changes_vec()` still used by 40+ existing tests. Migration to `prepare_changes` + `pipeline_finalize` deferred. `rust/src/pipeline.rs:44-54`


### Code Quality (from npm run code:check)

- ~~**[TypeScript]** `executeFullExecutionPlan()` Cognitive Complexity 27~~ — RESOLVED. Logic extracted into `buildDispatchInput`, `propagateChanges`, `remapPath`, `partitionChanges`. `eslint` passes clean.
- **[TypeScript]** Generated WASM glue code has Cognitive Complexity 23 and forbidden `require()` import. Consider build tool configuration. `rust/pkg-node/apex_state_wasm.js:616,800`
- ~~**[Tests]** Integration tests use destructured `render()` instead of `screen.getByTestId()`~~ — RESOLVED. The referenced file was replaced by v2 tests that use `screen` throughout.

### Rust Cleanup

- ~~**[Rust]** Remove `#[allow(dead_code)]` annotations~~ — RESOLVED. See completed items.

### concern_changes Removal Fallout

- ~~**[Rust/Tests]** concern_changes references~~ — RESOLVED. Tests use `get_concern_changes()` helper that filters unified `changes` vec by `_concerns.` prefix. Correct pattern.

### Concern Type Safety

- **[Types/Concerns]** Concern types in tests and prebuilts are not properly generic — some use `BoolLogic<any>` or untyped config objects as workarounds. All concern registrations (tests + prebuilts) should use fully typed generics tied to the store's state type, eliminating `any` casts. `src/concerns/prebuilts/`, `tests/integration/`, `tests/integrations_v2/`

### Other

- ~~**[JS]** Old WASM test files~~ — RESOLVED. Files no longer exist.
- ~~**[JS]** JS BoolLogic evaluator redundant~~ — INACCURATE. `evaluateBoolLogic` is actively used by legacy JS path (`registration.ts`) and 3 prebuilt concerns. Also publicly exported from `src/index.ts`.
- ~~**[JS/Valtio]** Getter dependency tracking concern~~ — INACCURATE. `attachComputedGetters()` uses valtio-reactive `computed()` which provides exact selective tracking. Tests in `tests/store/derived.test.tsx` confirm unrelated mutations don't trigger re-renders.

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

- **[JS/WASM boundary]** Sentinel collision risk — RESOLVED. `createFastJson` with `__APEX_UNDEFINED__` sentinel; isolated from user data. `src/utils/json.ts`, `src/wasm/bridge.ts`
- **[JS/WASM boundary]** JSON serialization fast paths — RESOLVED. `createFastJson` bypasses JSON for primitives (~4.7x faster). `src/utils/json.ts`
- **[WASM Architecture]** Single global `thread_local! PIPELINE` replaced with `PIPELINES: HashMap<u32, ProcessingPipeline>` — RESOLVED. Each store now gets its own isolated pipeline via `createWasmPipeline()`. Multiple concurrent Providers no longer interfere. `rust/src/lib.rs`, `src/wasm/bridge.ts`

- **[WASM Architecture]** `extractBoolLogicInputPaths` moved to Rust — RESOLVED. Function no longer needed; registration now directly passes `config.condition` to `registerBoolLogic()`.
- **[JS/WASM boundary]** Concern results from `processChanges()` applied to `_concerns` — RESOLVED. Implementation at `src/pipeline/processChanges.ts:189-193`.
- **[JS/Reactivity]** Valtio snapshot timing issue — RESOLVED. Pre-initialization pattern in `registration.ts:34-40` + `useLayoutEffect` ordering prevents race condition.
- **[JS/Concerns]** Concern integration deferred — RESOLVED. Both WASM BoolLogic and JS custom concerns fully integrated in `registerConcernEffects()`.
- **[JS]** Duplicate debt item about old test files — REMOVED. Consolidated into single pending item.
- **[Rust]** `#[allow(dead_code)]` audit — RESOLVED. Test-only methods marked `#[cfg(test)]`, unused deserialized fields removed, WASM-chain methods annotated with explanatory comments. All 13 clippy dead_code warnings eliminated.
- **[JS/Valtio]** `extractGetters`/`detectGetters` "dead code" — INACCURATE. Both are actively used: `extractGetters` via `prepareInitialState()` in Provider, `detectGetters` as test utility.
- **[JS]** Old WASM test files (`tests/wasm/`) — RESOLVED. Files already deleted.
- **[JS]** JS BoolLogic evaluator "redundant" — INACCURATE. Used by legacy JS path and 3 prebuilt concerns, publicly exported.
- **[Rust/Tests]** concern_changes removal fallout — RESOLVED. Tests correctly use `get_concern_changes()` filter on unified `changes` vec.
