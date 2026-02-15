# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

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
