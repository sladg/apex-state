# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

- **[WASM Architecture]** Move `extractBoolLogicInputPaths` from TypeScript to Rust. Currently in `src/concerns/registration.ts:21-69`, this function should be implemented in Rust/WASM so WASM constructs listener registrations from BoolLogic config directly, reducing JS→WASM crossings. User feedback: "wasm should construct the listeners based on BoolLogic config provided".
- **[JS/WASM boundary]** Store integration with WASM `processChanges` — callers (`_useFieldValue`, `useJitStore`) need to apply BoolLogic results from `processChanges()` output back to `_concerns` proxy. `src/concerns/registration.ts:68`
- **[JS]** Old WASM test files (`tests/wasm/interning.test.ts`, `tests/wasm/bool_logic.test.ts`, `tests/wasm/shadow.test.ts`, `tests/wasm/interning.bench.ts`) expect removed APIs (`evaluateBoolLogic`, `initShadowState`). Rewrite to match current `processChanges`/`registerBoolLogic` design.
- **[JS]** JS BoolLogic evaluator (`src/utils/boolLogic.ts`) is redundant now that WASM handles evaluation. Can be removed after full integration.

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

(Moved here after user approval and processing.)
