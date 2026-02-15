# Technical Debt

Opportunities for improvement tracked during development. Reviewed and prioritized by user before processing.

## Pending Items

(None tracked yet. As work progresses, issues will be added here with location references.)

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
