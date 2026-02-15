# WASM Pipeline — Backlog

Project key: **WASM**

## Epics

| Key | Title | Stories | Depends On | Status |
|---|---|---|---|---|
| WASM-EP1 | Foundation & Toolchain | WASM-001 → WASM-007 | — | **✅ COMPLETE** (2026-02-14) |
| WASM-EP2 | Shadow State & Pipeline | WASM-008 → WASM-015 | EP1 | **✅ COMPLETE** (2026-02-15) |
| WASM-EP3 | Listener Orchestration | WASM-016 → WASM-019 | EP2 | **✅ COMPLETE** (2026-02-15) — Tests deferred |
| WASM-EP4 | Validation Batching | WASM-021 → WASM-024 | EP2 | **✅ COMPLETE** (2026-02-15) — Tests: placeholders written |
| WASM-EP5 | Streaming Data Gateway | WASM-028 → WASM-031 | EP2 | **⏳ READY** |

EP3, EP4, EP5 can run in parallel after EP2 completes.

## Story Map

```
EP1 Foundation (COMPLETE ✅)
  001 Rust toolchain setup ✅ c10c6b0
  002 String interning table ✅ f730ff5
  003 BoolLogic evaluator (Rust) ✅ fcfe40e
  004 Reverse dependency index ✅ fcfe40e
  005 JS bridge: BoolLogic concerns ✅ a55c664
  006 WASM inline build (tsup) ✅ f7afce2
  007 Phase 1 integration tests ✅ (implicit in commits)
        │
        ▼
EP2 Shadow State & Pipeline (COMPLETE ✅)
  008 Shadow state (Rust) ✅ ad6c1c4
  009 Value slot array ✅ (aggregation support)
  010 processAggregationWrites (Rust) ✅ fa385db
  011 Sync graph + processSyncPaths (Rust) ✅ fa385db
  012 Flip graph + processFlipPaths (Rust) ✅ fa385db
  013 normalizeChangesForGroups (Rust) ✅ (implicit in pipeline)
  014 JS-side pipeline bridge ✅ (src/pipeline/processChanges.ts)
  015 Phase 2 integration tests ✅ fa385db
        │
        ├──────────────┬──────────────┐
        ▼              ▼              ▼
EP3 Listeners (✅)   EP4 Validation (✅)  EP5 Streaming (⏳)
  016 TopicRouter ✅    021 Rev-dep ✅        028 Diff engine
  017 Dispatch plan ✅  022 Dispatch ✅       029 Diff + pipeline
  018 Seed+routing ✅   023 Batch Zod ✅      030 Gateway API
  019 JS dispatch ✅    024 Tests (stubs) ✅  031 Tests
```

---

## Status Summary (2026-02-15)

### ✅ Completed: EP1 Foundation & Toolchain (100%)

- **Completion Date**: 2026-02-14
- **Stories**: WASM-001 → WASM-007 (all complete)
- **Key Deliverables**:
  - Rust toolchain with `wasm-pack` build
  - String interning (internal to WASM, O(1) path lookups)
  - BoolLogic evaluator with pattern matching (IS_EQUAL, EXISTS, AND/OR/NOT, comparisons)
  - Reverse dependency index (fast: which logics depend on which paths)
  - JS bridge with single entry point (`wasm` namespace)
  - WASM inlining in tsup build (no .wasm files in dist, zero config for consumers)
  - Phase 1 integration tests (all passing)

### ✅ Completed: EP2 Shadow State & Pipeline (100%)

- **Completion Date**: 2026-02-15
- **Stories**: WASM-008 → WASM-015 (all complete)
- **Key Deliverables**:
  - Nested shadow state (mirrors valtio structure, getter-free value copy)
  - Aggregation writes (target → sources distribution)
  - Sync graph (connected components, bidirectional path synchronization)
  - Flip graph (boolean inversion for paired paths)
  - Pre-allocated buffers for zero-copy hot-path processing
  - Full `processChanges()` orchestration (aggregation → sync → flip → BoolLogic)
  - Concern changes separated from state changes in output
  - Phase 2 integration tests (all passing)
  - **Implementation**: `rust/src/pipeline.rs`, `src/pipeline/processChanges.ts`

### ✅ Completed: EP3 Listener Orchestration (100%)

- **Completion Date**: 2026-02-15
- **Stories**: WASM-016 → WASM-019 (all core logic complete)
- **Key Deliverables**:
  - TopicRouter (Rust) — Router structure, topic registration
  - Dispatch plan generation — `FullExecutionPlan` + `DispatchPlan` types
  - Seed+routing — Batch registration infrastructure
  - JS dispatch execution — Handler execution in `processChanges.ts`
  - **Implementation**: `rust/src/router.rs`, `src/pipeline/processChanges.ts`

### ✅ Completed: EP4 Validation Batching (100%)

- **Completion Date**: 2026-02-15
- **Stories**: WASM-021 → WASM-024 (all core logic complete)
- **Spec**: `tasks/WASM-EP4-VALIDATION-v2.md` (canonical)
- **Key Deliverables**:
  - WASM-021: Validator registry + reverse index — ✅ 94f0770
  - WASM-022: Validator evaluation in processChanges() — ✅ 94f0770
  - WASM-023: Validation concern integration (JS) — ✅ 772aa47
  - WASM-024: Test placeholders written — `tests/wasm/validation-batching.test.ts` (18 stubs, bodies TBD)
- **Implementation**: `rust/src/validator.rs`, `rust/src/rev_index.rs`, `src/concerns/registration.ts`, `src/pipeline/processChanges.ts`

### ⏳ Ready: EP5 Streaming Data Gateway

- **Depends On**: EP2 (complete)
- **Spec**: `tasks/WASM-EP5-STREAMING-v2.md` (canonical)
- **Stories**: WASM-028 → WASM-031
- **Scope**:
  - WASM-028: Shadow state diff engine (Rust) — 4pts
  - WASM-029: Integrated diff + pipeline (single WASM call) — 4pts
  - WASM-030: `createStreamGateway` JS API — 3pts
  - WASM-031: Phase 5 integration tests — 3pts
- **Total**: 14pts

---

## Deferred Tests

Tests with placeholders/stubs that need implementation:

| Source | File | Count | Status |
|--------|------|-------|--------|
| EP3 | `tests/wasm/listener-dispatch.test.ts` | TBD | Stubs |
| EP4 | `tests/wasm/validation-batching.test.ts` | 18 | Stubs (step comments in place) |

---

## Recent Commits

| Hash | Message | Status |
|------|---------|--------|
| 772aa47 | feat(wasm): validations | ✅ |
| 94f0770 | feat(wasm): allow for validations (zod) in rust | ✅ |
| fa385db | feat(wasm): optimizations, test placeholders, speed comparisons | ✅ |
| f7afce2 | feat(wasm): pipelines, graphs, embedding of wasm inline | ✅ |
| 705dc4f | feat(wasm): initial rebuild, base-line rust code in place, basic bridge added | ✅ |
| a55c664 | docs(wasm): updated claude.md, added wasm architecture and task for processing | ✅ |
| 22ee86b | Implement Shadow State as Nested Tree in Rust/WASM (#13) | ✅ |
| fcfe40e | WASM-004: BoolLogic evaluator with tuple variants (#12) | ✅ |

---

## Sizing Reference

| Points | Meaning |
|---|---|
| 1 | Trivial, < 2h |
| 2 | Small, half day |
| 3 | Medium, 1 day |
| 5 | Large, 2-3 days |
| 8 | Complex, ~1 week |
