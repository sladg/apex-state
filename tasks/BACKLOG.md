# WASM Pipeline — Backlog

Project key: **WASM**

## Epics

| Key | Title | Stories | Depends On | Status |
|---|---|---|---|---|
| WASM-EP1 | Foundation & Toolchain | WASM-001 → WASM-007 | — | **✅ COMPLETE** (2026-02-14) |
| WASM-EP2 | Shadow State & Pipeline | WASM-008 → WASM-015 | EP1 | **✅ COMPLETE** (2026-02-15) |
| WASM-EP3 | Listener Orchestration | WASM-016 → WASM-019 | EP2 | **✅ COMPLETE** (2026-02-15) — Tests deferred |
| WASM-EP4 | Validation Batching | WASM-021 → WASM-024 | EP2 | **✅ COMPLETE** (2026-02-15) — Tests: placeholders written |
| ~~WASM-EP5~~ | ~~Streaming Data Gateway~~ | ~~WASM-028 → WASM-031~~ | ~~EP2~~ | **🚫 SUPERSEDED** — Absorbed by EP6; no-op filtering in every pipeline step eliminates the need for a separate streaming gateway |
| WASM-EP6 | Pipeline Refactor | WASM-032 | EP4 | **✅ COMPLETE** (2026-02-15) |
| WASM-EP7 | Clean Mode Split | WASM-033 → WASM-037 | EP6 | **✅ COMPLETE** (2026-03-03) — JS was already WASM-only; `path-groups.ts` + `graph-types.ts` dead code removed |
| WASM-EP8 | Recency-Based Sync | WASM-038 → WASM-040 | EP6 | **⏳ READY** |
| WASM-EP15 | Bundle Size Reduction | Story 1 (dual build) + Story 2 (Rust flags) | — | **⏳ READY** |
| WASM-EP11 | Rust Performance: Huge Objects | WASM-041 → WASM-044 | EP6 | **🔄 IN PROGRESS** (2/4 stories done) |
| WASM-EP12 | Rust Performance: Many Pipelines | WASM-045 → WASM-048 | EP6 | **✅ COMPLETE** (2026-03-03) |
| WASM-EP13 | Rust Deduplication & Abstraction | WASM-049 → WASM-054 | EP6 | **✅ COMPLETE** (2026-03-03) |
| WASM-EP14 | Rust Third-Party Crate Adoption | WASM-055 → WASM-060 | EP6 | **✅ COMPLETE** (2026-03-03) |
| WASM-EP9 | Pipeline Context Refactor | — | EP6 | **✅ COMPLETE** — Implemented as part of EP6 refactor |
| WASM-EP10 | Anchor Path + Multi-path Listeners | — | EP6 | **✅ COMPLETE** (2026-03-03) — anchorPath guards, multi-path `topic_paths[]`, dispatch ordering |

EP3, EP4 ran in parallel after EP2. EP6 followed EP4. EP7–EP10 complete.

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
        ├──────────────┐
        ▼              ▼
EP3 Listeners (✅)   EP4 Validation (✅)
  016 TopicRouter ✅    021 Rev-dep ✅
  017 Dispatch plan ✅  022 Dispatch ✅
  018 Seed+routing ✅   023 Batch Zod ✅
  019 JS dispatch ✅    024 Tests (stubs) ✅
                              │
                              ▼
                       EP6 Pipeline Refactor (✅)
                         032 Round-trip refactor ✅ 22199cf
                              │
                              ▼
                       EP7 Clean Mode Split (✅)
                         033–037 Complete — codebase is WASM-only;
                                 path-groups.ts + graph-types.ts removed
                              │
                              ▼
                       EP8 Recency-Based Sync (⏳)
                         038 Add recency tracking infrastructure
                         039 Update sync registration logic
                         040 Integration tests
                              │
                              ▼
                       EP11 Rust Perf: Huge Objects (🔄 2/4)
                         041 Remove subtree clone in shadow.set() ✅ 2026-03-04
                         042 affected_paths() → Vec<u32> (intern-first) ✅ 2026-03-04
                         043 Shadow state object keys as u32 (interned field names) ⏳
                         044 Hash-based object diff (skip no-op object sets) ⏳
                              │
                       EP12 Rust Perf: Many Pipelines (⏳)
                         045 Pre-sort TopicRouter at registration
                         046 Arc-based RevIndex (remove HashSet clone)
                         047 Configurable PipelineContext capacity hints
                         048 Graph merge: drain HashSet instead of tmp Vec
                              │
                       EP13 Rust Deduplication & Abstraction (✅)
                         049 PathIndexedRegistry<T> + RegistrySource ✅
                         050 Shared condition index helper fn ✅
                         051 Shared get_affected_targets helper fn ✅
                         052 Unified shadow collect_paths ✅
                         053 Shadow path resolution guard helper ✅
                         054 Unified change iteration — N/A (only aggregation has writes)
                              │
                       EP14 Rust Third-Party Crate Adoption (✅)
                         055 ahash: replace std HashMap everywhere ✅
                         056 string-interner — SKIPPED (Symbol↔u32 friction, lose ids_with_prefix)
                         057 smallvec: hot-path Vec<u32> collections ✅
                         058 slab — SKIPPED (depended on 056)
                         059 petgraph — SKIPPED (custom O(1) lookup better than full traversal)
                         060 indexmap — SKIPPED (insertion order ≠ depth order)
```

~~EP5 Streaming (SUPERSEDED)~~ — No-op change filtering at every pipeline step makes a separate streaming gateway unnecessary.

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

### 🚫 Superseded: EP5 Streaming Data Gateway

- **Decision**: Absorbed by EP6 Pipeline Refactor
- **Reason**: The pipeline now filters no-op changes at every step (diff engine built into pipeline checkpoints). A separate streaming gateway with its own diff/filter API is no longer needed — the pipeline itself is the filter.
- **Original spec**: `tasks/WASM-EP5-STREAMING-v2.md` (archived, not implemented)

### ✅ Completed: EP6 Pipeline Refactor (100%)

- **Completion Date**: 2026-02-15
- **Stories**: WASM-032 (single story epic)
- **Spec**: `tasks/WASM-EP5-PIPELINE-REFACTOR.md` (canonical)
- **Key Deliverables**:
  - Two-phase pipeline: `processChanges()` → JS executes listeners/validators → `pipelineFinalize()`
  - WASM owns all diffing (3 internal checkpoints)
  - WASM owns shadow state updates (during finalize)
  - JS simplified to ~20 lines: send → execute JS-only work → finalize → apply
  - No manual concern change handling in JS
  - No manual validator result writing in JS
  - No-op change filtering at every pipeline step
  - **Implementation**: `rust/src/pipeline.rs`, `rust/src/lib.rs`, `src/wasm/bridge.ts`, `src/pipeline/processChanges.ts`
- **Commits**: b9b89b1, c0bf901, 22199cf

### ⏳ Ready: EP7 Clean Mode Split

- **Depends On**: EP6 (complete)
- **Spec**: `tasks/WASM-EP7-CLEAN-MODE-SPLIT.md` (canonical)
- **Stories**: WASM-033 → WASM-037
- **Scope**:
  - WASM-033: Clean sync registration (skip JS PathGroups) — 2pts
  - WASM-034: Clean flip registration (skip JS PathGroups) — 1pt
  - WASM-035: Clean listener registration (keep `listenerHandlers` only) — 2pts
  - WASM-036: WASM aggregation path (remove `effect()`) — 3pts ⚠️ may need Rust change
  - WASM-037: Verify clean separation + benchmark test — 2pts
- **Total**: 10pts
- **Risk**: WASM-036 depends on whether Rust pipeline handles aggregation read direction (sources → target). Developer must verify before implementation.

### ⏳ Ready: EP8 Recency-Based Sync Path Prioritization

- **Depends On**: EP6 (complete)
- **Spec**: `tasks/WASM-EP8-RECENCY-SYNC.md` (canonical)
- **Stories**: WASM-038 → WASM-040
- **Scope**:
  - WASM-038: Add recency tracking infrastructure (change counter + HashMap) — 2pts
  - WASM-039: Update sync registration to prioritize most recent value — 2pts
  - WASM-040: Integration tests for recency prioritization — 1pt
- **Total**: 5pts
- **Motivation**: Currently sync paths use majority voting. This doesn't reflect user intent — if a user just changed `profile.name`, syncing should use that value (most recent), not the majority.
- **Benefits**: Better UX (last touched wins), minimal overhead (O(1) lookups), backward compatible fallback.

---

## Test Suite — Phase 1 (Placeholders) & Phase 2 (Implementation)

**Test plan**: `docs/WASM_TEST_PLAN_PHASE1.md`

**Errata**: Test plan section "Shadow state NOT updated in Phase 1" is **incorrect**. `processChanges()` DOES update shadow state during processing (needed for BoolLogic). Those stubs should be rewritten.

### Phase 1: Write Placeholders (3 tasks)

| Task | File | Action | Stubs | Status |
|------|------|--------|-------|--------|
| QA-1 | `tests/wasm/two-phase-pipeline.test.ts` | **New file** — EP6 two-phase flow | ~40 | ⏳ |
| QA-2 | `tests/wasm/listener-dispatch.test.ts` | **Fix bug** (L230,245) + add FullExecutionPlan stubs | +11 | ⏳ |
| QA-3 | `tests/wasm/two-phase-pipeline.test.ts` | **Extend** — E2E integration scenarios | +20 | ⏳ |

### Phase 2: Implement Tests (4 tasks — after Phase 1)

| Task | File | Stubs to implement | Priority | Status |
|------|------|--------------------|----------|--------|
| QA-4 | `tests/wasm/validation-batching.test.ts` | 18 (existing stubs) | Critical | ⏳ |
| QA-5 | `tests/wasm/two-phase-pipeline.test.ts` | ~60 (from QA-1 + QA-3) | Critical | ⏳ |
| QA-6 | `tests/wasm/listener-dispatch.test.ts` | ~11 (from QA-2) | High | ⏳ |
| QA-7 | Existing EP1/EP2 test files | Minor edge case additions | Medium | ⏳ |

### Existing Coverage (no work needed)

| File | Tests | Coverage |
|------|-------|----------|
| `tests/wasm/bool_logic.test.ts` | ~991 lines | ✅ Comprehensive |
| `tests/wasm/interning.test.ts` | ~533 lines | ✅ Comprehensive |
| `tests/wasm/shadow.test.ts` | ~1994 lines | ✅ Comprehensive |
| `tests/wasm/pipeline.test.ts` | ~340 lines | ✅ Good |
| `tests/wasm/pipeline-integration.test.ts` | ~630 lines | ✅ Good |
| `tests/wasm/diff-engine.test.ts` | ~271 lines | ✅ Comprehensive |

---

## Recent Commits

| Hash | Message | Status |
|------|---------|--------|
| 22199cf | feat(wasm): pipeline update, simplified typescript pipeline handler | ✅ |
| b9b89b1 | feat(wasm): optimize writes and filter out no-op changes | ✅ |
| c0bf901 | feat(wasm): diff engine | ✅ |
| 772aa47 | feat(wasm): validations | ✅ |
| 94f0770 | feat(wasm): allow for validations (zod) in rust | ✅ |
| fa385db | feat(wasm): optimizations, test placeholders, speed comparisons | ✅ |
| f7afce2 | feat(wasm): pipelines, graphs, embedding of wasm inline | ✅ |
| 705dc4f | feat(wasm): initial rebuild, base-line rust code in place, basic bridge added | ✅ |
| a55c664 | docs(wasm): updated claude.md, added wasm architecture and task for processing | ✅ |
| 22ee86b | Implement Shadow State as Nested Tree in Rust/WASM (#13) | ✅ |
| fcfe40e | WASM-004: BoolLogic evaluator with tuple variants (#12) | ✅ |

---

### 🔄 In Progress: EP11 Rust Performance — Huge Objects

**Spec**: `tasks/WASM-EP11-PERF-HUGE-OBJECTS.md` (canonical)

- **Depends On**: EP6 (complete)
- **Stories**: WASM-041 → WASM-044 | **Total**: 16pts
- **Progress**:
  - ✅ WASM-041: Remove subtree clone in `shadow.set()` — 2026-03-04
  - ✅ WASM-042: `affected_path_ids()` returning `Vec<u32>` (intern-first, all 3 callers updated) — 2026-03-04
  - ⏳ WASM-043: Shadow state object keys as `u32` (interned field names)
  - ⏳ WASM-044: Hash-based object diff (skip no-op object sets)

### ✅ Completed: EP12 Rust Performance — Many Pipelines

- **Completion Date**: 2026-03-03
- **Spec**: `tasks/WASM-EP12-PERF-MANY-PIPELINES.md` (canonical)
- **Stories**: WASM-045 → WASM-048 | **Total**: 6pts
- **Key Deliverables**:
  - WASM-045: Pre-sort TopicRouter at registration (cache `effective_depth` etc. in `SubscriberMeta`) ✅
  - WASM-046: Arc-based RevIndex (`Arc<HashSet<u32>>` — zero deep clone at registration) ✅
  - WASM-047: `PipelineContext::recalibrate()` capacity hints after register calls ✅
  - WASM-048: Graph merge: drain HashSet via `.remove()` instead of tmp Vec ✅

### ✅ Completed: EP13 Rust Deduplication & Abstraction

- **Completion Date**: 2026-03-03
- **Spec**: `tasks/WASM-EP13-DEDUPLICATION.md` (canonical)
- **Key Deliverables**:
  - Unified `RegistrySource` type (replaced identical `AggregationSource` + `ComputationSource`)
  - `PathIndexedRegistry<T>` generic struct with `HasRegistrySources` trait
  - Composition pattern: domain registries wrap `PathIndexedRegistry<T>` as `inner` field
  - Shared helper functions: `add_condition_paths`, `remove_condition_paths`, `remove_from_index`, `get_affected_targets`
  - Unified `shadow.collect_paths()` (parameterized)
  - Shadow path resolution guard helper
  - WASM-054 N/A: only aggregation has write direction

### ✅ Completed: EP14 Rust Third-Party Crate Adoption

- **Completion Date**: 2026-03-03
- **Spec**: `tasks/WASM-EP14-CRATE-ADOPTION.md` (canonical)
- **Key Deliverables**:
  - WASM-055 `ahash`: Replaced std HashMap/HashSet everywhere via `prelude.rs` re-exports ✅
  - WASM-056 `string-interner`: SKIPPED — Symbol↔u32 impedance at 100+ call sites, loses `ids_with_prefix()`
  - WASM-057 `smallvec`: Hot-path `SmallVec<[u32; 4]>` in rev_index + router ✅
  - WASM-058 `slab`: SKIPPED — depended on WASM-056
  - WASM-059 `petgraph`: SKIPPED — custom O(1) component lookup beats petgraph's full traversal
  - WASM-060 `indexmap`: SKIPPED — insertion order ≠ depth order; explicit sort still needed

---

## Sizing Reference

| Points | Meaning |
|---|---|
| 1 | Trivial, < 2h |
| 2 | Small, half day |
| 3 | Medium, 1 day |
| 5 | Large, 2-3 days |
| 8 | Complex, ~1 week |
