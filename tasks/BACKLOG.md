# WASM Pipeline — Backlog

Project key: **WASM**

## Epics

| Key | Title | Stories | Depends On | Status |
|---|---|---|---|---|
| WASM-EP1 | Foundation & Toolchain | WASM-001 → WASM-007 | — | Planned |
| WASM-EP2 | Shadow State & Pipeline | WASM-008 → WASM-015 | EP1 | Planned |
| WASM-EP3 | Listener Routing | WASM-016 → WASM-020 | EP2 | Planned |
| WASM-EP4 | Validation Batching | WASM-021 → WASM-024 | EP2 | Planned |
| WASM-EP5 | Streaming Data Gateway | WASM-025 → WASM-028 | EP2 | Planned |

EP3, EP4, EP5 can run in parallel after EP2 completes.

## Story Map

```
EP1 Foundation
  001 Rust toolchain setup
  002 String interning table
  003 BoolLogic evaluator (Rust)
  004 Reverse dependency index
  005 JS bridge: BoolLogic concerns
  006 WASM inline build (tsup)
  007 Phase 1 integration tests
        │
        ▼
EP2 Shadow State & Pipeline
  008 Shadow state (Rust)
  009 Value slot array
  010 processAggregationWrites (Rust)
  011 Sync graph + processSyncPaths (Rust)
  012 Flip graph + processFlipPaths (Rust)
  013 normalizeChangesForGroups (Rust)
  014 JS-side pipeline bridge
  015 Phase 2 integration tests
        │
        ├──────────────┬──────────────┐
        ▼              ▼              ▼
EP3 Listeners    EP4 Validation  EP5 Streaming
  016 TopicRouter   021 Rev-dep     025 Diff engine
  017 Dispatch plan 022 Dispatch    026 Gateway API
  018 Seed+routing  023 Batch Zod   027 Pipeline integration
  019 JS dispatch   024 Tests       028 Tests
  020 Tests
```

## Sizing Reference

| Points | Meaning |
|---|---|
| 1 | Trivial, < 2h |
| 2 | Small, half day |
| 3 | Medium, 1 day |
| 5 | Large, 2-3 days |
| 8 | Complex, ~1 week |
