---
title: Side Effects Guide
updated: 2026-02-05
audience: contributors working with side effects
---

# Side Effects Guide

Side effects are synchronous reactions to state changes that run during the change processing pipeline, before concerns re-evaluate. They are registered via the `useSideEffects` hook or the `registerSideEffects` function.

## Overview

| Side Effect      | What it does                                                     | Config format                            |
| ---------------- | ---------------------------------------------------------------- | ---------------------------------------- |
| **Sync paths**   | Keeps two paths in sync — changing one updates the other         | `[path1, path2]`                         |
| **Flip paths**   | Keeps two boolean paths as inverses                              | `[path1, path2]`                         |
| **Aggregations** | Derives a target value from multiple sources                     | `[target, source]` (target always first) |
| **Listeners**    | Reacts to changes under a path, optionally returning new changes | `{ path, scope, fn }`                    |

## Registration

### Via hook (recommended)

```typescript
store.useSideEffects('my-effects', {
  syncPaths: [...],
  flipPaths: [...],
  aggregations: [...],
  listeners: [...],
})
```

The hook registers on mount and cleans up on unmount via `useLayoutEffect`.

### Via function (outside React)

```typescript
import { registerSideEffects } from '@sladg/apex-state'

const cleanup = registerSideEffects(store, 'my-effects', { ... })
// Call cleanup() when done
```

Individual registration functions are also exported: `registerSyncPair`, `registerSyncPairsBatch`, `registerFlipPair`, `registerListener`.

## Type-Safe Pair Helpers (Large State Types)

The direct pair types (`SyncPair<T>`, `FlipPair<T>`, `AggregationPair<T>`, `ComputationPair<T>`) build an O(N²) union of all valid path combinations. This hits TypeScript's TS2589 recursion limit at ~1,500 paths.

For large state types, use the **lazy-validated helper functions** instead:

```typescript
import { syncPairs, flipPairs, aggregationPairs, computationPairs } from '@sladg/apex-state'

// Curried: first call binds the state type, second call validates pairs
const syncs = syncPairs<MyLargeState>()([
  ['billing.email', 'shipping.email'],   // ✓ both string — autocomplete works
  ['billing.phone', 'shipping.phone'],   // ✓ both string
  ['billing.total', 'shipping.email'],   // ✗ number vs string → type error
])

const flips = flipPairs<MyLargeState>()([
  ['isActive', 'isInactive'],            // ✓ both boolean
])

const aggs = aggregationPairs<MyLargeState>()([
  ['total', 'price1'],
  ['total', 'price2', { IS_EQUAL: ['price2.disabled', true] }],
])

const comps = computationPairs<MyLargeState>()([
  ['SUM', 'total', 'subtotal1'],
  ['AVG', 'average', 'score1'],
])
```

**How they work**: The function constraint uses `ResolvableDeepKey<T>` (O(N)) for path autocomplete. Validation uses `CheckPairValueMatch` which resolves `DeepValue` only for the concrete paths you write — O(1) per pair, O(K) total. Runtime behavior is identity (returns input as-is).

**Scaling limits** (Apple M4 Pro):

| Paths | Structure | Result | Time | Memory |
|-------|-----------|--------|------|--------|
| ~1,500 | `SyncPair<T>` (old) | TS2589 | — | — |
| ~1,650 | 10 sectors, depth 4 | PASS | 1.3s | 400 MB |
| ~33,000 | 200 sectors, depth 4 | PASS | 2.5s | 734 MB |
| ~52,800 | 6 nesting levels, depth 9 | PASS | 4.7s | 574 MB |
| ~82,500 | 500 sectors, depth 4 | PASS | 7.1s | 617 MB |
| ~105,600 | 7 nesting levels, depth 10 | TS2589 | — | — |

Practical limit: **~50K–80K paths** — a ~30–50x improvement over the direct types. The bottleneck is `ResolvableDeepKey` union resolution at the function constraint, not the pair validation itself.

**When to use which**:

- **`SyncPair<T>`** and direct types: Fine for state types with < ~1,000 paths. Use in type annotations.
- **`syncPairs<T>()()`** helpers: Required for large state types. Use to define pair arrays with validation.

## Sync Paths

Keep multiple paths synchronized. When one path changes, all paths in the same sync group receive the new value.

```typescript
store.useSideEffects("sync", {
  syncPaths: [
    ["billing.email", "shipping.email"],
    ["billing.phone", "shipping.phone"],
  ],
})
```

**Behavior on registration**: If the paths already have values, the most common non-null value wins and is applied to all paths in the group.

**Implementation**: Uses `PathGroups` data structure for O(1) connected component lookups. Multiple sync pairs can form a group — if A syncs with B and B syncs with C, all three stay in sync.

**Source**: `src/sideEffects/prebuilts/sync.ts`

### Batched Registration (Performance-Critical)

When registering multiple sync pairs at once (e.g. via `registerSideEffects` or `useSideEffects`), the system uses `registerSyncPairsBatch` instead of calling `registerSyncPair` in a loop. This is a **critical performance optimization** — do not revert to per-pair registration.

**Why**: Each `registerSyncPair` call triggers `processChanges()`, which writes to the valtio proxy via `applyBatch`, firing all registered concern `effect()` subscriptions. With N sync pairs and M concern effects, the loop approach causes N × M redundant re-evaluations. The batch approach adds all edges first, then calls `processChanges` once.

**Measured impact** (75 pairs / 150 fields, no concern effects):

| Approach | Median |
|----------|--------|
| Loop (75 `processChanges` calls) | ~4.0 ms |
| Batch (1 `processChanges` call) | ~0.4 ms |

With concern effects registered (real-world scenario), the difference is significantly larger.

**Functions**:

- `registerSyncPair(store, path1, path2)` — registers a single pair, calls `processChanges` immediately. Use for standalone/dynamic registration.
- `registerSyncPairsBatch(store, pairs)` — registers all pairs, calls `processChanges` once. Used internally by `registerSideEffects`.

Both share the same helpers (`collectGroupSyncChanges`, `makeSyncEdgeCleanup`) and produce identical behavior — only the number of `processChanges` calls differs.

## Flip Paths

Keep two boolean paths as inverses. When one becomes `true`, the other becomes `false`.

```typescript
store.useSideEffects("flips", {
  flipPaths: [
    ["isActive", "isInactive"],
    ["isExpanded", "isCollapsed"],
  ],
})
```

**Implementation**: Uses `PathGroups` for O(1) lookups, processed by the flip paths pipeline processor during change processing.

**Source**: `src/sideEffects/prebuilts/flip.ts`

## Aggregations

Derive a target value from multiple source paths. If all sources have the same value, the target gets that value. Otherwise, the target becomes `undefined`.

```typescript
store.useSideEffects("agg", {
  aggregations: [
    // target <- source (target is ALWAYS first)
    ["summary.price", "legs.0.price"],
    ["summary.price", "legs.1.price"],
    ["summary.price", "legs.2.price"],
  ],
})
```

In this example, `summary.price` will equal the leg price if all three legs have the same price, or `undefined` if they differ.

**Constraints**:

- Target is always the first element in the tuple
- A path cannot be both a target and a source (throws at registration)
- Multiple pairs can point to the same target for multi-source aggregation

**Implementation**: Uses `valtio-reactive`'s `effect()` for automatic dependency tracking — source reads are tracked, target writes trigger re-renders.

**Source**: `src/sideEffects/prebuilts/aggregation.ts`

## Listeners

React to changes under a watched path. Listeners receive scoped changes and state, and can optionally return new changes to apply.

```typescript
store.useSideEffects("listeners", {
  listeners: [
    {
      path: "user.profile", // watch changes under this path
      scope: "user.profile", // receive scoped state
      fn: (changes, state) => {
        // changes: [['name', 'Alice', {}]]  -- paths relative to scope
        // state: user.profile sub-object
        return [["audit.lastEdit", Date.now(), {}]] // return FULL paths
      },
    },
  ],
})
```

### Path and Scope

| `path`                | `scope`          | Changes receive                      | State receives |
| --------------------- | ---------------- | ------------------------------------ | -------------- |
| `'user.profile'`      | `'user.profile'` | Relative paths (`'name'`)            | Scoped object  |
| `'user.profile.name'` | `null`           | Full paths (`'user.profile.name'`)   | Full state     |
| `'a.b.c.d'`           | `'a.b'`          | Relative to scope (`'c.d'`)          | Value at `a.b` |
| `null`                | `null`           | Full paths for ALL changes (always fires) | Full state     |

**Constraints**:

- `scope` must be a parent/ancestor of `path` (or null, or equal to path)
- Listeners only fire for nested path changes, not the watched path itself
- Listeners are sorted by path depth (deepest first)
- Return `undefined` from `fn` if no new changes are needed

**Source**: `src/sideEffects/prebuilts/listeners.ts`

## Processing Order

During `processChanges()`, side effects run in this order:

1. Changes are normalized
2. **Sync paths** processor runs (propagates values to synced paths)
3. **Flip paths** processor runs (inverts paired booleans)
4. **Aggregation** writes are applied
5. **Listeners** execute (deepest paths first)
6. If any side effect produced new changes, the loop repeats (up to `maxIterations`)

After the pipeline settles, `valtio-reactive` effects fire and concerns re-evaluate.

## Key Files

| File                                       | Role                                            |
| ------------------------------------------ | ----------------------------------------------- |
| `src/sideEffects/registration.ts`          | Combined registration for all side effect types |
| `src/sideEffects/prebuilts/sync.ts`        | Sync path registration and initial sync         |
| `src/sideEffects/prebuilts/flip.ts`        | Flip path registration                          |
| `src/sideEffects/prebuilts/aggregation.ts` | Aggregation registration with `effect()`        |
| `src/sideEffects/prebuilts/listeners.ts`   | Listener registration with scope validation     |
| `src/pipeline/processChanges.ts`           | Main processing loop that invokes processors    |
| `src/pipeline/processors/syncPaths.ts`     | Sync path processor in pipeline                 |
| `src/pipeline/processors/flipPaths.ts`     | Flip path processor in pipeline                 |
| `src/pipeline/processors/listeners.ts`     | Listener processor in pipeline                  |
| `src/types/sideEffects.ts`                 | `SideEffects<DATA>` type definition             |

## Testing

- Integration tests: `tests/integration/side-effects.test.tsx`, `tests/integration/sync-paths.test.tsx`, `tests/integration/pipeline-sync-flip-listeners.test.tsx`
- Aggregation tests: `tests/integration/aggregations.test.tsx`, `tests/integration/aggregation-behavior.test.tsx`
- Pipeline tests: `tests/pipeline/integration.test.tsx`
