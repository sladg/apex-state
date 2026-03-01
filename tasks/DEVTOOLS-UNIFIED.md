# Proposal: Unified Redux DevTools Instance

**Status**: Draft
**Date**: 2026-02-25
**Scope**: `src/store/devtools.ts`, `src/utils/debug-log.ts`, `rust/src/lib.rs` (+ shadow.rs)

---

## Problem

Currently, the store exposes **three separate Redux DevTools instances**:

| Instance | Connected via | Granularity |
|----------|--------------|-------------|
| `{prefix}:state` | valtio `devtools()` | Per-field mutation |
| `{prefix}:concerns` | valtio `devtools()` | Per-concern mutation |
| `{prefix}:pipeline` | Custom `send()` | Per pipeline run |

This creates two problems:

### 1. Broken time-travel

When you use the DevTools time-travel slider on `:state`, valtio does:

```js
Object.assign(stateProxy, previousSnapshot)
```

But that leaves:

- `_concerns` at its current (post-rewind) position — **wrong**
- WASM shadow state unaware of the rewind — **wrong**
- Sync/flip graphs, topic router, listener state — all still at T_now — **wrong**

Rewinding one instance with three out-of-sync layers produces incoherent state.

### 2. Disordered action history

`runId` is sequential per pipeline instance, but DevTools shows actions interleaved across all three instances. The slider order is unpredictable and the action labels (`set:user.email`, `set:user.concerns.email`, `PIPELINE_RUN user.email`) don't map to each other.

---

## Solution: One Instance, Pipeline-Scoped Snapshots

Replace the three separate instances with a **single unified DevTools connection** that:

1. Sends **one action per pipeline run** (not per-field mutation)
2. Snapshots **both proxies together** in one consistent payload
3. Handles **`JUMP_TO_ACTION`** by rehydrating state + concerns + WASM atomically
4. Preserves the pipeline tree as action metadata (no information lost)

---

## Architecture

### Snapshot shape

```ts
interface UnifiedSnapshot {
  state: Record<string, unknown>    // snapshot(store.state)
  concerns: Record<string, unknown> // snapshot(store._concerns)
}
```

Every action in Redux DevTools history will carry this combined snapshot. Jumping to any point gives you the exact state AND concerns that existed after that pipeline run.

### Action shape

```
PIPELINE_RUN user.email          ← action type (same as today)
{                                ← action state (new: combined)
  state:    { user: { email: "alice@example.com", ... } },
  concerns: { "user.email": { validationState: { isError: false } } },
  pipeline: {                    ← pipeline tree (same as today)
    runId: 5,
    phase1: [...],
    "sync (+2)": [...],
    "[00] listener:3 onEmail": { ... }
  }
}
```

### Time-travel flow

```
DevTools sends: { type: "JUMP_TO_ACTION", state: "{...UnifiedSnapshot...}" }
                          │
                          ▼
         parse msg.state → { state, concerns, pipeline }
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
  Object.assign(    Object.assign(   wasm.resetShadowState(
    stateProxy,       concernsProxy,   parsed.state
    parsed.state        parsed.concerns  )  ← new WASM export
  )               )
```

All three layers rehydrated atomically from the same snapshot. Time-travel works.

> **Simplification principle**: During time-travel we already have the exact target state AND
> concerns from the snapshot. WASM does not need to compute anything — no pipeline, no
> BoolLogic evaluation, no sync/flip, no listeners. `resetShadowState` should be a **pure
> value replacement** — walk the snapshot JSON and overwrite the shadow tree in-place.
> Nothing else in WASM needs to change; graphs, interning, and registries are structural
> (registered once, not value-dependent) and remain valid across time-travel.

---

## Changes Required

### 1. `src/utils/debug-log.ts` — Add `subscribe` to `DevToolsInstance`

The real Redux DevTools API has `subscribe(listener)` — we just haven't typed it:

```ts
export interface DevToolsInstance {
  init:        (state: unknown) => void
  send:        (action: { type: string }, state: unknown) => void
  subscribe:   (listener: (msg: DevToolsMessage) => void) => () => void  // ← add
  unsubscribe: () => void
}

interface DevToolsMessage {
  type:  'DISPATCH' | string
  payload?: { type: 'JUMP_TO_ACTION' | 'JUMP_TO_STATE' | 'COMMIT' | string }
  state?: string  // JSON string of the snapshot
}
```

### 2. `src/store/devtools.ts` — Replace two `devtools()` calls with one custom connection

```ts
// REMOVE:
connectProxy(store.state,     `${dt.prefix}:state`,    enabled)
connectProxy(store._concerns, `${dt.prefix}:concerns`, enabled)

// ADD:
connectUnified(store, dt, enabled)
```

`connectUnified` will:

- Call `ext.connect({ name: prefix })` once
- Subscribe to messages for time-travel handling
- Expose a `sendSnapshot(pipelineTree)` function that the pipeline observer calls at `pipelineEnd`

### 3. `src/utils/debug-log.ts` — `pipelineEnd()` sends combined snapshot

Instead of:

```ts
pipelineEnd: () => {
  instance.send({ type: `PIPELINE_RUN${pathSuffix}` }, tree)
}
```

It becomes:

```ts
pipelineEnd: (getState: () => UnifiedSnapshot) => {
  const snap = getState()
  instance.send({ type: `PIPELINE_RUN${pathSuffix}` }, { ...snap, pipeline: tree })
}
```

`getState` is a small callback injected by the store — calls `snapshot()` on both proxies.

### 4. `rust/src/lib.rs` + `rust/src/shadow.rs` — New `resetShadowState` export

```rust
/// Reset the entire shadow state from a JS state snapshot.
/// Called during DevTools time-travel to resync WASM after proxy rehydration.
#[wasm_bindgen]
pub fn reset_shadow_state(state_json: &str) -> Result<(), JsValue> {
    let value: serde_json::Value = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    PIPELINE.with_borrow_mut(|p| p.shadow.reset(value));
    Ok(())
}
```

`shadow.reset(value)` replaces the entire shadow tree from a JSON snapshot — symmetric to how shadow state is built during `processChanges`.

### 5. `src/wasm/bridge.ts` — Expose `resetShadowState`

```ts
export const wasm = {
  // ... existing exports
  resetShadowState: (stateJson: string) => wasmModule.reset_shadow_state(stateJson),
}
```

---

## What We Keep / What We Lose

| | Current | Unified |
|--|---------|---------|
| Per-field action granularity | ✅ (`:state`, `:concerns`) | ❌ — one action per pipeline run |
| Ordered, readable history | ❌ — interleaved from 3 instances | ✅ — single timeline |
| Time-travel works | ❌ — desync between layers | ✅ — atomic rehydration |
| Pipeline tree visible | ✅ | ✅ — nested under `pipeline:` key |
| Consistent snapshots | ❌ — each instance independent | ✅ — state+concerns always paired |
| WASM sync on time-travel | ❌ — never synced | ✅ — `resetShadowState` called |

The one thing we lose is sub-pipeline granularity. Under the current setup, if a listener produces 3 field changes, those appear as 3 separate actions in `:state`. Under the unified model, they're all inside one `PIPELINE_RUN` action in the `pipeline.phase2` subtree. The information is still there — just nested rather than top-level.

---

## Open Questions

1. **Non-pipeline mutations**: Some state changes may happen outside the pipeline (e.g., direct valtio proxy mutations in tests or setup). These won't produce a `PIPELINE_RUN`. Do we want to capture them at all? If so, we need a fallback `subscribe()` on the state proxy for out-of-band mutations.

2. **Initial state**: `instance.init(snapshot)` should be called with the initial combined snapshot after store setup, before any pipeline runs.

3. **`shadow.reset()` scope**: Only the shadow value tree needs resetting — graphs, interning, and registries are structural (registered once, not value-dependent) and survive time-travel intact. `reset()` is a pure value replacement: walk the snapshot, overwrite shadow values, done. No pipeline execution, no BoolLogic, no listener dispatch.

4. **StrictMode guard**: The existing `WeakSet<object>` guard in `connectProxy` prevents duplicate connections during StrictMode double-mounts. The unified approach needs an equivalent guard on the store instance itself.

---

## Files Touched

```
src/store/devtools.ts                      replace two devtools() with connectUnified()
src/utils/debug-log.ts                     add subscribe to DevToolsInstance; pipelineEnd gets getState callback
src/pipeline/process-changes.wasm-impl.ts  pass getState into observer at pipelineEnd
src/store/provider.tsx                     wire getState into DevToolsRef or observer creation
src/wasm/bridge.ts                         expose resetShadowState
rust/src/lib.rs                            add reset_shadow_state wasm_bindgen export
rust/src/shadow.rs                         implement Shadow::reset(value: serde_json::Value)
```

No public API changes. No changes to `createStore`, `useStore`, or `useConcerns`.
