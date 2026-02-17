---
title: Architecture Guide
updated: 2026-01-29
audience: contributors touching core architecture
---

# Apex State Architecture

Review `docs/guides/WORKFLOW_RULES.md` for the non-negotiables. This document explains _why_ the architecture looks the way it does so you can make surgical changes without breaking the feedback loops that keep Apex State stable.

## Goals at a Glance

- **Deterministic feedback**: Concerns re-run only when the data they read changes.
- **Isolated writes**: Computations never dirty the user-facing proxy.
- **Predictable React updates**: All hooks observe the same snapshots.
- **Recoverable internals**: Operational machinery lives outside React render cycles.

## High-Level Flow

```
User mutation → Valtio proxy → processChanges()
  ↳ side-effect graphs (optional) → additional mutations
  ↳ valtio-reactive effect() → concern evaluation
      ↳ writes to store._concerns
React components → useSnapshot(store.state | store._concerns)
```

Every arrow is implemented in the code referenced below; this diagram is the north star when diagnosing bugs.

## StoreInstance Anatomy

```
StoreInstance {
  state: proxy({ ... })          // user data (tracked)
  _concerns: proxy({ ... })      // concern outputs (tracked)
  _internal: ref({ ... })        // graphs, queues, registries (not tracked)
}
```

| Component             | Location                   | Notes                                        |
| --------------------- | -------------------------- | -------------------------------------------- |
| `StoreInstance` types | `src/store/types.ts`       | Defines public surface + internal helpers.   |
| Provider creation     | `src/store/Provider.tsx`   | Builds proxies, installs listeners on mount. |
| Hooks + setters       | `src/store/createStore.ts` | All public hooks share this store instance.  |

The separation between `proxy` and `ref` is the reason React renders only when user data or concern outputs shift.

## Dependency Tracking

- We rely on `valtio-reactive`'s `effect()` (see `src/concerns/registration.ts`).
- Any deep property read during `evaluate()` becomes a tracked dependency.
- Concern outputs are written into `_concerns`; reading another concern should go through BoolLogic conditions on state paths instead of reading `_concerns` directly.
- `derive-valtio` was rejected because it tracks proxies wholesale; rationale captured in `CONCERNS_REFERENCE.md`.

## Change Processing

| Stage                    | File                                 | Purpose                                                                       |
| ------------------------ | ------------------------------------ | ----------------------------------------------------------------------------- |
| Queue construction       | `src/store/createStore.ts` (setters) | Wrap incoming mutations into change objects.                                  |
| Processing loop          | `src/store/executor.ts`              | Applies changes, resolves side-effects, iterates with guard (max 100 passes). |
| Side-effect registration | `src/sideEffects/registration.ts`    | Builds graphs used by the executor.                                           |

`_internal.processing` carries the queue + reentrancy guard so repeated mutations stay bounded.

## Type System Staples

- `DeepKey` / `DeepValue`: `src/types/deepKey.ts`, `src/types/deepValue.ts` — keep hook inputs/outputs aligned with the store schema.
- `SideEffects<DATA>`: `src/types/sideEffects.ts` — shared contract for sync paths, flips, aggregations, listeners.
- Concern typing: `src/concerns/types.ts` — config + return typing for everything registered via `useConcerns`.

Keep these generics untouched; they enforce invariants across the public API.

## Valtio Pitfalls

### `proxy()` mutates the original object

`proxy(obj)` uses `obj` as the Proxy target. Mutations through the proxy write directly to the original object:

```typescript
const original = { fieldA: '' }
const p = proxy(original)
p.fieldA = 'changed'
original.fieldA // → 'changed' (mutated!)
```

**Consequence:** Never pass shared/singleton objects to `proxy()`. The Provider deep-clones `initialState` before proxying to prevent caller objects from being tainted. Tests must not rely on fixture mutation for state persistence.

**Consequence:** `structuredClone()` fails on proxy-tainted objects (`DataCloneError`), since valtio adds non-cloneable internal metadata.

### Getters on proxy: no dependency tracking

Valtio preserves JS getters on proxied objects — they re-evaluate correctly when accessed:

```typescript
const state = proxy({
  firstName: 'John',
  get fullName() { return this.firstName + '!' }
})
state.firstName = 'Jane'
state.fullName // → 'Jane!' (re-evaluated)
```

**However, getters have no selective dependency tracking.** Valtio's `subscribe()` fires on ANY mutation to the proxy, not just properties the getter reads. In React, `useSnapshot` will trigger a re-render for every state change, and the getter re-evaluates even if its dependencies didn't change.

**Current status:** Getters are optimized via `valtio-reactive`'s `computed()`. The Provider extracts getters from `initialState` using `extractGetters`, then re-attaches them as `computed()` proxies with selective dependency tracking — each getter only re-evaluates when its actual dependencies change. Cleanup is GC-based: `computed()` internally creates `watch()` subscriptions with no explicit dispose API; these are collected once the referenced `stateProxy` becomes unreachable (e.g., after Provider unmount).

## Performance Notes

- Valtio batches writes in the same tick; the executor applies them once, then `effect()` coalesces concern evaluations.
- Concerns must remain fast and pure—no async, no mutation. Heavy work belongs in side-effects.
- For bulk updates, favor `useJitStore().setChanges` so mutations enter the pipeline together.

## When You Need More Detail

| Topic                                   | Source                          |
| --------------------------------------- | ------------------------------- |
| Failure stories and rejected approaches | `CONCERNS_REFERENCE.md`         |
| Concern lifecycle                       | `docs/guides/CONCERNS_GUIDE.md` |
| Hook surface area                       | `docs/guides/STORE_HOOKS.md`    |

If a change conflicts with any invariant above, stop and escalate—it likely warrants an ADR rather than a stealth edit.
