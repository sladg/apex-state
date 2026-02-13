---
created: 2026-02-05 (d221b60)
updated: 2026-02-05 (d221b60)
status: active
---

# HMR Behavior with Apex State

How Vite Hot Module Replacement interacts with concerns, side effects, and store state.

---

## Summary

- **Store state**: Preserved across HMR cycles (created with `useMemo([], ...)` in Provider)
- **Concerns**: May teardown and re-register on HMR, causing re-renders (see details below)
- **Side effects** (syncPaths, flipPaths, listeners): Safe — operate on untracked `_internal`, no re-renders

---

## Concerns and HMR

### How it works

The `useConcerns` hook uses `useLayoutEffect` with `customConcerns` in its dependency array. When Vite HMR sends new function references, the effect tears down and re-registers:

1. **Cleanup**: effects are disposed, values are **deleted** from `_concerns` proxy
2. **Re-registration**: effects are re-created, values are written back to `_concerns`

Even if the evaluated values are identical, the delete-then-write cycle mutates the `_concerns` proxy, which notifies all `useSnapshot` subscribers and triggers re-renders.

### When do concern references change?

Vite re-executes a module when it (or something it imports) changes, then walks up the import graph to the nearest HMR boundary (React components with Fast Refresh).

| Scenario | References change? | Re-registration correct? |
|---|---|---|
| Edit the concern file directly | Yes | **Yes** — logic actually changed |
| Edit a dependency of the concern file | Yes | Unnecessary — concern code unchanged |
| Concern defined in same file as component, edit component | Yes | Unnecessary — concern code unchanged |
| Concern in separate file, edit only the component | **No** | N/A — no re-registration |
| Default concerns from library (node_modules) | **No** | N/A — no re-registration |

**Key insight**: The most common HMR scenario (editing a concern file directly) results in correct re-registration behavior because the logic actually changed. Unnecessary re-registration only occurs in edge cases.

### Best practices for clients

1. **Define concerns in separate files** from components — this is the most impactful optimization. When you edit a component file, concern imports from other modules keep their cached references.

   ```
   // Good: separate files
   concerns/
     customValidation.ts
     customDisabled.ts
   components/
     MyField.tsx          ← editing this won't re-register concerns
   ```

2. **Memoize inline concern arrays** if passing custom concerns:

   ```tsx
   // Avoid: new array reference every render
   useConcerns('field', registration, [myConcern])

   // Better: stable reference
   const concerns = useMemo(() => [myConcern], [myConcern])
   useConcerns('field', registration, concerns)
   ```

3. **Avoid defining concerns inside component bodies** — module-level definitions are stable across renders:

   ```tsx
   // Avoid: new object every render
   const MyField = () => {
     const myConcern = { name: 'custom', evaluate: ... }
     // ...
   }

   // Better: module-level, stable reference
   const myConcern = { name: 'custom', evaluate: ... }
   const MyField = () => { /* ... */ }
   ```

---

## Side Effects and HMR

Side effects (syncPaths, flipPaths, listeners, aggregations) are **safe from re-render issues** because they operate on `store._internal`, which is wrapped in `ref()` and not tracked by valtio.

| Side effect | What it modifies | Tracked by valtio? | Re-render on HMR? |
|---|---|---|---|
| **syncPaths** | `_internal.graphs.sync` + `state` (initial sync only) | Graph: No. State: only if values differ | No (values already in sync) |
| **flipPaths** | `_internal.graphs.flip` | No | No |
| **listeners** | `_internal.graphs.listeners` | No | No |
| **aggregations** | `_internal.graphs` + registration maps | No | No |

The `useSideEffects` hook does teardown/re-register on HMR (same pattern as concerns), but since all operations target untracked internal state, no component re-renders.

---

## Potential future improvements

Several approaches were evaluated for making concern re-registration HMR-friendly. None were implemented due to tradeoffs:

### Approach: Separate cleanup from value deletion

Split `useLayoutEffect` cleanup (dispose effects) from `_concerns` value deletion (unmount only). Use a diff to handle registration changes.

- **Pro**: Prevents delete-then-rewrite cycle
- **Con**: Adds complexity (ref tracking, diff logic, separate unmount cleanup). Edge case with stale values between `useLayoutEffect` and `useEffect` cleanup on unmount.

### Approach: Stabilize references with useRef + version proxy

Keep effects alive across HMR, update concern functions via ref, bump a version proxy to force re-evaluation.

- **Pro**: No teardown at all during HMR
- **Con**: Adding/removing concern types during HMR doesn't work (no new effects created, stale values not cleaned up). Version bump during render is technically a side effect.

### Approach: Hash function bodies with toString()

Compare `Function.prototype.toString()` to detect actual code changes vs. reference-only changes.

- **Pro**: Simple implementation, detects real changes
- **Con**: Closure variables (module-level constants, imported helpers) are invisible to toString(). Cannot be enforced in client code. Adds production overhead for a dev-only benefit.

### Why current behavior is acceptable

The most common development scenario — editing a concern's `evaluate` function — results in correct re-registration because the logic actually changed and values likely differ. Unnecessary re-registration only occurs when:

1. A transitive dependency of the concern file changes
2. Concerns are co-located with components in the same file

Both are avoidable through code organization (separate concern files), making the current behavior acceptable for most real-world usage.
