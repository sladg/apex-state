# Phase 2, Task 04: Basic React Hooks

**Task IDs**: APEX-14, APEX-15, APEX-16
**Priority**: P0 (Critical)
**Dependencies**: Task 03 (Base Store)
**Phase**: Core Store

---

## 🎯 Worker Prompt

**YOU ARE**: A React hooks specialist
**YOUR FOCUS**: Implement core hooks for store access: useStore, useJitStore, useSideEffects
**STAY FOCUSED**: Do NOT implement side-effect logic yet. Focus on hook interfaces and basic state access.
**SUCCESS MEANS**: Hooks provide type-safe access to store, reactive updates work, side-effects can be registered

---

## 📋 Task Breakdown

### APEX-14: Implement useStore Hook

Create a useState-like hook for accessing and updating specific paths.

**What to do:**
1. Hook accepts path parameter: `useStore(path: DeepKey<DATA>)`
2. Returns tuple: `[state, setState]` like React's useState
3. State value is reactive (triggers re-render on change)
4. setState updates valtio proxy at the specified path
5. Type-safe: infers correct types from path

### APEX-15: Implement useJitStore Hook

Create a Just-In-Time data access hook for bulk operations.

**What to do:**
1. Hook takes no parameters: `useJitStore()`
2. Returns object with three properties:
   - `proxyValue`: reactive snapshot from valtio (useSnapshot)
   - `setChanges`: function to apply batch changes
   - `getState`: function to get non-reactive snapshot
3. setChanges accepts `ArrayOfChanges<DATA, META>`
4. getState returns current state without triggering re-renders

### APEX-16: Implement useSideEffects Hook

Create a useLayoutEffect wrapper for registering side effects.

**What to do:**
1. Hook accepts: `useSideEffects(id: string, effects: SideEffects<DATA>)`
2. Registers side effects on mount
3. Unregisters side effects on unmount using unique ID
4. Uses useLayoutEffect for synchronous execution
5. SideEffects type prepared (implementation comes later)

---

## ✅ Acceptance Criteria

### APEX-14 Criteria:
- [ ] `useStore` hook exported from `src/hooks/useStore.ts`
- [ ] Signature: `useStore<DATA, P extends DeepKey<DATA>>(path: P): [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void]`
- [ ] Uses valtio's useSnapshot for reactive state
- [ ] setState updates store at path using deepSet utility
- [ ] Re-renders only when accessed path value changes
- [ ] Throws error if used outside Provider context
- [ ] Test demonstrates reactive updates

### APEX-15 Criteria:
- [ ] `useJitStore` hook exported from `src/hooks/useJitStore.ts`
- [ ] Returns object: `{ proxyValue: DATA, setChanges: (changes: ArrayOfChanges<DATA, META>) => void, getState: () => DATA }`
- [ ] proxyValue uses valtio's useSnapshot (reactive)
- [ ] setChanges applies all changes (basic implementation, pipeline integration later)
- [ ] getState returns snapshot without reactivity (use valtio's snapshot function)
- [ ] Throws error if used outside Provider context
- [ ] Test demonstrates batch updates

### APEX-16 Criteria:
- [ ] `useSideEffects` hook exported from `src/hooks/useSideEffects.ts`
- [ ] Signature: `useSideEffects(id: string, effects: SideEffects<DATA>): void`
- [ ] Uses useLayoutEffect (not useEffect)
- [ ] Registers effects on mount, unregisters on unmount
- [ ] `SideEffects<DATA>` type defined (empty/placeholder for now)
- [ ] Store maintains registry of side effects by ID
- [ ] Test demonstrates registration/unregistration

---

## 📦 Expected Output

### File Structure:

```
src/
  hooks/
    useStore.ts          # useStore hook
    useJitStore.ts       # useJitStore hook
    useSideEffects.ts    # useSideEffects hook
    useStoreContext.ts   # Internal: access store from context
    index.ts             # Re-export all hooks
  store/
    sideEffectsRegistry.ts  # Registry for side effects
  types/
    sideEffects.ts       # SideEffects type (placeholder)
  index.ts               # Re-export hooks

tests/
  hooks/
    useStore.test.tsx
    useJitStore.test.tsx
    useSideEffects.test.tsx
```

### src/hooks/useStore.ts signature:

```typescript
import type { DeepKey, DeepValue, GenericMeta } from '../types'

export function useStore<
  DATA extends object,
  P extends DeepKey<DATA>,
  META extends GenericMeta = GenericMeta
>(
  path: P
): [
  DeepValue<DATA, P>,
  (value: DeepValue<DATA, P>, meta?: META) => void
]
```

### src/hooks/useJitStore.ts signature:

```typescript
import type { ArrayOfChanges, GenericMeta } from '../types'

export interface JitStoreReturn<DATA, META extends GenericMeta> {
  proxyValue: DATA
  setChanges: (changes: ArrayOfChanges<DATA, META>) => void
  getState: () => DATA
}

export function useJitStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(): JitStoreReturn<DATA, META>
```

### src/hooks/useSideEffects.ts signature:

```typescript
import type { SideEffects } from '../types/sideEffects'

export function useSideEffects<DATA extends object>(
  id: string,
  effects: SideEffects<DATA>
): void
```

### Placeholder SideEffects type (for now):

```typescript
// src/types/sideEffects.ts
export interface SideEffects<DATA> {
  // Placeholder - will be filled in later tasks
  // syncPaths?: ...
  // flipPaths?: ...
  // listeners?: ...
  // validators?: ...
  // aggregations?: ...
  // clearPaths?: ...
}
```

---

## 🧪 Verification Steps

### Test Examples:

```typescript
import { render, screen, waitFor } from '@testing-library/react'
import { createGenericStore } from '../src'

test('useStore reads and updates values', async () => {
  const store = createGenericStore<{ count: number }>()

  function Counter() {
    const [count, setCount] = store.useStore('count')
    return (
      <div>
        <span>{count}</span>
        <button onClick={() => setCount(count + 1)}>Increment</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ count: 0 }}>
      <Counter />
    </store.Provider>
  )

  expect(screen.getByText('0')).toBeInTheDocument()

  getByText('Increment').click()
  await waitFor(() => expect(screen.getByText('1')).toBeInTheDocument())
})

test('useJitStore batch updates', () => {
  const store = createGenericStore<{ a: number, b: number }>()

  function Component() {
    const { proxyValue, setChanges } = store.useJitStore()

    const updateBoth = () => {
      setChanges([
        ['a', 10, {}],
        ['b', 20, {}]
      ])
    }

    return (
      <div>
        <span>a: {proxyValue.a}</span>
        <span>b: {proxyValue.b}</span>
        <button onClick={updateBoth}>Update</button>
      </div>
    )
  }

  // Test that both update together
})

test('useSideEffects registers and unregisters', () => {
  const store = createGenericStore<{ value: number }>()

  function Component() {
    store.useSideEffects('test-id', {
      // Placeholder effects
    })
    return <div>Test</div>
  }

  const { unmount } = render(
    <store.Provider initialState={{ value: 0 }}>
      <Component />
    </store.Provider>
  )

  // Verify effects registered
  // Unmount and verify effects unregistered
  unmount()
})
```

### Manual Verification:

```bash
npm run type-check
npm run build
npm test
```

---

## 🚨 Common Pitfalls

- **DON'T**: Implement the synchronizer pipeline yet - just apply changes directly for now
- **DON'T**: Implement actual side-effect logic - just set up the registration system
- **DON'T**: Break valtio's reactivity - use useSnapshot correctly
- **DO**: Use valtio's snapshot() for non-reactive reads
- **DO**: Ensure hooks throw errors outside Provider context
- **DO**: Test that re-renders only happen when accessed data changes
- **DO**: Use useLayoutEffect (not useEffect) for useSideEffects

---

## 💡 Implementation Tips

### useStore Implementation:

```typescript
import { useCallback } from 'react'
import { useSnapshot } from 'valtio'
import { useStoreContext } from './useStoreContext'
import { deepGet, deepSet } from '../store/utils/deepAccess'

export function useStore<DATA, P extends DeepKey<DATA>>(
  path: P
): [DeepValue<DATA, P>, (value: DeepValue<DATA, P>) => void] {
  const store = useStoreContext<DATA>()
  const snap = useSnapshot(store.state)

  const value = deepGet(snap, path)

  const setValue = useCallback((newValue: DeepValue<DATA, P>, meta?: META) => {
    deepSet(store.state, path, newValue)
    // Later: integrate with synchronizer pipeline
  }, [store, path])

  return [value, setValue]
}
```

### useJitStore Implementation:

```typescript
import { useSnapshot, snapshot } from 'valtio'
import { useStoreContext } from './useStoreContext'

export function useJitStore<DATA, META>(): JitStoreReturn<DATA, META> {
  const store = useStoreContext<DATA>()
  const proxyValue = useSnapshot(store.state)

  const setChanges = useCallback((changes: ArrayOfChanges<DATA, META>) => {
    // For now: apply changes directly
    // Later: integrate with synchronizer pipeline
    changes.forEach(([path, value, meta]) => {
      deepSet(store.state, path, value)
    })
  }, [store])

  const getState = useCallback(() => {
    return snapshot(store.state) as DATA
  }, [store])

  return { proxyValue, setChanges, getState }
}
```

### useSideEffects Implementation:

```typescript
import { useLayoutEffect } from 'react'
import { useStoreContext } from './useStoreContext'

export function useSideEffects<DATA>(
  id: string,
  effects: SideEffects<DATA>
): void {
  const store = useStoreContext<DATA>()

  useLayoutEffect(() => {
    // Register effects
    store.sideEffectsRegistry.register(id, effects)

    // Cleanup: unregister
    return () => {
      store.sideEffectsRegistry.unregister(id)
    }
  }, [store, id, effects])
}
```

---

## 📚 Reference Materials

- Valtio useSnapshot: https://github.com/pmndrs/valtio#useSnapshot
- Valtio snapshot function: Non-reactive reads
- React useLayoutEffect: Synchronous effect execution
- React custom hooks patterns

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 05**: `05-synchronizer-pipeline.md` - Build the change processing pipeline
