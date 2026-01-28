# Phase 4, Task 07: State Change Listeners

**Task IDs**: APEX-24, APEX-25, APEX-26
**Priority**: P1 (High)
**Dependencies**: Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Core

---

## 🎯 Worker Prompt

**YOU ARE**: An event system architect
**YOUR FOCUS**: Implement state change listeners with smart nested path breakdown
**STAY FOCUSED**: Break down changes only when necessary (listeners exist at that level)
**SUCCESS MEANS**: Listeners trigger correctly, nested breakdown works, efficient execution

---

## 📋 Task Breakdown

### APEX-24: Implement Listener Registration System

Allow registering state change listeners (global or scoped to specific paths).

**What to do:**
1. API: `registerListener(id, key, fn)` where key is null (global) or DeepKey
2. API: `unregisterListener(id)`
3. Store listeners indexed by path for fast lookup
4. Support multiple listeners for same path
5. Global listeners (key=null) receive all changes

### APEX-25: Implement Smart Virtual Change Breakdown

Break down object changes into nested path changes based on registered listeners.

**What to do:**
1. When `some.value` changes to object, check if listeners exist for nested paths
2. Break down ONLY to level + 1 if listeners exist at that level
3. If value is primitive (cannot break down), stop at that level
4. Virtual breakdown ONLY for triggering listeners, NOT for state updates
5. Build index of "which paths have listeners" for fast lookup
6. Lazy breakdown - only as deep as necessary

### APEX-26: Implement Listener Invocation System

Call appropriate listeners when changes occur.

**What to do:**
1. Global listeners (key=null) receive all changes
2. Scoped listeners receive only relevant changes
3. Pass current state snapshot to listener functions
4. Handle listener errors gracefully (don't break change application)
5. Listeners invoked in registration order

---

## ✅ Acceptance Criteria

### APEX-24 Criteria:
- [ ] `ListenersRegistry<DATA, META>` class in `src/sideEffects/listeners/registry.ts`
- [ ] Method: `register(id, key, fn)` where key is `null | DeepKey<DATA>`
- [ ] Method: `unregister(id)`
- [ ] Method: `getListeners(key)` returns listeners for specific path
- [ ] Method: `getGlobalListeners()` returns global listeners
- [ ] Fast lookup via Map indexed by path
- [ ] Test demonstrates multiple listeners per path

### APEX-25 Criteria:
- [ ] `breakdownChanges` function in `src/sideEffects/listeners/breakdown.ts`
- [ ] Signature: `breakdownChanges<DATA, META>(changes, registry) => ArrayOfChanges<DATA, META>`
- [ ] Only breaks down when listeners exist at nested levels
- [ ] Breaks down to level + 1 if object, stops if primitive
- [ ] Example: `user` → `{name: 'John', age: 30}` breaks down to `user.name` and `user.age` ONLY if listeners exist
- [ ] Index of listener paths cached for performance
- [ ] Test demonstrates smart breakdown logic

### APEX-26 Criteria:
- [ ] `listenersSynchronizer` function in `src/pipeline/synchronizers/listeners.ts`
- [ ] Invokes global listeners with all changes
- [ ] Invokes scoped listeners with filtered changes
- [ ] Passes current state snapshot to listeners
- [ ] Catches and logs listener errors without breaking pipeline
- [ ] Test demonstrates listener invocation
- [ ] Test demonstrates error handling

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    listeners/
      registry.ts        # ListenersRegistry class
      breakdown.ts       # Smart change breakdown logic
      types.ts           # OnStateChangesListenerFunction type
      index.ts
  pipeline/
    synchronizers/
      listeners.ts       # listenersSynchronizer
  types/
    sideEffects.ts       # Updated with listeners config

tests/
  sideEffects/
    listeners/
      registry.test.ts
      breakdown.test.ts
      synchronizer.test.ts
      integration.test.tsx
```

### src/sideEffects/listeners/types.ts:

```typescript
import type { DeepKey, DeepValue, ArrayOfChanges } from '../../types'

/**
 * Listener function for state changes.
 *
 * If key is null: receives all changes and full state
 * If key is a path: receives changes for that scope and scoped state
 */
export type OnStateChangesListenerFunction<
  DATA extends object,
  META extends GenericMeta,
  Key extends DeepKey<DATA> | null = null
> = {
  key: Key
  fn: (
    changes: Key extends null
      ? ArrayOfChanges<DATA, META>
      : ArrayOfChanges<DeepValue<DATA, Key>, META>,
    currentState: Key extends null ? DATA : DeepValue<DATA, Key>
  ) => void
}
```

### src/sideEffects/listeners/registry.ts:

```typescript
import type { DeepKey, GenericMeta } from '../../types'
import type { OnStateChangesListenerFunction } from './types'

export class ListenersRegistry<
  DATA extends object,
  META extends GenericMeta
> {
  private listeners = new Map<string, OnStateChangesListenerFunction<DATA, META>[]>()
  private globalListeners: OnStateChangesListenerFunction<DATA, META, null>[] = []
  private listenerIds = new Map<string, string | null>() // id → key
  private listenerPathsIndex = new Set<string>() // Paths that have listeners

  register<K extends DeepKey<DATA> | null>(
    id: string,
    key: K,
    fn: OnStateChangesListenerFunction<DATA, META, K>['fn']
  ): void {
    const listener = { key, fn } as OnStateChangesListenerFunction<DATA, META>

    if (key === null) {
      this.globalListeners.push(listener as any)
    } else {
      const pathKey = key as string
      if (!this.listeners.has(pathKey)) {
        this.listeners.set(pathKey, [])
      }
      this.listeners.get(pathKey)!.push(listener)
      this.listenerPathsIndex.add(pathKey)
    }

    this.listenerIds.set(id, key as string | null)
  }

  unregister(id: string): void {
    const key = this.listenerIds.get(id)
    if (key === undefined) return

    if (key === null) {
      // Remove from global listeners
      // (need to track by id properly for removal)
    } else {
      const listeners = this.listeners.get(key)
      if (listeners) {
        // Remove listener (need to track by id properly)
      }
      this.listenerPathsIndex.delete(key)
    }

    this.listenerIds.delete(id)
  }

  getListeners(key: DeepKey<DATA>): OnStateChangesListenerFunction<DATA, META>[] {
    return this.listeners.get(key as string) || []
  }

  getGlobalListeners(): OnStateChangesListenerFunction<DATA, META, null>[] {
    return this.globalListeners
  }

  hasListenerForPath(path: string): boolean {
    return this.listenerPathsIndex.has(path)
  }

  hasListenerForNestedPath(basePath: string): boolean {
    // Check if any listener path starts with basePath
    for (const path of this.listenerPathsIndex) {
      if (path.startsWith(basePath + '.')) {
        return true
      }
    }
    return false
  }
}
```

### src/sideEffects/listeners/breakdown.ts:

```typescript
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ListenersRegistry } from './registry'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Smart breakdown of changes for listener triggering.
 *
 * Only breaks down changes when listeners exist at nested levels.
 * Breaks down to level + 1 if object, stops if primitive.
 *
 * Example:
 *   Change: ['user', {name: 'John', age: 30}, {}]
 *   If listener exists for 'user.name' → add ['user.name', 'John', {}]
 *   If listener exists for 'user.age' → add ['user.age', 30, {}]
 */
export function breakdownChanges<
  DATA extends object,
  META extends GenericMeta
>(
  changes: ArrayOfChanges<DATA, META>,
  registry: ListenersRegistry<DATA, META>,
  state: DATA
): ArrayOfChanges<DATA, META> {
  const result: ArrayOfChanges<DATA, META> = [...changes]

  for (const [path, value, meta] of changes) {
    // Check if there are listeners for nested paths
    if (!registry.hasListenerForNestedPath(path as string)) {
      continue // No nested listeners, skip breakdown
    }

    // If value is object, break down to level + 1
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      for (const key of Object.keys(value)) {
        const nestedPath = `${path}.${key}` as DeepKey<DATA>
        const nestedValue = (value as any)[key]

        // Only add if listener exists at this exact level OR deeper
        if (
          registry.hasListenerForPath(nestedPath as string) ||
          registry.hasListenerForNestedPath(nestedPath as string)
        ) {
          result.push([nestedPath, nestedValue, meta])

          // Recursively break down further if needed
          if (
            nestedValue &&
            typeof nestedValue === 'object' &&
            !Array.isArray(nestedValue)
          ) {
            const deeperChanges = breakdownChanges(
              [[nestedPath, nestedValue, meta]],
              registry,
              state
            )
            result.push(...deeperChanges.slice(1)) // Skip the first (already added)
          }
        }
      }
    }
  }

  return result
}
```

### src/pipeline/synchronizers/listeners.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ListenersRegistry } from '../../sideEffects/listeners/registry'
import { breakdownChanges } from '../../sideEffects/listeners/breakdown'
import { snapshot } from 'valtio'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Synchronizer for listeners side-effect.
 *
 * Breaks down changes smartly and invokes relevant listeners.
 * Does NOT add new changes - only triggers listeners.
 */
export function createListenersSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: ListenersRegistry<DATA, META>
): Synchronizer<DATA, META> {
  return (changes, state) => {
    // Break down changes for listener triggering
    const allChanges = breakdownChanges(changes, registry, state)

    // Get current state snapshot for listeners
    const currentState = snapshot(state) as DATA

    // Invoke global listeners
    const globalListeners = registry.getGlobalListeners()
    for (const listener of globalListeners) {
      try {
        listener.fn(allChanges, currentState)
      } catch (error) {
        console.error('Error in global listener:', error)
        // Don't break pipeline execution
      }
    }

    // Invoke scoped listeners
    const processedPaths = new Set<string>()
    for (const [path, value, meta] of allChanges) {
      const pathKey = path as string
      if (processedPaths.has(pathKey)) continue
      processedPaths.add(pathKey)

      const listeners = registry.getListeners(path)
      if (listeners.length === 0) continue

      // Filter changes relevant to this path
      const relevantChanges = allChanges.filter(
        ([p]) => (p as string).startsWith(pathKey)
      )

      // Get scoped state
      const scopedState = deepGet(currentState, path)

      for (const listener of listeners) {
        try {
          listener.fn(relevantChanges as any, scopedState)
        } catch (error) {
          console.error(`Error in listener for path "${pathKey}":`, error)
          // Don't break pipeline execution
        }
      }
    }

    // Listeners don't add changes
    return changes
  }
}
```

---

## 🧪 Verification Steps

```typescript
test('listeners: global listener receives all changes', () => {
  const store = createGenericStore<{ a: number, b: number }>()
  const receivedChanges: any[] = []

  function Component() {
    store.useSideEffects('listener', {
      listeners: {
        handlers: [{
          id: 'global',
          key: null,
          fn: (changes) => { receivedChanges.push(...changes) }
        }]
      }
    })

    const { setChanges } = store.useJitStore()

    return (
      <button onClick={() => setChanges([
        ['a', 10, {}],
        ['b', 20, {}]
      ])}>
        Update
      </button>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 0, b: 0 }}>
      <Component />
    </store.Provider>
  )

  getByText('Update').click()

  expect(receivedChanges).toHaveLength(2)
  expect(receivedChanges[0][0]).toBe('a')
  expect(receivedChanges[1][0]).toBe('b')
})

test('listeners: scoped listener receives only relevant changes', () => {
  const store = createGenericStore<{ user: { name: string }, other: number }>()
  const userChanges: any[] = []

  function Component() {
    store.useSideEffects('listener', {
      listeners: {
        handlers: [{
          id: 'user-listener',
          key: 'user',
          fn: (changes) => { userChanges.push(...changes) }
        }]
      }
    })

    const { setChanges } = store.useJitStore()

    return (
      <button onClick={() => setChanges([
        ['user.name', 'Alice', {}],
        ['other', 42, {}]
      ])}>
        Update
      </button>
    )
  }

  render(
    <store.Provider initialState={{ user: { name: '' }, other: 0 }}>
      <Component />
    </store.Provider>
  ).getByText('Update').click()

  // Should only receive user.name change, not 'other'
  expect(userChanges).toHaveLength(1)
  expect(userChanges[0][0]).toBe('user.name')
})

test('listeners: smart breakdown only when necessary', () => {
  const registry = new ListenersRegistry()

  // Register listener for user.name
  registry.register('test', 'user.name', () => {})

  const changes = [['user', { name: 'Alice', age: 30 }, {}]]
  const brokenDown = breakdownChanges(changes, registry, {})

  // Should break down user.name (has listener) but not user.age (no listener)
  expect(brokenDown.some(([p]) => p === 'user.name')).toBe(true)
  expect(brokenDown.some(([p]) => p === 'user.age')).toBe(false)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Break down all changes always (only when listeners exist)
- **DON'T**: Let listener errors break the pipeline
- **DON'T**: Modify state in listeners (read-only operation)
- **DO**: Use smart breakdown to avoid unnecessary work
- **DO**: Handle errors gracefully with try-catch
- **DO**: Test nested breakdown logic thoroughly

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 08**: `08-flip-paths.md` - Implement flip paths side-effect
