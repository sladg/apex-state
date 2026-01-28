# Phase 5, Task 08: Flip Paths Side-Effect

**Task IDs**: APEX-22, APEX-23
**Priority**: P1 (High)
**Dependencies**: Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Secondary

---

## 🎯 Worker Prompt

**YOU ARE**: A state synchronization specialist
**YOUR FOCUS**: Implement flip paths for boolean/enum values that should be opposites
**STAY FOCUSED**: Simple bidirectional flip logic, handle only boolean and two-value enums
**SUCCESS MEANS**: Flipped paths have opposite values, no infinite flip loops

---

## 📋 Task Breakdown

### APEX-22: Implement Flip Paths Registration/Unregistration

Allow registering pairs of paths that should have opposite boolean/enum values.

**What to do:**
1. API: `registerFlipPaths(id, path1, path2)`
2. API: `unregisterFlipPaths(id)`
3. Store flip pairs for quick lookup
4. Support bidirectional flip (either can change first)

### APEX-23: Implement Flip Paths Change Processor

Process changes to flip opposite path values.

**What to do:**
1. When one path changes, update flipped path to opposite value
2. Works with boolean values: true ↔ false
3. Works with two-value enums: value1 ↔ value2
4. Set meta flag `isFlipPathChange: true`
5. Avoid infinite flip loops (don't flip changes that are already flip changes)

---

## ✅ Acceptance Criteria

### APEX-22 Criteria:
- [ ] `FlipPathsRegistry<DATA>` class in `src/sideEffects/flipPaths/registry.ts`
- [ ] Method: `register(id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>): void`
- [ ] Method: `unregister(id: string): void`
- [ ] Method: `getFlippedPath(path: DeepKey<DATA>): DeepKey<DATA> | undefined`
- [ ] Store bidirectional mapping for fast lookups
- [ ] Test demonstrates registration and lookup

### APEX-23 Criteria:
- [ ] `flipPathsSynchronizer` function in `src/pipeline/synchronizers/flipPaths.ts`
- [ ] Signature: `Synchronizer<DATA, META>`
- [ ] For each change: if flipped path exists, add flip change
- [ ] Boolean flip: true → false, false → true
- [ ] Enum flip: detect two-value enums and flip between them
- [ ] Meta flag `isFlipPathChange: true` set
- [ ] Skip processing changes with `isFlipPathChange: true`
- [ ] Integrated into pipeline
- [ ] Test demonstrates boolean and enum flipping

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    flipPaths/
      registry.ts        # FlipPathsRegistry class
      index.ts
  pipeline/
    synchronizers/
      flipPaths.ts       # flipPathsSynchronizer
  types/
    sideEffects.ts       # Updated with flipPaths config

tests/
  sideEffects/
    flipPaths/
      registry.test.ts
      synchronizer.test.ts
      integration.test.tsx
```

### src/sideEffects/flipPaths/registry.ts:

```typescript
import type { DeepKey } from '../../types'

/**
 * Registry for flip path pairs.
 * Maintains bidirectional mapping: path1 ↔ path2
 */
export class FlipPathsRegistry<DATA extends object> {
  private flips = new Map<string, string>() // path → flipped path
  private flipIds = new Map<string, [string, string]>() // id → [path1, path2]

  register(
    id: string,
    path1: DeepKey<DATA>,
    path2: DeepKey<DATA>
  ): void {
    const p1 = path1 as string
    const p2 = path2 as string

    // Bidirectional mapping
    this.flips.set(p1, p2)
    this.flips.set(p2, p1)

    this.flipIds.set(id, [p1, p2])
  }

  unregister(id: string): void {
    const paths = this.flipIds.get(id)
    if (!paths) return

    const [path1, path2] = paths
    this.flips.delete(path1)
    this.flips.delete(path2)
    this.flipIds.delete(id)
  }

  getFlippedPath(path: DeepKey<DATA>): DeepKey<DATA> | undefined {
    const flipped = this.flips.get(path as string)
    return flipped as DeepKey<DATA> | undefined
  }

  hasFlip(path: DeepKey<DATA>): boolean {
    return this.flips.has(path as string)
  }
}
```

### src/pipeline/synchronizers/flipPaths.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { FlipPathsRegistry } from '../../sideEffects/flipPaths/registry'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Synchronizer for flip paths side-effect.
 *
 * For each change, if a flipped path exists, add a change with opposite value.
 * Handles booleans and two-value enums.
 */
export function createFlipPathsSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: FlipPathsRegistry<DATA>
): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []

    for (const [path, value, meta] of changes) {
      // Skip if this is already a flip change (avoid infinite loops)
      if (meta.isFlipPathChange) {
        continue
      }

      // Check if this path has a flip pair
      const flippedPath = registry.getFlippedPath(path)
      if (!flippedPath) {
        continue
      }

      // Get the current value of the flipped path
      const currentFlippedValue = deepGet(state, flippedPath)

      // Calculate flipped value
      const flippedValue = flipValue(value, currentFlippedValue)

      // Add flip change
      newChanges.push([
        flippedPath,
        flippedValue,
        { ...meta, isFlipPathChange: true }
      ])
    }

    return [...changes, ...newChanges]
  }
}

/**
 * Flip a value to its opposite.
 * - Boolean: true ↔ false
 * - Enum: value1 ↔ value2 (if only two possible values)
 */
function flipValue(newValue: any, currentOppositeValue: any): any {
  // Boolean flip
  if (typeof newValue === 'boolean') {
    return !newValue
  }

  // For enums/other types: just use the current opposite value
  // This maintains the "swap" behavior for two-value enums
  return currentOppositeValue
}
```

### Updated SideEffects type:

```typescript
// src/types/sideEffects.ts
export interface FlipPathConfig<DATA> {
  pairs: Array<{
    id: string
    path1: DeepKey<DATA>
    path2: DeepKey<DATA>
  }>
}

export interface SideEffects<DATA> {
  syncPaths?: SyncPathConfig<DATA>
  flipPaths?: FlipPathConfig<DATA>
  // ... other side effects
}
```

---

## 🧪 Verification Steps

```typescript
test('flip paths: boolean flip', () => {
  const store = createGenericStore<{ a: boolean, b: boolean }>()

  function Component() {
    store.useSideEffects('flip', {
      flipPaths: {
        pairs: [{ id: 'ab', path1: 'a', path2: 'b' }]
      }
    })

    const [a, setA] = store.useStore('a')
    const [b] = store.useStore('b')

    return (
      <div>
        <span>a: {String(a)}</span>
        <span>b: {String(b)}</span>
        <button onClick={() => setA(!a)}>Flip A</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: true, b: false }}>
      <Component />
    </store.Provider>
  )

  // Initial: a=true, b=false
  expect(getByText('a: true')).toBeInTheDocument()
  expect(getByText('b: false')).toBeInTheDocument()

  // Flip A to false → B should become true
  getByText('Flip A').click()

  waitFor(() => {
    expect(getByText('a: false')).toBeInTheDocument()
    expect(getByText('b: true')).toBeInTheDocument()
  })
})

test('flip paths: enum flip', () => {
  type State = { mode: 'light' | 'dark', theme: 'light' | 'dark' }
  const store = createGenericStore<State>()

  function Component() {
    store.useSideEffects('flip', {
      flipPaths: {
        pairs: [{ id: 'mode-theme', path1: 'mode', path2: 'theme' }]
      }
    })

    const [mode, setMode] = store.useStore('mode')
    const [theme] = store.useStore('theme')

    return (
      <div>
        <span>mode: {mode}</span>
        <span>theme: {theme}</span>
        <button onClick={() => setMode(mode === 'light' ? 'dark' : 'light')}>
          Toggle
        </button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ mode: 'light', theme: 'dark' }}>
      <Component />
    </store.Provider>
  )

  // Initial: mode=light, theme=dark
  expect(getByText('mode: light')).toBeInTheDocument()
  expect(getByText('theme: dark')).toBeInTheDocument()

  // Toggle mode to dark → theme should become light (swap)
  getByText('Toggle').click()

  waitFor(() => {
    expect(getByText('mode: dark')).toBeInTheDocument()
    expect(getByText('theme: light')).toBeInTheDocument()
  })
})

test('flip paths: no infinite loops', () => {
  const registry = new FlipPathsRegistry<{ a: boolean, b: boolean }>()
  registry.register('ab', 'a', 'b')

  const changes: ArrayOfChanges = [
    ['a', true, { isFlipPathChange: true }] // Already a flip change
  ]

  const synchronizer = createFlipPathsSynchronizer(registry)
  const result = synchronizer(changes, { a: false, b: true })

  // Should NOT add another flip change
  expect(result).toHaveLength(1)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Process flip changes that already have `isFlipPathChange: true`
- **DON'T**: Assume only booleans - handle enums too
- **DON'T**: Create complex flip logic - keep it simple (swap values)
- **DO**: Test bidirectional flipping (either path can change first)
- **DO**: Test that infinite loops are prevented
- **DO**: Handle edge cases: undefined, null values

---

## 💡 Implementation Tips

### Flip Value Logic:

For booleans: Simple `!value`

For enums: The flip logic is a "swap" - when one changes, the other gets the previous value of the first. This is why we use `currentOppositeValue` in the flip function.

Example:
- Initial: a='light', b='dark'
- Change a to 'dark' → b gets 'light' (swap)

### Preventing Infinite Loops:

Always check `meta.isFlipPathChange` before processing. If it's already a flip change, skip it.

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 09**: `09-aggregations.md` - Implement aggregations side-effect (PERFORMANCE CRITICAL)
