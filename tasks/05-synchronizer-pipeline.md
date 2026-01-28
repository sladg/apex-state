# Phase 3, Task 05: Synchronizer Pipeline

**Task IDs**: APEX-36, APEX-37, APEX-38
**Priority**: P0 (Critical)
**Dependencies**: Task 04 (Basic Hooks)
**Phase**: Synchronizer Pipeline

---

## 🎯 Worker Prompt

**YOU ARE**: A state management pipeline architect
**YOUR FOCUS**: Build the change processing pipeline that coordinates all side-effects
**STAY FOCUSED**: Do NOT implement individual side-effects yet. Create the pipeline infrastructure.
**SUCCESS MEANS**: Changes flow through pipeline, side-effects can be plugged in, atomic updates work

---

## 📋 Task Breakdown

### APEX-36: Design Synchronizer Pipeline Architecture

Design the pipeline system for processing ArrayOfChanges before state application.

**What to do:**
1. Define synchronizer function signature
2. Define pipeline execution order: sync → flip → listeners → validators → aggregations → clear
3. Each synchronizer can add/modify/remove changes
4. Final changes applied atomically to state

### APEX-37: Implement Synchronizer Execution Engine

Build the engine that runs changes through the pipeline.

**What to do:**
1. Accept initial ArrayOfChanges
2. Run through synchronizer functions in defined order
3. Detect infinite loops (max iterations limit)
4. Apply final changes atomically to valtio proxy
5. Ensure single React re-render for all changes

### APEX-38: Integrate Synchronizers with setChanges

Connect useJitStore's setChanges to the synchronizer pipeline.

**What to do:**
1. setChanges calls synchronizer pipeline before applying
2. Meta information preserved through pipeline
3. All side effects processed correctly
4. Store updates only after pipeline completes

---

## ✅ Acceptance Criteria

### APEX-36 Criteria:
- [ ] `Synchronizer<DATA, META>` type defined in `src/pipeline/types.ts`
- [ ] `PipelineConfig` type defines execution order
- [ ] Synchronizer function signature: `(changes: ArrayOfChanges<DATA, META>, state: DATA) => ArrayOfChanges<DATA, META>`
- [ ] Pipeline order documented: sync → flip → listeners → validators → aggregations → clear
- [ ] Support for synchronizers to add new changes

### APEX-37 Criteria:
- [ ] `executePipeline` function exported from `src/pipeline/executor.ts`
- [ ] Signature: `executePipeline<DATA, META>(initialChanges, state, synchronizers) => ArrayOfChanges<DATA, META>`
- [ ] Executes synchronizers in configured order
- [ ] Detects infinite loops (max 100 iterations)
- [ ] Throws error on infinite loop detection
- [ ] Returns final, stable changes for application
- [ ] Test demonstrates multi-pass stabilization

### APEX-38 Criteria:
- [ ] useJitStore's setChanges integrated with pipeline
- [ ] useStore's setState integrated with pipeline
- [ ] Pipeline executes before any state updates
- [ ] Atomic update: all final changes applied together
- [ ] Single React re-render per setChanges call
- [ ] Meta information preserved through pipeline
- [ ] Test demonstrates pipeline integration

---

## 📦 Expected Output

### File Structure:

```
src/
  pipeline/
    types.ts              # Synchronizer types
    executor.ts           # Pipeline execution engine
    synchronizers/
      index.ts            # Synchronizer registration
      syncPaths.ts        # Placeholder for sync (task 06)
      flipPaths.ts        # Placeholder for flip (task 08)
      listeners.ts        # Placeholder for listeners (task 07)
      validators.ts       # Placeholder for validators (task 10)
      aggregations.ts     # Placeholder for aggregations (task 09)
      clearPaths.ts       # Placeholder for clear (task 11)
  hooks/
    useStore.ts           # Updated to use pipeline
    useJitStore.ts        # Updated to use pipeline
  index.ts

tests/
  pipeline/
    executor.test.ts
    integration.test.ts
```

### src/pipeline/types.ts:

```typescript
import type { ArrayOfChanges, GenericMeta } from '../types'

/**
 * A synchronizer processes changes and can add/modify/remove changes.
 * It receives current changes and state, returns modified changes.
 */
export type Synchronizer<
  DATA extends object,
  META extends GenericMeta = GenericMeta
> = (
  changes: ArrayOfChanges<DATA, META>,
  state: DATA
) => ArrayOfChanges<DATA, META>

/**
 * Configuration for the pipeline execution order
 */
export interface PipelineConfig<DATA, META extends GenericMeta> {
  synchronizers: Array<{
    name: string
    fn: Synchronizer<DATA, META>
  }>
  maxIterations: number  // Default: 100
}
```

### src/pipeline/executor.ts:

```typescript
import type { ArrayOfChanges, GenericMeta } from '../types'
import type { Synchronizer, PipelineConfig } from './types'

/**
 * Executes the synchronizer pipeline until changes stabilize.
 *
 * The pipeline runs iteratively:
 * 1. Apply all synchronizers in order
 * 2. If new changes were added, run again
 * 3. Stop when no new changes or max iterations reached
 *
 * @throws Error if infinite loop detected (max iterations exceeded)
 */
export function executePipeline<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(
  initialChanges: ArrayOfChanges<DATA, META>,
  state: DATA,
  config: PipelineConfig<DATA, META>
): ArrayOfChanges<DATA, META> {
  let currentChanges = initialChanges
  let iteration = 0

  while (iteration < config.maxIterations) {
    const changesBeforeIteration = currentChanges.length

    // Run through all synchronizers
    for (const { name, fn } of config.synchronizers) {
      currentChanges = fn(currentChanges, state)
    }

    // If no new changes were added, we're done
    if (currentChanges.length === changesBeforeIteration) {
      break
    }

    iteration++
  }

  if (iteration >= config.maxIterations) {
    throw new Error(
      `Pipeline exceeded max iterations (${config.maxIterations}). ` +
      'Possible infinite loop in side-effects.'
    )
  }

  return currentChanges
}

/**
 * Apply changes atomically to valtio state
 */
export function applyChanges<DATA extends object, META extends GenericMeta>(
  state: DATA,
  changes: ArrayOfChanges<DATA, META>
): void {
  // Use valtio batch if available, or apply all at once
  // This ensures single re-render
  changes.forEach(([path, value]) => {
    deepSet(state, path, value)
  })
}
```

### Default pipeline order:

```typescript
// src/pipeline/synchronizers/index.ts
import type { PipelineConfig } from '../types'

export function createDefaultPipeline<DATA, META>(): PipelineConfig<DATA, META> {
  return {
    synchronizers: [
      { name: 'sync', fn: syncPathsSynchronizer },      // Task 06
      { name: 'flip', fn: flipPathsSynchronizer },      // Task 08
      { name: 'listeners', fn: listenersSynchronizer }, // Task 07
      { name: 'validators', fn: validatorsSynchronizer }, // Task 10
      { name: 'aggregations', fn: aggregationsSynchronizer }, // Task 09
      { name: 'clear', fn: clearPathsSynchronizer }     // Task 11
    ],
    maxIterations: 100
  }
}

// Placeholder synchronizers (no-ops for now)
function syncPathsSynchronizer<DATA, META>(
  changes: ArrayOfChanges<DATA, META>,
  state: DATA
): ArrayOfChanges<DATA, META> {
  // TODO: Implement in task 06
  return changes
}

// ... other placeholders
```

### Updated useJitStore integration:

```typescript
export function useJitStore<DATA, META>(): JitStoreReturn<DATA, META> {
  const store = useStoreContext<DATA>()
  const proxyValue = useSnapshot(store.state)

  const setChanges = useCallback((changes: ArrayOfChanges<DATA, META>) => {
    // Execute pipeline
    const finalChanges = executePipeline(
      changes,
      snapshot(store.state),
      store.pipelineConfig
    )

    // Apply final changes atomically
    applyChanges(store.state, finalChanges)
  }, [store])

  // ... rest
}
```

---

## 🧪 Verification Steps

### Test Examples:

```typescript
import { executePipeline } from '../src/pipeline/executor'

test('pipeline executes synchronizers in order', () => {
  const executionOrder: string[] = []

  const config = {
    synchronizers: [
      { name: 'first', fn: (changes) => { executionOrder.push('first'); return changes } },
      { name: 'second', fn: (changes) => { executionOrder.push('second'); return changes } }
    ],
    maxIterations: 100
  }

  executePipeline([], {}, config)
  expect(executionOrder).toEqual(['first', 'second'])
})

test('pipeline stabilizes when no new changes', () => {
  let runCount = 0
  const config = {
    synchronizers: [
      {
        name: 'test',
        fn: (changes) => {
          runCount++
          return changes // No new changes
        }
      }
    ],
    maxIterations: 100
  }

  executePipeline([['path', 'value', {}]], {}, config)
  expect(runCount).toBe(1) // Should only run once
})

test('pipeline detects infinite loops', () => {
  const config = {
    synchronizers: [
      {
        name: 'infinite',
        fn: (changes) => {
          // Always add a new change
          return [...changes, ['new', 'value', {}]]
        }
      }
    ],
    maxIterations: 10
  }

  expect(() => {
    executePipeline([], {}, config)
  }).toThrow(/exceeded max iterations/)
})

test('setChanges applies changes atomically', async () => {
  const store = createGenericStore<{ a: number, b: number }>()

  let renderCount = 0

  function Component() {
    const { proxyValue, setChanges } = store.useJitStore()
    renderCount++

    return (
      <div>
        <button onClick={() => setChanges([
          ['a', 10, {}],
          ['b', 20, {}]
        ])}>
          Update
        </button>
        <span>a: {proxyValue.a}, b: {proxyValue.b}</span>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 0, b: 0 }}>
      <Component />
    </store.Provider>
  )

  const initialRenderCount = renderCount

  getByText('Update').click()

  await waitFor(() => {
    expect(getByText('a: 10, b: 20')).toBeInTheDocument()
  })

  // Should only re-render once for both changes
  expect(renderCount).toBe(initialRenderCount + 1)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Implement actual side-effect logic yet - use placeholders
- **DON'T**: Apply changes during pipeline execution - wait until the end
- **DON'T**: Mutate state during pipeline - work with change arrays only
- **DO**: Ensure changes are applied atomically (single re-render)
- **DO**: Add comprehensive error handling for infinite loops
- **DO**: Preserve meta information through the pipeline
- **DO**: Test that the execution order is correct

---

## 💡 Implementation Tips

### Detecting Stabilization:

The pipeline should run until changes stabilize:
- After running all synchronizers, check if new changes were added
- If yes, run again (synchronizers might need to process new changes)
- If no, we're done - apply the changes

### Atomic Updates:

Valtio batching options:
1. Use `batch()` from valtio/utils (if available)
2. Apply all changes in quick succession (React batches automatically)
3. Use a single object spread to update multiple paths

### Performance:

- Don't run unnecessary iterations
- Short-circuit when no changes added
- Consider memoization for expensive synchronizers (later optimization)

---

## 📚 Reference Materials

- Valtio batch updates: Check for batching utilities
- React automatic batching (React 18+)
- Pipeline pattern for data processing

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 06**: `06-sync-paths.md` - Implement sync paths side-effect (PERFORMANCE CRITICAL)
- **Task 07**: `07-listeners.md` - Implement state change listeners
