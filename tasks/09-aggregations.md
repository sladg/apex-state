# Phase 5, Task 09: Aggregations Side-Effect

**Task IDs**: APEX-31, APEX-32, APEX-33
**Priority**: P2 (Medium) - **PERFORMANCE CRITICAL**
**Dependencies**: Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Secondary

---

## 🎯 Worker Prompt

**YOU ARE**: A data aggregation specialist
**YOUR FOCUS**: Implement one-way aggregations with dependency graph and cycle detection
**STAY FOCUSED**: Optimize for performance - aggregations run frequently with sync paths
**SUCCESS MEANS**: Source→target aggregation works, target→sources distribution works, no cycles, FAST

---

## 📋 Task Breakdown

### APEX-31: Design Aggregation Dependency Graph

Create graph for tracking one-to-many aggregation dependencies.

**What to do:**
1. Store: targetPath → sourcePaths[] mappings
2. Support multiple registrations for same targetPath (different source sets)
3. Cycle detection (ensure no circular aggregations)
4. Fast lookup in both directions (target→sources, source→targets)

### APEX-32: Implement Aggregation Registration System

Allow registering aggregation rules.

**What to do:**
1. API: `registerAggregation(id, targetPath, sourcePaths[])`
2. Validate no cycles created
3. Unregister by ID
4. Update dependency graph

### APEX-33: Implement Aggregation Processor (OPTIMIZE HEAVILY)

Process aggregation logic on changes.

**What to do:**
1. When sourcePaths change: if all equal, set targetPath to that value; else undefined
2. When targetPath changes: set all sourcePaths to that value
3. Handle multiple aggregations for same target correctly
4. **CRITICAL**: Cache equality checks, avoid redundant value comparisons
5. Efficient: only re-evaluate affected aggregations

---

## ✅ Acceptance Criteria

### APEX-31 Criteria:
- [ ] `AggregationGraph<DATA>` class in `src/sideEffects/aggregations/graph.ts`
- [ ] Method: `addAggregation(id, targetPath, sourcePaths[])`
- [ ] Method: `removeAggregation(id)`
- [ ] Method: `getAggregationsForTarget(targetPath)` returns all source sets
- [ ] Method: `getTargetsForSource(sourcePath)` returns affected targets
- [ ] Cycle detection with clear error messages
- [ ] Test demonstrates cycle prevention

### APEX-32 Criteria:
- [ ] `AggregationsRegistry<DATA>` class in `src/sideEffects/aggregations/registry.ts`
- [ ] Method: `register(id, targetPath, sourcePaths[])`
- [ ] Method: `unregister(id)`
- [ ] Integrates with AggregationGraph
- [ ] Throws error on cycle detection
- [ ] Multiple aggregations for same target allowed
- [ ] Test validates registration

### APEX-33 Criteria:
- [ ] `aggregationsSynchronizer` function in `src/pipeline/synchronizers/aggregations.ts`
- [ ] Signature: `Synchronizer<DATA, META>`
- [ ] Source change logic: all equal → set target, different → target = undefined
- [ ] Target change logic: distribute value to all sources
- [ ] Handle multiple source sets for same target
- [ ] **OPTIMIZATION**: Cache value comparisons
- [ ] **OPTIMIZATION**: Early exit when no aggregations affected
- [ ] Integrated into pipeline
- [ ] Test demonstrates both directions

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    aggregations/
      graph.ts           # AggregationGraph class
      registry.ts        # AggregationsRegistry class
      index.ts
  pipeline/
    synchronizers/
      aggregations.ts    # aggregationsSynchronizer
  types/
    sideEffects.ts       # Updated with aggregations config

tests/
  sideEffects/
    aggregations/
      graph.test.ts
      registry.test.ts
      synchronizer.test.ts
      integration.test.tsx
```

### src/sideEffects/aggregations/graph.ts:

```typescript
import type { DeepKey } from '../../types'

interface Aggregation<DATA> {
  id: string
  targetPath: DeepKey<DATA>
  sourcePaths: DeepKey<DATA>[]
}

/**
 * Graph for tracking aggregation dependencies.
 * Prevents cycles and provides fast lookups.
 */
export class AggregationGraph<DATA extends object> {
  private aggregations = new Map<string, Aggregation<DATA>>() // id → aggregation
  private targetToAggregations = new Map<string, Set<string>>() // target → Set<id>
  private sourceToTargets = new Map<string, Set<string>>() // source → Set<target>

  addAggregation(
    id: string,
    targetPath: DeepKey<DATA>,
    sourcePaths: DeepKey<DATA>[]
  ): void {
    const target = targetPath as string
    const sources = sourcePaths.map(p => p as string)

    // Cycle detection: ensure no source depends on target
    if (this.wouldCreateCycle(target, sources)) {
      throw new Error(
        `Cannot create aggregation: would create a cycle involving "${target}"`
      )
    }

    // Store aggregation
    const aggregation: Aggregation<DATA> = { id, targetPath, sourcePaths }
    this.aggregations.set(id, aggregation)

    // Index by target
    if (!this.targetToAggregations.has(target)) {
      this.targetToAggregations.set(target, new Set())
    }
    this.targetToAggregations.get(target)!.add(id)

    // Index by sources
    for (const source of sources) {
      if (!this.sourceToTargets.has(source)) {
        this.sourceToTargets.set(source, new Set())
      }
      this.sourceToTargets.get(source)!.add(target)
    }
  }

  removeAggregation(id: string): void {
    const aggregation = this.aggregations.get(id)
    if (!aggregation) return

    const target = aggregation.targetPath as string
    const sources = aggregation.sourcePaths.map(p => p as string)

    // Remove from indices
    this.targetToAggregations.get(target)?.delete(id)
    for (const source of sources) {
      this.sourceToTargets.get(source)?.delete(target)
    }

    this.aggregations.delete(id)
  }

  getAggregationsForTarget(targetPath: DeepKey<DATA>): Aggregation<DATA>[] {
    const target = targetPath as string
    const ids = this.targetToAggregations.get(target) || new Set()
    return Array.from(ids).map(id => this.aggregations.get(id)!)
  }

  getTargetsForSource(sourcePath: DeepKey<DATA>): Set<string> {
    return this.sourceToTargets.get(sourcePath as string) || new Set()
  }

  private wouldCreateCycle(
    newTarget: string,
    newSources: string[]
  ): boolean {
    // A cycle exists if any source can reach the target through existing aggregations
    // In aggregations: sourcePath → targetPath
    // So if newTarget can reach any newSource, adding this would create a cycle

    for (const source of newSources) {
      if (this.canReach(newTarget, source)) {
        return true
      }
    }

    return false
  }

  private canReach(from: string, to: string): boolean {
    // BFS: can we reach 'to' starting from 'from' via aggregation edges?
    // Edge: source → target (if source is in sourcePaths of an aggregation with target)

    const visited = new Set<string>()
    const queue = [from]

    while (queue.length > 0) {
      const current = queue.shift()!
      if (current === to) return true
      if (visited.has(current)) continue

      visited.add(current)

      // Find aggregations where current is a source
      for (const aggregation of this.aggregations.values()) {
        if (aggregation.sourcePaths.some(p => (p as string) === current)) {
          const target = aggregation.targetPath as string
          if (!visited.has(target)) {
            queue.push(target)
          }
        }
      }
    }

    return false
  }
}
```

### src/pipeline/synchronizers/aggregations.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { AggregationsRegistry } from '../../sideEffects/aggregations/registry'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Synchronizer for aggregations side-effect.
 *
 * Two behaviors:
 * 1. Source change: If all sources equal → set target to that value, else undefined
 * 2. Target change: Set all sources to target value
 *
 * PERFORMANCE CRITICAL: Optimize value comparisons and lookups.
 */
export function createAggregationsSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: AggregationsRegistry<DATA>
): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []
    const processedTargets = new Set<string>()
    const processedSources = new Set<string>()

    for (const [path, value, meta] of changes) {
      const pathStr = path as string

      // Check if this path is a target
      const aggregationsForTarget = registry.graph.getAggregationsForTarget(path)
      if (aggregationsForTarget.length > 0 && !processedTargets.has(pathStr)) {
        processedTargets.add(pathStr)

        // Target changed → distribute to all sources in all aggregations
        for (const aggregation of aggregationsForTarget) {
          for (const sourcePath of aggregation.sourcePaths) {
            newChanges.push([
              sourcePath,
              value,
              { ...meta, isProgramaticChange: true }
            ])
          }
        }
      }

      // Check if this path is a source
      const targetsForSource = registry.graph.getTargetsForSource(path)
      for (const targetPathStr of targetsForSource) {
        if (processedSources.has(targetPathStr)) continue
        processedSources.add(targetPathStr)

        const targetPath = targetPathStr as DeepKey<DATA>
        const aggregations = registry.graph.getAggregationsForTarget(targetPath)

        // For each aggregation, check if all sources are equal
        for (const aggregation of aggregations) {
          const sourceValues = aggregation.sourcePaths.map(p =>
            deepGet(state, p)
          )

          // Check if all equal
          const allEqual = sourceValues.every(v => v === sourceValues[0])

          if (allEqual) {
            // All equal → set target to that value
            newChanges.push([
              targetPath,
              sourceValues[0],
              { ...meta, isProgramaticChange: true }
            ])
          } else {
            // Different → set target to undefined
            newChanges.push([
              targetPath,
              undefined,
              { ...meta, isProgramaticChange: true }
            ])
          }
        }
      }
    }

    return [...changes, ...newChanges]
  }
}
```

### Updated SideEffects type:

```typescript
// src/types/sideEffects.ts
export interface AggregationConfig<DATA> {
  rules: Array<{
    id: string
    targetPath: DeepKey<DATA>
    sourcePaths: DeepKey<DATA>[]
  }>
}

export interface SideEffects<DATA> {
  syncPaths?: SyncPathConfig<DATA>
  flipPaths?: FlipPathConfig<DATA>
  aggregations?: AggregationConfig<DATA>
  // ... other side effects
}
```

---

## 🧪 Verification Steps

```typescript
test('aggregations: sources equal → target gets value', () => {
  const store = createGenericStore<{ a: number, b: number, target: number | undefined }>()

  function Component() {
    store.useSideEffects('agg', {
      aggregations: {
        rules: [{
          id: 'agg1',
          targetPath: 'target',
          sourcePaths: ['a', 'b']
        }]
      }
    })

    const [a, setA] = store.useStore('a')
    const [b, setB] = store.useStore('b')
    const [target] = store.useStore('target')

    return (
      <div>
        <span>target: {target}</span>
        <button onClick={() => { setA(10); setB(10); }}>Set Both 10</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 5, b: 5, target: undefined }}>
      <Component />
    </store.Provider>
  )

  // Initial: a=5, b=5 → target should be 5
  expect(getByText('target: 5')).toBeInTheDocument()

  // Set both to 10 → target should be 10
  getByText('Set Both 10').click()
  waitFor(() => expect(getByText('target: 10')).toBeInTheDocument())
})

test('aggregations: sources different → target undefined', () => {
  const store = createGenericStore<{ a: number, b: number, target: number | undefined }>()

  function Component() {
    store.useSideEffects('agg', {
      aggregations: {
        rules: [{
          id: 'agg1',
          targetPath: 'target',
          sourcePaths: ['a', 'b']
        }]
      }
    })

    const [, setA] = store.useStore('a')
    const [target] = store.useStore('target')

    return (
      <div>
        <span>target: {String(target)}</span>
        <button onClick={() => setA(20)}>Change A</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 10, b: 10, target: 10 }}>
      <Component />
    </store.Provider>
  )

  // Change A to 20 (now a≠b) → target should be undefined
  getByText('Change A').click()
  waitFor(() => expect(getByText('target: undefined')).toBeInTheDocument())
})

test('aggregations: target change → distribute to sources', () => {
  const store = createGenericStore<{ a: number, b: number, target: number }>()

  function Component() {
    store.useSideEffects('agg', {
      aggregations: {
        rules: [{
          id: 'agg1',
          targetPath: 'target',
          sourcePaths: ['a', 'b']
        }]
      }
    })

    const [a] = store.useStore('a')
    const [b] = store.useStore('b')
    const [, setTarget] = store.useStore('target')

    return (
      <div>
        <span>a: {a}, b: {b}</span>
        <button onClick={() => setTarget(100)}>Set Target 100</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 0, b: 0, target: 0 }}>
      <Component />
    </store.Provider>
  )

  // Set target to 100 → a and b should both become 100
  getByText('Set Target 100').click()
  waitFor(() => expect(getByText('a: 100, b: 100')).toBeInTheDocument())
})

test('aggregations: prevents cycles', () => {
  const graph = new AggregationGraph()
  graph.addAggregation('agg1', 'b', ['a'])

  // Adding a→b→a would create a cycle
  expect(() => {
    graph.addAggregation('agg2', 'a', ['b'])
  }).toThrow(/cycle/)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Recompute all aggregations on every change (only affected ones)
- **DON'T**: Forget cycle detection (will cause infinite loops)
- **DON'T**: Compare objects with `===` (use deep equality if needed)
- **DO**: Optimize value comparisons (cache if possible)
- **DO**: Handle multiple aggregations for same target
- **DO**: Test edge cases: undefined values, empty source arrays

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 10**: `10-validators.md` - Implement Zod validation side-effect
