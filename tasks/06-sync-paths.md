# Phase 4, Task 06: Sync Paths Side-Effect

**Task IDs**: APEX-19, APEX-20, APEX-21
**Priority**: P1 (High) - **PERFORMANCE CRITICAL**
**Dependencies**: Task 05 (Synchronizer Pipeline)
**Phase**: Side Effects - Core

---

## 🎯 Worker Prompt

**YOU ARE**: A graph algorithms specialist
**YOUR FOCUS**: Implement sync paths with efficient dependency graph and transitive sync propagation
**STAY FOCUSED**: Optimize heavily - sync paths run on almost every state change
**SUCCESS MEANS**: Synced paths update together, transitive dependencies work, no cycles, FAST execution

---

## 📋 Task Breakdown

### APEX-19: Design Sync Paths Dependency Graph

Create an efficient graph data structure for tracking sync path dependencies.

**What to do:**
1. Directed graph supporting add/remove nodes and edges
2. Transitive dependency resolution (A→B→C means A,B,C all sync)
3. Cycle detection to prevent infinite loops
4. Fast lookup: "which paths depend on this path"
5. Optimize for read-heavy operations (lookups frequent, modifications rare)

### APEX-20: Implement Sync Paths Registration/Unregistration

Allow registering pairs of paths that should always have the same value.

**What to do:**
1. API: `registerSyncPaths(path1, path2, id)`
2. API: `unregisterSyncPaths(id)`
3. Updates dependency graph on register/unregister
4. Validates no cycles created before accepting registration
5. Store in side-effects registry

### APEX-21: Implement Sync Paths Change Processor (OPTIMIZE HEAVILY)

Process changes to propagate values across synced paths.

**What to do:**
1. When path changes, find all synced paths (direct + transitive)
2. Update all synced paths to the same value
3. Set meta flag `isSyncPathChange: true` for propagated changes
4. Avoid infinite loops (don't sync changes that are already sync changes)
5. **CRITICAL**: Optimize graph traversal, use memoization, batch lookups

---

## ✅ Acceptance Criteria

### APEX-19 Criteria:
- [ ] `SyncPathGraph<DATA>` class exported from `src/sideEffects/syncPaths/graph.ts`
- [ ] Methods: `addEdge(path1, path2)`, `removeEdge(id)`, `getSyncedPaths(path)`
- [ ] Efficient transitive closure computation
- [ ] Cycle detection with clear error messages
- [ ] Fast O(1) or O(log n) lookups for synced paths
- [ ] Tests demonstrate: A→B→C syncs all three

### APEX-20 Criteria:
- [ ] `SyncPathsRegistry` class in `src/sideEffects/syncPaths/registry.ts`
- [ ] Method: `register(id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>): void`
- [ ] Method: `unregister(id: string): void`
- [ ] Throws error on cycle detection
- [ ] Integrates with SyncPathGraph
- [ ] Multiple sync pairs can be registered
- [ ] Test validates cycle prevention

### APEX-21 Criteria:
- [ ] `syncPathsSynchronizer` function in `src/pipeline/synchronizers/syncPaths.ts`
- [ ] Signature: `Synchronizer<DATA, META>`
- [ ] For each change: find all synced paths and add sync changes
- [ ] Meta flag `isSyncPathChange: true` set on propagated changes
- [ ] Skip processing changes that already have `isSyncPathChange: true`
- [ ] **OPTIMIZATION**: Graph traversal cached/memoized
- [ ] **OPTIMIZATION**: Batch path lookups
- [ ] **OPTIMIZATION**: Early exit when no syncs for a path
- [ ] Integrated into pipeline (replace placeholder)
- [ ] Test demonstrates transitive sync works

---

## 📦 Expected Output

### File Structure:

```
src/
  sideEffects/
    syncPaths/
      graph.ts           # SyncPathGraph class
      registry.ts        # SyncPathsRegistry class
      index.ts           # Exports
  pipeline/
    synchronizers/
      syncPaths.ts       # syncPathsSynchronizer function
  store/
    createStore.ts       # Updated to include sync registry
  types/
    sideEffects.ts       # Updated with syncPaths config

tests/
  sideEffects/
    syncPaths/
      graph.test.ts
      registry.test.ts
      synchronizer.test.ts
      integration.test.tsx
```

### src/sideEffects/syncPaths/graph.ts:

```typescript
import type { DeepKey } from '../../types'

/**
 * Efficient graph for tracking sync path dependencies.
 * Optimized for frequent reads (getSyncedPaths) and infrequent writes.
 */
export class SyncPathGraph<DATA extends object> {
  private edges: Map<string, Set<string>> = new Map()
  private edgeIds: Map<string, string> = new Map()
  private transitiveClosure: Map<string, Set<string>> = new Map()
  private closureDirty = false

  /**
   * Add bidirectional sync edge between two paths.
   * @throws Error if adding this edge would create a cycle
   */
  addEdge(
    id: string,
    path1: DeepKey<DATA>,
    path2: DeepKey<DATA>
  ): void {
    // Check for cycles before adding
    if (this.wouldCreateCycle(path1, path2)) {
      throw new Error(
        `Cannot sync "${path1}" and "${path2}": would create a cycle`
      )
    }

    // Add bidirectional edges
    this.addDirectedEdge(path1, path2)
    this.addDirectedEdge(path2, path1)

    // Store ID for later removal
    this.edgeIds.set(id, `${path1}|${path2}`)

    // Mark transitive closure as dirty (recalculate on next read)
    this.closureDirty = true
  }

  /**
   * Remove sync edge by ID
   */
  removeEdge(id: string): void {
    const edgeKey = this.edgeIds.get(id)
    if (!edgeKey) return

    const [path1, path2] = edgeKey.split('|')
    this.removeDirectedEdge(path1, path2)
    this.removeDirectedEdge(path2, path1)

    this.edgeIds.delete(id)
    this.closureDirty = true
  }

  /**
   * Get all paths that should sync with the given path.
   * Returns transitive closure (all directly and indirectly connected paths).
   *
   * PERFORMANCE CRITICAL: This is called on every state change.
   * Uses memoized transitive closure for O(1) lookups.
   */
  getSyncedPaths(path: DeepKey<DATA>): Set<DeepKey<DATA>> {
    if (this.closureDirty) {
      this.recomputeTransitiveClosure()
    }

    return this.transitiveClosure.get(path as string) || new Set()
  }

  private addDirectedEdge(from: string, to: string): void {
    if (!this.edges.has(from)) {
      this.edges.set(from, new Set())
    }
    this.edges.get(from)!.add(to)
  }

  private removeDirectedEdge(from: string, to: string): void {
    this.edges.get(from)?.delete(to)
  }

  private wouldCreateCycle(path1: string, path2: string): boolean {
    // DFS to check if path2 can reach path1
    // If yes, adding path1→path2 would create a cycle
    const visited = new Set<string>()
    const stack = [path2]

    while (stack.length > 0) {
      const current = stack.pop()!
      if (current === path1) return true
      if (visited.has(current)) continue

      visited.add(current)
      const neighbors = this.edges.get(current) || new Set()
      stack.push(...neighbors)
    }

    return false
  }

  private recomputeTransitiveClosure(): void {
    this.transitiveClosure.clear()

    // For each node, compute all reachable nodes via BFS
    for (const start of this.edges.keys()) {
      const reachable = new Set<string>()
      const queue = [start]
      const visited = new Set<string>()

      while (queue.length > 0) {
        const current = queue.shift()!
        if (visited.has(current)) continue

        visited.add(current)
        if (current !== start) {
          reachable.add(current)
        }

        const neighbors = this.edges.get(current) || new Set()
        queue.push(...neighbors)
      }

      this.transitiveClosure.set(start, reachable)
    }

    this.closureDirty = false
  }
}
```

### src/pipeline/synchronizers/syncPaths.ts:

```typescript
import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { SyncPathsRegistry } from '../../sideEffects/syncPaths/registry'

/**
 * Synchronizer for sync paths side-effect.
 *
 * For each change, finds all synced paths and adds changes to sync them.
 * Skips changes that are already sync changes to avoid infinite loops.
 *
 * PERFORMANCE CRITICAL: Heavily optimized for frequent execution.
 */
export function createSyncPathsSynchronizer<
  DATA extends object,
  META extends GenericMeta
>(
  registry: SyncPathsRegistry<DATA>
): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []

    for (const [path, value, meta] of changes) {
      // Skip if this is already a sync change (avoid infinite loops)
      if (meta.isSyncPathChange) {
        continue
      }

      // Get all paths that should sync with this one
      const syncedPaths = registry.graph.getSyncedPaths(path)

      // Add sync changes for all synced paths
      for (const syncedPath of syncedPaths) {
        newChanges.push([
          syncedPath as DeepKey<DATA>,
          value,
          { ...meta, isSyncPathChange: true }
        ])
      }
    }

    return [...changes, ...newChanges]
  }
}
```

### Updated SideEffects type:

```typescript
// src/types/sideEffects.ts
export interface SyncPathConfig<DATA> {
  pairs: Array<{
    id: string
    path1: DeepKey<DATA>
    path2: DeepKey<DATA>
  }>
}

export interface SideEffects<DATA> {
  syncPaths?: SyncPathConfig<DATA>
  // ... other side effects added in later tasks
}
```

---

## 🧪 Verification Steps

### Test Examples:

```typescript
test('sync paths: bidirectional sync', () => {
  const graph = new SyncPathGraph<{ a: number, b: number }>()
  graph.addEdge('test', 'a', 'b')

  const store = createGenericStore<{ a: number, b: number }>()

  function Component() {
    store.useSideEffects('sync', {
      syncPaths: {
        pairs: [{ id: 'ab', path1: 'a', path2: 'b' }]
      }
    })

    const [a, setA] = store.useStore('a')
    const [b] = store.useStore('b')

    return (
      <div>
        <span>a: {a}</span>
        <span>b: {b}</span>
        <button onClick={() => setA(10)}>Set A</button>
      </div>
    )
  }

  const { getByText } = render(
    <store.Provider initialState={{ a: 0, b: 0 }}>
      <Component />
    </store.Provider>
  )

  getByText('Set A').click()

  // Both should update to 10
  expect(getByText('a: 10')).toBeInTheDocument()
  expect(getByText('b: 10')).toBeInTheDocument()
})

test('sync paths: transitive sync A→B→C', () => {
  const graph = new SyncPathGraph<{ a: number, b: number, c: number }>()
  graph.addEdge('ab', 'a', 'b')
  graph.addEdge('bc', 'b', 'c')

  // Changing A should sync B and C
  const syncedWithA = graph.getSyncedPaths('a')
  expect(syncedWithA).toContain('b')
  expect(syncedWithA).toContain('c')
})

test('sync paths: prevents cycles', () => {
  const graph = new SyncPathGraph<{ a: number, b: number, c: number }>()
  graph.addEdge('ab', 'a', 'b')
  graph.addEdge('bc', 'b', 'c')

  // Adding C→A would create a cycle
  expect(() => {
    graph.addEdge('ca', 'c', 'a')
  }).toThrow(/cycle/)
})

test('sync paths: performance with many syncs', () => {
  const graph = new SyncPathGraph<any>()

  // Add 100 sync pairs
  for (let i = 0; i < 100; i++) {
    graph.addEdge(`sync${i}`, `path${i}`, `path${i + 1}`)
  }

  // Lookups should be fast
  const start = performance.now()
  for (let i = 0; i < 1000; i++) {
    graph.getSyncedPaths('path50')
  }
  const duration = performance.now() - start

  // Should complete 1000 lookups in < 10ms
  expect(duration).toBeLessThan(10)
})
```

---

## 🚨 Common Pitfalls

- **DON'T**: Process sync changes again (check `isSyncPathChange` flag)
- **DON'T**: Recompute transitive closure on every lookup (memoize it)
- **DON'T**: Forget cycle detection (will cause infinite loops)
- **DO**: Optimize for read-heavy workload (reads >> writes)
- **DO**: Use efficient data structures (Map, Set)
- **DO**: Test with complex sync chains
- **DO**: Benchmark performance with many sync paths

---

## 💡 Implementation Tips

### Optimization Strategies:

1. **Lazy Transitive Closure**: Only recompute when graph changes
2. **Memoization**: Cache getSyncedPaths results
3. **Early Exit**: If no edges for a path, return immediately
4. **Batch Operations**: Group multiple registrations to avoid repeated recalculations

### Graph Algorithm Choice:

- Use BFS for transitive closure (simpler than Floyd-Warshall)
- Mark closure as dirty on modifications
- Recompute on first read after modification

---

## 📚 Reference Materials

- Graph algorithms: Transitive closure, cycle detection
- TypeScript Map/Set performance characteristics
- Memoization patterns

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 07**: `07-listeners.md` - Implement state change listeners with smart breakdown
