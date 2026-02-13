---
title: Graphology Replacement with PathGroups
audience: maintainers
created: 2026-02-05 (aee8140)
updated: 2026-02-05 (aee8140)
status: archived
---

# Graphology Replacement with PathGroups

## Problem Statement

The original implementation used [graphology](https://graphology.github.io/) for tracking sync/flip path relationships. While graphology is a powerful graph library, it has a critical performance issue for our use case:

**`connectedComponents(graph)` is O(V+E) and was called on every change.**

This means for every state change, the entire graph was traversed to compute connected components. In batch operations with many sync/flip relationships, this became a significant bottleneck.

### Locations where O(V+E) recomputation occurred:
- `src/pipeline/processors/syncPaths.ts:23` - on every sync path change
- `src/pipeline/processors/flipPaths.ts:23` - on every flip path change
- `src/sideEffects/prebuilts/sync.ts:30` - on registration

## Solution: PathGroups Data Structure

We replaced graphology with a custom `PathGroups` data structure that maintains connected components incrementally, providing O(1) lookups instead of O(V+E) recomputation.

### Data Structure

```typescript
interface PathGroups {
  pathToGroup: Map<string, number>       // O(1) lookup: path -> group ID
  groupToPaths: Map<number, Set<string>> // O(1) lookup: group ID -> all paths
  edges: Set<string>                     // Edge existence tracking
  adjacency: Map<string, Set<string>>    // For split detection on removal
  nextGroupId: number
}
```

### Complexity Comparison

| Operation | Before (graphology) | After (PathGroups) |
|-----------|--------------------|--------------------|
| Get all groups | O(V+E) | O(G) where G = num groups |
| Get group for path | O(V+E) | O(1) |
| Check same group | O(V+E) | O(1) |
| Add edge | O(1) | O(n) merge, typically small |
| Remove edge | O(1) | O(n) split check |
| Has path | O(1) | O(1) |
| Has edge | O(1) | O(1) |

### Key Insight

For change processing (the hot path), `getAllGroups()` drops from O(V+E) to O(G) where G is the number of connected components. Since G is typically much smaller than V+E, this provides significant performance improvement.

The trade-off is that `addEdge` now does O(n) work when merging groups, but:
1. Edge additions are rare compared to change processing
2. The merge operation is still fast in practice

## API Reference

### `createPathGroups(): PathGroups`
Creates a new empty PathGroups instance.

### `addEdge(groups, path1, path2): void`
Adds an edge between two paths. Handles:
- Creating new group if both paths are new
- Adding path to existing group
- Merging groups (smaller into larger for efficiency)

### `removeEdge(groups, path1, path2): void`
Removes an edge. Handles:
- Removing isolated nodes
- Detecting and handling component splits via BFS

### `getAllGroups(groups): string[][]`
Returns all connected components as arrays of paths. O(G) complexity.

### `getGroupPaths(groups, path): string[]`
Returns all paths in the same connected component. O(1) lookup.

### `hasPath(groups, path): boolean`
Checks if a path exists. O(1).

### `hasEdge(groups, path1, path2): boolean`
Checks if an edge exists between two paths. O(1).

### `getPathDegree(groups, path): number`
Returns the number of edges connected to a path. O(1).

## Files Changed

1. **New**: `src/core/pathGroups.ts` - PathGroups implementation
2. **Modified**: `src/core/graphTypes.ts` - Type aliases for backward compatibility
3. **Modified**: `src/store/Provider.tsx` - Use `createPathGroups()` instead of `new Graph()`
4. **Modified**: `src/pipeline/processors/syncPaths.ts` - Use `getAllGroups()` instead of `connectedComponents()`
5. **Modified**: `src/pipeline/processors/flipPaths.ts` - Same as above
6. **Modified**: `src/sideEffects/prebuilts/sync.ts` - Use PathGroups operations
7. **Modified**: `src/sideEffects/prebuilts/flip.ts` - Use PathGroups operations
8. **Modified**: `tests/benchmarking/pipeline.bench.spec.ts` - Use PathGroups in mock store
9. **Modified**: `package.json` - Removed graphology dependencies
10. **Modified**: `tsup.config.ts` - Removed graphology from externals

## Dependencies Removed

- `graphology` (production dependency)
- `graphology-components` (production dependency)
- `graphology-types` (dev dependency)

This reduces the bundle size and eliminates external graph library dependencies.
