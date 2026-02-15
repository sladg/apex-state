/**
 * PathGroups Data Structure
 *
 * A custom data structure that provides O(1) connected component lookups
 * instead of O(V+E) recomputation on every change like graphology's connectedComponents.
 *
 * When wasmGraphType is set ('sync' | 'flip'), addEdge/removeEdge automatically
 * mirror registrations to WASM bridge for pipeline processing.
 *
 * Operations complexity:
 * - getAllGroups(): O(G) where G is number of groups (typically small)
 * - getGroupPaths(path): O(1) lookup
 * - addEdge(p1, p2): O(n) where n is smaller group size (merge)
 * - removeEdge(p1, p2): O(n) for affected component
 * - hasPath(path): O(1)
 * - hasEdge(p1, p2): O(1)
 */

import {
  isWasmLoaded,
  registerFlipBatch,
  registerSyncBatch,
  unregisterFlipBatch,
  unregisterSyncBatch,
} from '../wasm/bridge'

/** Graph type for WASM mirroring. When set, addEdge/removeEdge mirror to WASM. */
export type WasmGraphType = 'sync' | 'flip'

/**
 * Creates a canonical edge key for consistent lookups.
 * Sorts paths alphabetically to ensure addEdge(a,b) and addEdge(b,a) produce same key.
 */
const makeEdgeKey = (path1: string, path2: string): string =>
  path1 < path2 ? `${path1}--${path2}` : `${path2}--${path1}`

/**
 * PathGroups maintains connected components with O(1) lookups.
 *
 * Internal structure:
 * - pathToGroup: Maps each path to its group ID
 * - groupToPaths: Maps each group ID to the set of paths in that group
 * - edges: Set of edge keys for edge existence checks and proper removal
 * - adjacency: Maps each path to its direct neighbors (for split detection on removal)
 * - nextGroupId: Counter for generating unique group IDs
 */
export interface PathGroups {
  pathToGroup: Map<string, number>
  groupToPaths: Map<number, Set<string>>
  edges: Set<string>
  adjacency: Map<string, Set<string>>
  nextGroupId: number
  wasmGraphType?: WasmGraphType
}

/**
 * Creates a new empty PathGroups instance.
 */
export const createPathGroups = (
  wasmGraphType?: WasmGraphType,
): PathGroups => ({
  pathToGroup: new Map(),
  groupToPaths: new Map(),
  edges: new Set(),
  adjacency: new Map(),
  nextGroupId: 0,
  wasmGraphType,
})

/**
 * BFS traversal from a starting path, collecting all reachable paths.
 */
const bfsCollect = (
  adjacency: Map<string, Set<string>>,
  startPath: string,
): Set<string> => {
  const visited = new Set<string>([startPath])
  const queue = [startPath]

  while (queue.length > 0) {
    const current = queue.shift()!
    const neighbors = adjacency.get(current)
    if (!neighbors) continue

    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        visited.add(neighbor)
        queue.push(neighbor)
      }
    }
  }

  return visited
}

/**
 * Removes an isolated path (one with no edges) from the data structure.
 */
const removeIsolatedPath = (groups: PathGroups, path: string): void => {
  const groupId = groups.pathToGroup.get(path)
  if (groupId !== undefined) {
    groups.groupToPaths.get(groupId)?.delete(path)
    if (groups.groupToPaths.get(groupId)?.size === 0) {
      groups.groupToPaths.delete(groupId)
    }
  }
  groups.pathToGroup.delete(path)
  groups.adjacency.delete(path)
}

/**
 * Handles component split after edge removal.
 * If path2 is no longer reachable from path1, creates a new group for path2's component.
 */
const handleComponentSplit = (
  groups: PathGroups,
  path1: string,
  path2: string,
): void => {
  // BFS from path1 to see if we can reach path2
  const reachableFromPath1 = bfsCollect(groups.adjacency, path1)

  // If path2 is reachable, no split occurred
  if (reachableFromPath1.has(path2)) return

  // Component split: create new group for path2's component
  const newGroupId = groups.nextGroupId++
  const oldGroupId = groups.pathToGroup.get(path2)!
  const newComponent = bfsCollect(groups.adjacency, path2)

  // Move paths to new group
  for (const path of newComponent) {
    groups.pathToGroup.set(path, newGroupId)
    groups.groupToPaths.get(oldGroupId)?.delete(path)
  }
  groups.groupToPaths.set(newGroupId, newComponent)

  // Clean up empty old group
  if (groups.groupToPaths.get(oldGroupId)?.size === 0) {
    groups.groupToPaths.delete(oldGroupId)
  }
}

/** Mirror an edge addition to WASM bridge if graph type is set. */
const mirrorAddToWasm = (
  wasmGraphType: WasmGraphType | undefined,
  path1: string,
  path2: string,
): void => {
  if (!wasmGraphType || !isWasmLoaded()) return
  if (wasmGraphType === 'sync') {
    registerSyncBatch([[path1, path2]])
  } else {
    registerFlipBatch([[path1, path2]])
  }
}

/** Mirror an edge removal to WASM bridge if graph type is set. */
const mirrorRemoveFromWasm = (
  wasmGraphType: WasmGraphType | undefined,
  path1: string,
  path2: string,
): void => {
  if (!wasmGraphType || !isWasmLoaded()) return
  if (wasmGraphType === 'sync') {
    unregisterSyncBatch([[path1, path2]])
  } else {
    unregisterFlipBatch([[path1, path2]])
  }
}

/**
 * Adds an edge between two paths.
 * If paths are in different groups, merges them (smaller into larger).
 * If either path is new, adds it to the appropriate group.
 */
export const addEdge = (
  groups: PathGroups,
  path1: string,
  path2: string,
): void => {
  const edgeKey = makeEdgeKey(path1, path2)

  // Skip if edge already exists
  if (groups.edges.has(edgeKey)) return

  // Mirror to WASM before local update
  mirrorAddToWasm(groups.wasmGraphType, path1, path2)

  // Add edge
  groups.edges.add(edgeKey)

  // Update adjacency
  if (!groups.adjacency.has(path1)) groups.adjacency.set(path1, new Set())
  if (!groups.adjacency.has(path2)) groups.adjacency.set(path2, new Set())
  groups.adjacency.get(path1)!.add(path2)
  groups.adjacency.get(path2)!.add(path1)

  const g1 = groups.pathToGroup.get(path1)
  const g2 = groups.pathToGroup.get(path2)

  if (g1 === undefined && g2 === undefined) {
    // Both new - create new group
    const id = groups.nextGroupId++
    groups.pathToGroup.set(path1, id)
    groups.pathToGroup.set(path2, id)
    groups.groupToPaths.set(id, new Set([path1, path2]))
  } else if (g1 !== undefined && g2 === undefined) {
    // Add path2 to path1's group
    groups.pathToGroup.set(path2, g1)
    groups.groupToPaths.get(g1)!.add(path2)
  } else if (g1 === undefined && g2 !== undefined) {
    // Add path1 to path2's group
    groups.pathToGroup.set(path1, g2)
    groups.groupToPaths.get(g2)!.add(path1)
  } else if (g1 !== g2) {
    // Merge groups - move smaller into larger
    const set1 = groups.groupToPaths.get(g1!)!
    const set2 = groups.groupToPaths.get(g2!)!
    const [smallerId, smallerSet, largerId, largerSet] =
      set1.size < set2.size ? [g1!, set1, g2!, set2] : [g2!, set2, g1!, set1]

    for (const path of smallerSet) {
      groups.pathToGroup.set(path, largerId)
      largerSet.add(path)
    }
    groups.groupToPaths.delete(smallerId)
  } else {
    // g1 === g2 (both defined, same group) - paths already connected, no-op
  }
}

/**
 * Removes an edge between two paths.
 * If removal splits the component, creates new groups via BFS.
 * If a path becomes isolated (no edges), removes it entirely.
 */
export const removeEdge = (
  groups: PathGroups,
  path1: string,
  path2: string,
): void => {
  const edgeKey = makeEdgeKey(path1, path2)

  // Skip if edge doesn't exist
  if (!groups.edges.has(edgeKey)) return

  // Mirror to WASM before local update
  mirrorRemoveFromWasm(groups.wasmGraphType, path1, path2)

  // Remove edge
  groups.edges.delete(edgeKey)

  // Update adjacency
  groups.adjacency.get(path1)?.delete(path2)
  groups.adjacency.get(path2)?.delete(path1)

  // Check for isolated nodes
  const adj1 = groups.adjacency.get(path1)
  const adj2 = groups.adjacency.get(path2)
  const path1Isolated = !adj1 || adj1.size === 0
  const path2Isolated = !adj2 || adj2.size === 0

  if (path1Isolated) removeIsolatedPath(groups, path1)
  if (path2Isolated) removeIsolatedPath(groups, path2)

  // If both paths still have edges, check if component split
  if (!path1Isolated && !path2Isolated) {
    handleComponentSplit(groups, path1, path2)
  }
}

/**
 * Returns all connected components as arrays of paths.
 * O(G) where G is number of groups.
 */
export const getAllGroups = (groups: PathGroups): string[][] =>
  [...groups.groupToPaths.values()].map((set) => [...set])

/**
 * Returns all paths in the same connected component as the given path.
 * O(1) lookup.
 */
export const getGroupPaths = (groups: PathGroups, path: string): string[] => {
  const groupId = groups.pathToGroup.get(path)
  if (groupId === undefined) return []
  return [...groups.groupToPaths.get(groupId)!]
}

/**
 * Checks if a path exists in the structure.
 * O(1) lookup.
 */
export const hasPath = (groups: PathGroups, path: string): boolean =>
  groups.pathToGroup.has(path)

/**
 * Checks if an edge exists between two paths.
 * O(1) lookup.
 */
export const hasEdge = (
  groups: PathGroups,
  path1: string,
  path2: string,
): boolean => groups.edges.has(makeEdgeKey(path1, path2))

/**
 * Returns the degree (number of edges) for a path.
 * O(1) lookup.
 */
export const getPathDegree = (groups: PathGroups, path: string): number =>
  groups.adjacency.get(path)?.size ?? 0
