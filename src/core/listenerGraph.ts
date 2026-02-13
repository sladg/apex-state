/**
 * ListenerGraph Data Structure
 *
 * Manages listener groups with pre-computed routing edges.
 * Groups are sorted deepest-first; edges connect each group
 * to all downstream (shallower) groups for change routing.
 *
 * Operations:
 * - createListenerGraph(): O(1) — empty graph
 * - addGroup(graph, path): O(G log G) — insert + re-sort + recompute edges
 * - removeGroup(graph, path): O(G) — remove + recompute edges
 * - addNode(graph, id, scope, groupPath): O(1)
 * - removeNode(graph, id, groupPath): O(M) where M = members in group
 */

import type {
  ListenerGraph,
  ListenerRoute,
} from '../pipeline/processors/listeners.types'
import { getPathDepth } from '../utils/pathUtils'

/**
 * Creates a new empty ListenerGraph instance.
 */
export const createListenerGraph = (): ListenerGraph => ({
  order: [],
  groupMeta: new Map(),
  groupMembers: new Map(),
  nodes: new Map(),
  edges: new Map(),
  fns: new Map(),
})

/**
 * Recompute all routing edges based on current order.
 * For each group at index i, edges point to ALL groups at index > i.
 */
const recomputeEdges = (graph: ListenerGraph): void => {
  graph.edges.clear()
  for (const [i, groupPath] of graph.order.entries()) {
    const routes: ListenerRoute[] = graph.order
      .slice(i + 1)
      .map((targetPath) => {
        const meta = graph.groupMeta.get(targetPath)!
        return {
          target: targetPath,
          prefix: meta.prefix,
          prefixLen: meta.prefixLen,
          depth: meta.depth,
        }
      })
    graph.edges.set(groupPath, routes)
  }
}

/**
 * Add a group for the given path if it doesn't exist.
 * Re-sorts order and recomputes edges.
 */
export const addGroup = (graph: ListenerGraph, path: string): void => {
  if (graph.groupMeta.has(path)) return

  const depth = getPathDepth(path)
  graph.groupMeta.set(path, {
    prefix: depth === 0 ? '' : path + '.',
    prefixLen: depth === 0 ? 0 : path.length + 1,
    depth,
  })
  graph.groupMembers.set(path, [])
  graph.order.push(path)

  // Sort by depth DESC (stable sort preserves insertion order within same depth)
  graph.order.sort((a, b) => {
    const metaA = graph.groupMeta.get(a)!
    const metaB = graph.groupMeta.get(b)!
    return metaB.depth - metaA.depth
  })
  recomputeEdges(graph)
}

/**
 * Add groups for multiple paths at once, deferring sort + edge recomputation
 * to after all groups are inserted. O(G log G) once instead of per-path.
 */
export const addGroups = (graph: ListenerGraph, paths: string[]): void => {
  let added = false
  for (const path of paths) {
    if (graph.groupMeta.has(path)) continue

    const depth = getPathDepth(path)
    graph.groupMeta.set(path, {
      prefix: depth === 0 ? '' : path + '.',
      prefixLen: depth === 0 ? 0 : path.length + 1,
      depth,
    })
    graph.groupMembers.set(path, [])
    graph.order.push(path)
    added = true
  }

  if (added) {
    graph.order.sort((a, b) => {
      const metaA = graph.groupMeta.get(a)!
      const metaB = graph.groupMeta.get(b)!
      return metaB.depth - metaA.depth
    })
    recomputeEdges(graph)
  }
}

/**
 * Remove a group entirely (only if it has no members).
 * Recomputes edges after removal.
 */
export const removeGroup = (graph: ListenerGraph, path: string): void => {
  graph.groupMeta.delete(path)
  graph.groupMembers.delete(path)
  const orderIndex = graph.order.indexOf(path)
  if (orderIndex !== -1) {
    graph.order.splice(orderIndex, 1)
  }
  recomputeEdges(graph)
}

/**
 * Add a listener node to a group.
 */
export const addNode = (
  graph: ListenerGraph,
  id: string,
  scope: string | null,
  groupPath: string,
): void => {
  graph.nodes.set(id, { scope, groupPath })
  graph.groupMembers.get(groupPath)!.push(id)
}

/**
 * Remove a listener node from its group.
 * Returns true if the group is now empty (caller should remove it).
 */
export const removeNode = (
  graph: ListenerGraph,
  id: string,
  groupPath: string,
): boolean => {
  graph.nodes.delete(id)

  const members = graph.groupMembers.get(groupPath)
  if (members) {
    const idx = members.indexOf(id)
    if (idx !== -1) members.splice(idx, 1)
    return members.length === 0
  }
  return false
}
