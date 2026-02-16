import {
  addEdge,
  getPathDegree,
  hasEdge,
  hasPath,
  removeEdge,
} from '../../core/path-groups'
import type { StoreInstance } from '../../core/types'
import type { GenericMeta } from '../../types'

/** Legacy JS implementation - uses JS graph structure */
export const registerFlipPair = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path1: string & {},
  path2: string & {},
): (() => void) => {
  const { flip } = store._internal.graphs

  // Add edge (implicitly adds nodes if they don't exist)
  addEdge(flip, path1, path2)

  return () => {
    // Remove edge
    if (hasEdge(flip, path1, path2)) {
      removeEdge(flip, path1, path2)
    }
    // Note: removeEdge handles isolated node cleanup automatically
    // But we check explicitly for paths that might have other connections
    if (hasPath(flip, path1) && getPathDegree(flip, path1) === 0) {
      removeEdge(flip, path1, path1) // This is a no-op but keeps the pattern
    }
    if (hasPath(flip, path2) && getPathDegree(flip, path2) === 0) {
      removeEdge(flip, path2, path2) // This is a no-op but keeps the pattern
    }
  }
}
