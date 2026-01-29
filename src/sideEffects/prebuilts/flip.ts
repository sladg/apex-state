import type { StoreInstance } from '../../core/types'
import type { GenericMeta } from '../../types'

export const registerFlipPair = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path1: string & {},
  path2: string & {},
): (() => void) => {
  const { flip } = store._internal.graphs

  // Add nodes if they don't exist
  if (!flip.hasNode(path1)) flip.addNode(path1)
  if (!flip.hasNode(path2)) flip.addNode(path2)

  // Add edge if it doesn't exist
  if (!flip.hasEdge(path1, path2)) {
    flip.addEdge(path1, path2)
  }

  return () => {
    // Remove edge
    if (flip.hasEdge(path1, path2)) {
      flip.dropEdge(path1, path2)
    }
    // Remove isolated nodes
    if (flip.hasNode(path1) && flip.degree(path1) === 0) flip.dropNode(path1)
    if (flip.hasNode(path2) && flip.degree(path2) === 0) flip.dropNode(path2)
  }
}
