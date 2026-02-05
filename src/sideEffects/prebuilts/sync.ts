import {
  addEdge,
  getGroupPaths,
  getPathDegree,
  hasEdge,
  hasPath,
  removeEdge,
} from '../../core/pathGroups'
import type { StoreInstance } from '../../core/types'
import { processChanges } from '../../pipeline/processChanges'
import type { ArrayOfChanges, GenericMeta } from '../../types'
import { dot } from '../../utils/dot'
import { is } from '../../utils/is'

export const registerSyncPair = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  path1: string & {},
  path2: string & {},
): (() => void) => {
  const { sync } = store._internal.graphs

  // Add edge (implicitly adds nodes if they don't exist)
  addEdge(sync, path1, path2)

  // Find all paths in this sync group
  const component = getGroupPaths(sync, path1)

  // Get values and count occurrences (excluding null/undefined)
  const valueCounts = new Map<unknown, number>()
  for (const path of component) {
    const value = dot.get__unsafe(store.state, path)
    if (is.not.nil(value)) {
      const count = valueCounts.get(value) ?? 0
      valueCounts.set(value, count + 1)
    }
  }

  // Find most common value
  let mostCommonValue: unknown = undefined
  let maxCount = 0
  for (const [value, count] of valueCounts) {
    if (count > maxCount) {
      maxCount = count
      mostCommonValue = value
    }
  }

  // Sync all paths to most common value (if one exists)
  if (is.not.undefined(mostCommonValue)) {
    const changes: ArrayOfChanges<DATA, META> = []
    for (const path of component) {
      const currentValue = dot.get__unsafe(store.state, path)
      if (currentValue !== mostCommonValue) {
        // Type cast needed: path is validated at registration, isSyncPathChange is GenericMeta property
        changes.push([
          path,
          mostCommonValue,
          { isSyncPathChange: true },
        ] as ArrayOfChanges<DATA, META>[number])
      }
    }
    if (changes.length > 0) {
      processChanges(store, changes)
    }
  }

  return () => {
    // Remove edge
    if (hasEdge(sync, path1, path2)) {
      removeEdge(sync, path1, path2)
    }
    // Note: removeEdge handles isolated node cleanup automatically
    // But we check explicitly for paths that might have other connections
    if (hasPath(sync, path1) && getPathDegree(sync, path1) === 0) {
      removeEdge(sync, path1, path1) // This is a no-op but keeps the pattern
    }
    if (hasPath(sync, path2) && getPathDegree(sync, path2) === 0) {
      removeEdge(sync, path2, path2) // This is a no-op but keeps the pattern
    }
  }
}
