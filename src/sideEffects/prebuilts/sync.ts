import type { PathGroups } from '../../core/pathGroups'
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

/**
 * Collects sync changes needed to align a group of paths to the most common value.
 * Returns changes array (may be empty if no sync needed).
 */
const collectGroupSyncChanges = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  component: string[],
): ArrayOfChanges<DATA, META> => {
  // Count value occurrences (excluding null/undefined)
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

  // Collect divergent changes
  const changes: ArrayOfChanges<DATA, META> = []
  if (is.not.undefined(mostCommonValue)) {
    for (const path of component) {
      const currentValue = dot.get__unsafe(store.state, path)
      if (currentValue !== mostCommonValue) {
        changes.push([
          path,
          mostCommonValue,
          { isSyncPathChange: true },
        ] as ArrayOfChanges<DATA, META>[number])
      }
    }
  }

  return changes
}

/**
 * Creates an edge cleanup function for a sync pair.
 */
const makeSyncEdgeCleanup =
  (sync: PathGroups, path1: string, path2: string): (() => void) =>
  () => {
    if (hasEdge(sync, path1, path2)) {
      removeEdge(sync, path1, path2)
    }
    if (hasPath(sync, path1) && getPathDegree(sync, path1) === 0) {
      removeEdge(sync, path1, path1)
    }
    if (hasPath(sync, path2) && getPathDegree(sync, path2) === 0) {
      removeEdge(sync, path2, path2)
    }
  }

/**
 * Batch version of registerSyncPair. Adds all edges first, then computes
 * initial sync changes across all final groups and calls processChanges once.
 * This avoids cascading effect re-evaluations when registering many pairs.
 */
export const registerSyncPairsBatch = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  pairs: [string & {}, string & {}][],
): (() => void) => {
  const { sync } = store._internal.graphs
  const edgeCleanups: (() => void)[] = []

  // Phase 1: Add ALL edges without calling processChanges
  for (const [path1, path2] of pairs) {
    addEdge(sync, path1, path2)
    edgeCleanups.push(makeSyncEdgeCleanup(sync, path1, path2))
  }

  // Phase 2: Iterate final groups (deduplicate via pathToGroup)
  const processedGroups = new Set<number>()
  const allChanges: ArrayOfChanges<DATA, META> = []

  for (const [path1] of pairs) {
    const groupId = sync.pathToGroup.get(path1)
    if (groupId === undefined || processedGroups.has(groupId)) continue
    processedGroups.add(groupId)

    const component = getGroupPaths(sync, path1)
    const changes = collectGroupSyncChanges(store, component)
    allChanges.push(...changes)
  }

  // Phase 3: Single processChanges call with all accumulated changes
  if (allChanges.length > 0) {
    processChanges(store, allChanges)
  }

  return () => edgeCleanups.forEach((fn) => fn())
}

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

  // Find all paths in this sync group and apply sync changes
  const component = getGroupPaths(sync, path1)
  const changes = collectGroupSyncChanges(store, component)
  if (changes.length > 0) {
    processChanges(store, changes)
  }

  return makeSyncEdgeCleanup(sync, path1, path2)
}
