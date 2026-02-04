import { connectedComponents } from 'graphology-components'

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

  // Add nodes if they don't exist
  if (!sync.hasNode(path1)) sync.addNode(path1)
  if (!sync.hasNode(path2)) sync.addNode(path2)

  // Add edge if it doesn't exist
  const edgeKey = `${path1}--${path2}`
  if (!sync.hasEdge(path1, path2)) {
    sync.addEdge(path1, path2, { key: edgeKey })
  }

  // Find all paths in this sync group using connected components
  const components = connectedComponents(sync)
  const component = components.find((c) => c.includes(path1)) ?? [path1, path2]

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
    if (sync.hasEdge(path1, path2)) {
      sync.dropEdge(path1, path2)
    }
    // Remove isolated nodes
    if (sync.hasNode(path1) && sync.degree(path1) === 0) sync.dropNode(path1)
    if (sync.hasNode(path2) && sync.degree(path2) === 0) sync.dropNode(path2)
  }
}
