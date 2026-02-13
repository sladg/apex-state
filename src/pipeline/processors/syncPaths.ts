/**
 * Sync Path Processor
 *
 * Processes sync path side effects - synchronizes values across paired paths.
 * Uses normalizeChangesForGroups to handle exact, parent, and child changes.
 */

import { getAllGroups } from '../../core/pathGroups'
import type { StoreInstance } from '../../core/types'
import type { GenericMeta } from '../../types'
import type { ArrayOfChanges__internal } from '../../types/changes'
import { normalizeChangesForGroups } from '../normalizeChanges'
import { queueChange } from '../queue'

export const processSyncPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges__internal<DATA, META>,
  store: StoreInstance<DATA, META>,
): void => {
  const { sync } = store._internal.graphs
  const { queue } = store._internal.processing

  // Get connected components (groups of synced paths) - O(1) with PathGroups
  const pathGroups = getAllGroups(sync)
  if (pathGroups.length === 0) return

  // Normalize changes for grouped paths
  const normalizedChanges = normalizeChangesForGroups({
    changes,
    pathGroups,
    matchMode: 'all',
  })

  // Apply each normalized change to all neighbors
  for (const match of normalizedChanges) {
    const meta = { isSyncPathChange: true, ...match.meta }

    // Sync to all paths in the connected group except the matched one
    for (const neighborPath of match.connectedPaths) {
      if (neighborPath === match.matchedPath) continue

      // Build target path: neighbor + relativePath (if child change)
      const targetPath = match.relativePath
        ? `${neighborPath}.${match.relativePath}`
        : neighborPath

      queueChange({ queue, path: targetPath, value: match.value, meta })
    }
  }
}
