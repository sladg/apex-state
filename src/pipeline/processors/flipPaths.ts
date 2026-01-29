/**
 * Flip Path Processor
 *
 * Processes flip path side effects - inverts boolean values across paired paths.
 * Uses normalizeChangesForGroups to handle exact, parent, and child changes.
 */

import { connectedComponents } from 'graphology-components'

import type { StoreInstance } from '../../core/types'
import type { ArrayOfChanges, GenericMeta } from '../../types'
import { AnyChange, normalizeChangesForGroups } from '../normalizeChanges'
import { queueChange } from '../queue'

export const processFlipPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
): void => {
  const { flip } = store._internal.graphs
  const { queue } = store._internal.processing

  // Get connected components (groups of flipped paths)
  const pathGroups = connectedComponents(flip)
  if (pathGroups.length === 0) return

  // Normalize changes for grouped paths
  const normalizedChanges = normalizeChangesForGroups({
    changes: changes as AnyChange[],
    pathGroups,
    matchMode: 'all',
  })

  // Apply each normalized change to all neighbors (with flipped value)
  for (const match of normalizedChanges) {
    // Flip paths only work with boolean values
    if (typeof match.value !== 'boolean') continue

    const meta = { isFlipPathChange: true, ...match.meta }

    // Flip to all paths in the connected group except the matched one
    for (const neighborPath of match.connectedPaths) {
      if (neighborPath === match.matchedPath) continue

      // Build target path: neighbor + relativePath (if child change)
      const targetPath = match.relativePath
        ? `${neighborPath}.${match.relativePath}`
        : neighborPath

      queueChange({ queue, path: targetPath, value: !match.value, meta })
    }
  }
}
