/**
 * Sync Paths Synchronizer
 *
 * For each change, finds all synced paths and adds changes to sync them.
 * Skips changes that are already sync changes to avoid infinite loops.
 *
 * PERFORMANCE CRITICAL: Heavily optimized for frequent execution.
 */

import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { SyncPathsRegistry } from '../../sideEffects/syncPaths/registry'

/**
 * Create a synchronizer for sync paths side-effect.
 *
 * @param registry - The SyncPathsRegistry instance managing sync relationships
 * @returns A synchronizer function that propagates changes across synced paths
 */
export const createSyncPathsSynchronizer = <
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(registry: SyncPathsRegistry<DATA>): Synchronizer<DATA, META> => {
  return (changes, _state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []

    for (const [path, value, meta] of changes) {
      // Skip if this is already a sync change (avoid infinite loops)
      if (meta.isSyncPathChange) {
        continue
      }

      // Get all paths that should sync with this one
      const syncedPaths = registry.graph.getSyncedPaths(path)

      // Add sync changes for all synced paths
      for (const syncedPath of syncedPaths) {
        newChanges.push([
          syncedPath as DeepKey<DATA>,
          value,
          { ...meta, isSyncPathChange: true } as META,
        ])
      }
    }

    return [...changes, ...newChanges]
  }
}