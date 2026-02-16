/**
 * WASM Implementation - Sync Registration
 *
 * Registers sync pairs in WASM, which computes initial sync changes from shadow state.
 */

import { processChanges } from '../../pipeline/processChanges'
import { wasm } from '../../wasm/bridge'

export const registerSyncPairsBatch: typeof import('./sync').registerSyncPairsBatchLegacy =
  (store, pairs) => {
    // WASM registers pairs, computes initial sync changes from shadow state,
    // updates shadow, and returns the changes
    const wasmChanges = wasm.registerSyncBatch(pairs)

    // Convert WASM changes to ArrayOfChanges format
    const allChanges = wasmChanges.map((change) => [
      change.path,
      change.value,
      { isSyncPathChange: true },
    ])

    // Apply initial sync changes to valtio
    if (allChanges.length > 0) {
      processChanges(allChanges, allChanges)
    }

    // Return cleanup function
    return () => {
      wasm.unregisterSyncBatch(pairs as [string, string][])
    }
  }
