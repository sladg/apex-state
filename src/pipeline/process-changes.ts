/**
 * Change Processing Pipeline
 *
 * Routes changes through WASM for aggregation, sync, flip, and BoolLogic.
 * Listener dispatch uses WASM-created dispatch plans with JS handler execution.
 *
 * Falls back to JS processors when WASM is not loaded.
 */

import { snapshot } from 'valtio'

import type { DebugTrackEntry, StoreInstance } from '../core/types'
import type { ArrayOfChanges, GenericMeta } from '../types'
import { applyBatch } from './apply-batch'
import {
  processAggregationWrites,
  processFlipPaths,
  processListeners,
  processSyncPaths,
} from './processors'

// ---------------------------------------------------------------------------
// JS fallback pipeline (used when WASM is not loaded)
// ---------------------------------------------------------------------------

const processChangesJS = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  const { processing } = store._internal

  // Use queue as the mutable batch - processors write to it directly
  processing.queue = [...initialChanges]

  // Get current state snapshot for processing
  const currentState = snapshot(store.state) as DATA

  // Single-pass sequential processing - order matters
  // 1. Aggregation writes: intercept writes to targets, distribute to sources
  processAggregationWrites(processing.queue, store)

  // 2. Sync: bidirectional path synchronization
  processSyncPaths(processing.queue, store)

  // 3. Flip: invert boolean values across paired paths
  processFlipPaths(processing.queue, store)

  // 4. Listeners: reactive side effects
  processListeners(processing.queue, store, currentState)

  // Record debug tracking before applying
  if (store._debug) {
    const trackEntry: DebugTrackEntry = {
      input: initialChanges.map(([p, v, m]) => [p as string, v, m]),
      applied: processing.queue.map(([path, value]) => ({
        path: path as string,
        value,
      })),
      appliedConcerns: [],
      timestamp: Date.now(),
    }
    store._debug.calls.push(trackEntry)
  }

  // Apply all accumulated changes to state once
  applyBatch(processing.queue, store.state)
}

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

// Export individual processors for explicit use
export const processChangesLegacy = processChangesJS

export const processChanges = processChangesLegacy
