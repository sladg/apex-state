/**
 * Change Processing Pipeline
 *
 * Single-pass pipeline: change → aggregation → sync → flip → listeners → done
 * No reprocessing - each processor runs once on all accumulated changes
 */

import { snapshot } from 'valtio'

import type { StoreInstance } from '~/core/types'
import type { GenericMeta } from '~/types'

import type { ChangeTuple } from '../types/changes'
import { applyBatch } from './applyBatch'
import {
  processAggregationWrites,
  processFlipPaths,
  processListeners,
  processSyncPaths,
} from './processors'

export const processChanges = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ChangeTuple,
): void => {
  const { processing } = store._internal

  try {
    // Use queue as the mutable batch - processors write to it directly
    processing.queue = [...initialChanges]

    // Get current state snapshot for processing
    const currentState = snapshot(store.state) as DATA

    // Single-pass sequential processing - order matters
    // Each processor mutates the queue in place, adding new changes to the end
    // 1. Aggregation writes: intercept writes to targets, distribute to sources
    processAggregationWrites(processing.queue, store)

    // 2. Sync: bidirectional path synchronization
    processSyncPaths(processing.queue, store)

    // 3. Flip: invert boolean values across paired paths
    processFlipPaths(processing.queue, store)

    // 4. Listeners: reactive side effects
    processListeners(processing.queue, store, currentState)

    // Apply all accumulated changes to state once
    applyBatch(processing.queue, store.state)
  } catch {
    // @TODO: Add logging/error
  }
}
