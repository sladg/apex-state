/**
 * Change processor with sequential side-effect execution
 *
 * Simple, linear processing of changes through side-effect graphs.
 * No factory pattern - just direct function calls in sequence.
 */

import _set from 'lodash/set'
import { snapshot } from 'valtio'

import type { ArrayOfChanges, GenericMeta } from '../types'
import type { FlushQueue, StoreInstance } from './types'

/**
 * Symbol used to identify queue flush exceptions
 * @internal
 */
const FLUSH_QUEUE = Symbol('FLUSH_QUEUE')

/**
 * Queue a change for processing.
 *
 * TYPE BOUNDARY: Graph operations (sync, flip, aggregation) store paths as strings,
 * losing compile-time type information. This helper centralizes the type assertion
 * needed to push runtime paths back into the typed ArrayOfChanges queue.
 *
 * Type safety is ensured at registration time when paths are validated against DATA.
 * Meta accepts GenericMeta since internal processing constructs metadata from scratch.
 */
const queueChange = <DATA extends object, META extends GenericMeta>(
  queue: ArrayOfChanges<DATA, META>,
  path: string,
  value: unknown,
  meta: GenericMeta,
): void => {
  // Cast is safe: paths in graphs were validated at registration
  queue.push([path, value, meta] as ArrayOfChanges<DATA, META>[number])
}

/**
 * Process sync paths - propagate changes to synced paths
 * Uses graphology neighbors() for efficient lookup
 */
const processSyncPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
): void => {
  const { sync } = store._internal.graphs
  const { queue } = store._internal.processing

  for (const change of changes) {
    const [path, value, meta] = change
    if (meta?.isSyncPathChange) continue

    // Check if path exists in graph
    if (!sync.hasNode(path as string)) continue

    // Get all neighbors (synced paths)
    const neighbors = sync.neighbors(path as string)

    for (const syncPath of neighbors) {
      queueChange(queue, syncPath, value, {
        ...(meta || {}),
        isSyncPathChange: true,
      })
    }
  }
}

/**
 * Process flip paths - propagate inverse boolean values
 * Uses graphology neighbors() for efficient lookup
 */
const processFlipPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
): void => {
  const { flip } = store._internal.graphs
  const { queue } = store._internal.processing

  for (const change of changes) {
    const [path, value, meta] = change as [string, unknown, GenericMeta]
    if (meta?.isFlipPathChange) continue
    if (typeof value !== 'boolean') continue

    // Check if path exists in graph
    if (!flip.hasNode(path)) continue

    // Get all neighbors (flipped paths)
    const neighbors = flip.neighbors(path)

    for (const flipPath of neighbors) {
      queueChange(queue, flipPath, !value, {
        ...(meta || {}),
        isFlipPathChange: true,
      })
    }
  }
}

/**
 * Process aggregations - recalculate targets when sources change
 * Uses directed graph: outNeighbors(changedPath) to find affected targets
 */
const processAggregations = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): void => {
  const { aggregations } = store._internal.graphs
  const { queue } = store._internal.processing

  // Find all affected targets using graph traversal
  const affectedTargets = new Set<string>()

  for (const change of changes) {
    const [path] = change as [string, unknown, GenericMeta]

    if (!aggregations.hasNode(path)) continue

    // Get all targets that depend on this source
    const targets = aggregations.outNeighbors(path)
    for (const target of targets) {
      affectedTargets.add(target)
    }
  }

  // Recalculate each affected target
  for (const target of affectedTargets) {
    const rules = aggregations.getNodeAttribute(target, 'rules') as
      | {
          id: string
          sourcePaths: string[]
          aggregate: (state: DATA, paths: string[]) => unknown
        }[]
      | undefined

    if (!rules || rules.length === 0) continue

    // Use first rule's aggregate function
    const rule = rules[0]
    const result = rule.aggregate(currentState, rule.sourcePaths)

    queueChange(queue, target, result, { isAggregationChange: true })
  }
}

/**
 * Process listeners - call registered listeners for changed paths
 * Supports flushQueue to stop processing and clear the queue
 */
const processListeners = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): void => {
  const { listeners } = store._internal.graphs
  const { queue } = store._internal.processing

  const flushQueue: FlushQueue = () => {
    store._internal.processing.queue = []
    throw FLUSH_QUEUE
  }

  try {
    for (const change of changes) {
      const [path] = change as [string, unknown, GenericMeta]

      const pathListeners = listeners.get(path)
      if (!pathListeners) continue

      for (const listener of pathListeners) {
        const result = listener(change, currentState, flushQueue)
        if (result && result.length > 0) {
          // Add listener metadata
          for (const r of result) {
            queueChange(queue, r[0], r[1], {
              ...(r[2] || {}),
              isListenerChange: true,
            })
          }
        }
      }
    }
  } catch (e) {
    if (e === FLUSH_QUEUE) {
      // queue is cleared by flushQueue function
      return
    }
    throw e
  }
}

/**
 * Apply a batch of changes to the state
 */
const applyBatch = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  state: DATA,
): void => {
  for (const [path, value] of changes) {
    _set(state, path as string, value)
  }
}

/**
 * Main change processor
 *
 * Processes changes through side-effect graphs until stabilization.
 * Simple sequential execution - no factory pattern.
 */
export const processChanges = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>,
): void => {
  const { processing } = store._internal
  const maxIterations = store.config.maxIterations ?? 100

  // Reentrancy guard - queue changes if already processing
  if (processing.isProcessing) {
    processing.queue.push(...initialChanges)
    return
  }

  processing.isProcessing = true
  processing.queue = [...initialChanges]

  let iterations = 0

  try {
    while (processing.queue.length > 0 && iterations < maxIterations) {
      // Take current batch
      const batch = processing.queue
      processing.queue = []

      // Get current state snapshot for processing
      const currentState = snapshot(store.state) as DATA

      // Sequential processing - order matters
      processSyncPaths(batch, store)
      processFlipPaths(batch, store)
      processAggregations(batch, store, currentState)
      processListeners(batch, store, currentState)

      // Apply batch to state
      applyBatch(batch, store.state)

      iterations++
    }

    if (iterations >= maxIterations) {
      console.warn(
        `[apex-state] Max iterations (${maxIterations}) reached - possible infinite loop in side-effects`,
      )
    }
  } finally {
    processing.isProcessing = false
  }
}
