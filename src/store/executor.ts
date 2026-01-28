/**
 * Change processor with sequential side-effect execution
 *
 * Simple, linear processing of changes through side-effect graphs.
 * No factory pattern - just direct function calls in sequence.
 */

import { snapshot } from 'valtio'
import _set from 'lodash/set'
import type { GenericMeta, ArrayOfChanges } from '../types'
import type { StoreInstance } from './types'

/**
 * Metadata added to changes during processing
 */
interface ChangeMeta {
  fromSync?: boolean
  fromFlip?: boolean
  fromAggregation?: boolean
  fromListener?: boolean
}

/**
 * Process sync paths - propagate changes to synced paths
 * Uses graphology neighbors() for efficient lookup
 */
const processSyncPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>
): void => {
  const { sync } = store._internal.graphs
  const { queue } = store._internal.processing

  for (const change of changes) {
    const [path, value, meta] = change
    if ((meta as ChangeMeta)?.fromSync) continue

    // Check if path exists in graph
    if (!sync.hasNode(path as string)) continue

    // Get all neighbors (synced paths)
    const neighbors = sync.neighbors(path as string)

    for (const syncPath of neighbors) {
      queue.push([
        syncPath as any,
        value,
        { ...(meta || {}), fromSync: true } as META,
      ])
    }
  }
}

/**
 * Process flip paths - propagate inverse boolean values
 * Uses graphology neighbors() for efficient lookup
 */
const processFlipPaths = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>
): void => {
  const { flip } = store._internal.graphs
  const { queue } = store._internal.processing

  for (const change of changes) {
    const [path, value, meta] = change
    if ((meta as ChangeMeta)?.fromFlip) continue
    if (typeof value !== 'boolean') continue

    // Check if path exists in graph
    if (!flip.hasNode(path as string)) continue

    // Get all neighbors (flipped paths)
    const neighbors = flip.neighbors(path as string)

    for (const flipPath of neighbors) {
      queue.push([
        flipPath as any,
        !value as any,
        { ...(meta || {}), fromFlip: true } as META,
      ])
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
  currentState: DATA
): void => {
  const { aggregations } = store._internal.graphs
  const { queue } = store._internal.processing

  // Find all affected targets using graph traversal
  const affectedTargets = new Set<string>()

  for (const [path] of changes) {
    const pathStr = path as string
    if (!aggregations.hasNode(pathStr)) continue

    // Get all targets that depend on this source
    const targets = aggregations.outNeighbors(pathStr)
    for (const target of targets) {
      affectedTargets.add(target)
    }
  }

  // Recalculate each affected target
  for (const target of affectedTargets) {
    const rules = aggregations.getNodeAttribute(target, 'rules') as Array<{
      id: string
      sourcePaths: string[]
      aggregate: (state: DATA, paths: string[]) => unknown
    }> | undefined

    if (!rules || rules.length === 0) continue

    // Use first rule's aggregate function
    const rule = rules[0]
    const result = rule.aggregate(currentState, rule.sourcePaths)

    queue.push([
      target as any,
      result as any,
      { fromAggregation: true } as unknown as META,
    ])
  }
}

/**
 * Process listeners - call registered listeners for changed paths
 */
const processListeners = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA
): void => {
  const { listeners } = store._internal.graphs
  const { queue } = store._internal.processing

  for (const change of changes) {
    const [path] = change
    const pathListeners = listeners.get(path as string)
    if (!pathListeners) continue

    for (const listener of pathListeners) {
      const result = listener(change, currentState)
      if (result && result.length > 0) {
        // Add fromListener metadata
        for (const r of result) {
          queue.push([
            r[0],
            r[1],
            { ...(r[2] || {}), fromListener: true } as META,
          ])
        }
      }
    }
  }
}

/**
 * Apply a batch of changes to the state
 */
const applyBatch = <DATA extends object, META extends GenericMeta>(
  changes: ArrayOfChanges<DATA, META>,
  state: DATA
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
export const processChanges = <DATA extends object, META extends GenericMeta = GenericMeta>(
  store: StoreInstance<DATA, META>,
  initialChanges: ArrayOfChanges<DATA, META>
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
        `[apex-state] Max iterations (${maxIterations}) reached - possible infinite loop in side-effects`
      )
    }
  } finally {
    processing.isProcessing = false
  }
}
