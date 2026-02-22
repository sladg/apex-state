/**
 * Aggregation Writes Processor
 *
 * Preprocessor that intercepts writes to aggregation target paths and
 * distributes them to all source paths. Runs BEFORE sync/flip/listeners.
 *
 * Example:
 *   User writes: form.allChecked = true
 *   Processor intercepts and replaces with:
 *     item1.checked = true
 *     item2.checked = true
 *     item3.checked = true
 */

import type { Aggregation, StoreInstance } from '../../core/types'
import type { ArrayOfChanges, GenericMeta } from '../../types'
import { queueChange } from '../queue'

/**
 * Distribute a change to all source paths in the aggregation
 */
const distributeToSources = <DATA extends object, META extends GenericMeta>(
  aggregation: Aggregation,
  targetPath: string,
  changePath: string,
  value: unknown,
  meta: GenericMeta,
  queue: ArrayOfChanges<DATA, META>,
): void => {
  const isExactMatch = changePath === targetPath

  for (const sourcePath of aggregation.sourcePaths) {
    // Calculate final path (handle child paths)
    const finalPath = isExactMatch
      ? sourcePath
      : `${sourcePath}.${changePath.substring(targetPath.length + 1)}`

    queueChange({
      queue,
      path: finalPath,
      value,
      meta: { ...meta, isAggregationChange: true },
    })
  }
}

export const processAggregationWrites = <
  DATA extends object,
  META extends GenericMeta,
>(
  changes: ArrayOfChanges<DATA, META>,
  store: StoreInstance<DATA, META>,
): void => {
  const { aggregations } = store._internal.registrations
  const { queue } = store._internal.processing

  // Process changes in reverse (so we can safely splice)
  for (let i = changes.length - 1; i >= 0; i--) {
    const [path, value, meta = {}] = changes[i]!

    // Check if this path is an aggregation target (or child of one)
    for (const [targetPath, items] of aggregations) {
      // Check for exact match or child path
      const isExactMatch = path === targetPath
      const isChildPath = path.startsWith(targetPath + '.')

      if (isExactMatch || isChildPath) {
        // Distribute to all source paths in all aggregations for this target
        for (const aggregation of items) {
          distributeToSources(aggregation, targetPath, path, value, meta, queue)
        }

        // Remove original change from batch
        changes.splice(i, 1)
        break // Move to next change
      }
    }
  }
}
