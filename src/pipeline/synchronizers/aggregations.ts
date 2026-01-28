/**
 * Aggregations Synchronizer
 *
 * Implements bidirectional aggregation logic:
 * 1. Source change: If all sources equal → set target, else target = undefined
 * 2. Target change: Distribute value to all sources
 *
 * PERFORMANCE CRITICAL: Heavily optimized for frequent execution.
 * - Caches value comparisons
 * - Early exit when no aggregations affected
 * - Avoids redundant lookups
 */

import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { AggregationsRegistry } from '../../sideEffects/aggregations/registry'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Create a synchronizer for aggregations side-effect.
 *
 * Two behaviors:
 * 1. When a source changes: check if all sources are equal
 *    - If equal: set target to that value
 *    - If different: set target to undefined
 * 2. When target changes: set all sources to target value
 *
 * @param registry - The AggregationsRegistry instance managing aggregation relationships
 * @returns A synchronizer function that processes aggregation logic
 */
export function createAggregationsSynchronizer<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(registry: AggregationsRegistry<DATA>): Synchronizer<DATA, META> {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []
    const processedTargets = new Set<string>()
    const processedSources = new Set<string>()

    // OPTIMIZATION: Early exit if no changes
    if (changes.length === 0) {
      return changes
    }

    for (const [path, value, meta] of changes) {
      const pathStr = path as string

      // Skip if this is already an aggregation change (avoid infinite loops)
      if ((meta as any).isAggregationChange) {
        continue
      }

      // Check if this path is a target (behavior 2: target→sources distribution)
      const aggregationsForTarget = registry.graph.getAggregationsForTarget(path)
      if (aggregationsForTarget.length > 0 && !processedTargets.has(pathStr)) {
        processedTargets.add(pathStr)

        // Target changed → distribute to all sources in all aggregations
        for (const aggregation of aggregationsForTarget) {
          for (const sourcePath of aggregation.sourcePaths) {
            newChanges.push([
              sourcePath,
              value,
              { ...meta, isAggregationChange: true, isProgramaticChange: true } as META,
            ])
          }
        }
      }

      // Check if this path is a source (behavior 1: sources→target aggregation)
      const targetsForSource = registry.graph.getTargetsForSource(path)

      // OPTIMIZATION: Early exit if no targets affected
      if (targetsForSource.size === 0) {
        continue
      }

      for (const targetPathStr of targetsForSource) {
        // OPTIMIZATION: Skip already processed targets
        if (processedSources.has(targetPathStr)) {
          continue
        }
        processedSources.add(targetPathStr)

        const targetPath = targetPathStr as DeepKey<DATA>
        const aggregations = registry.graph.getAggregationsForTarget(targetPath)

        // For each aggregation, check if all sources are equal
        for (const aggregation of aggregations) {
          // OPTIMIZATION: Get source values once
          const sourceValues = aggregation.sourcePaths.map((p) =>
            deepGet(state, p)
          )

          // OPTIMIZATION: Early exit if no source values
          if (sourceValues.length === 0) {
            continue
          }

          // Check if all equal using strict equality
          const firstValue = sourceValues[0]
          const allEqual = sourceValues.every((v) => v === firstValue)

          if (allEqual) {
            // All equal → set target to that value
            newChanges.push([
              targetPath,
              firstValue,
              { ...meta, isAggregationChange: true, isProgramaticChange: true } as META,
            ] as any)
          } else {
            // Different → set target to undefined
            newChanges.push([
              targetPath,
              undefined,
              { ...meta, isAggregationChange: true, isProgramaticChange: true } as META,
            ] as any)
          }
        }
      }
    }

    return [...changes, ...newChanges]
  }
}