import { effect } from 'valtio-reactive'

import type { Aggregation, StoreInstance } from '../../core/types'
import type { AggregationPair, GenericMeta } from '../../types'
import { deepEqual, deepGetUnsafe, deepSetUnsafe } from '../../utils/deepAccess'

/**
 * Register multiple aggregations together (follows concerns pattern)
 * Creates all effects in one pass, returns single cleanup function
 */
export const registerAggregations = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  id: string,
  aggregationPairs: AggregationPair<DATA>[],
): (() => void) => {
  const { aggregations } = store._internal.registrations
  const disposeCallbacks: (() => void)[] = []

  // Collect all targets and sources for validation
  const targets = new Set<string>()
  const sources = new Set<string>()

  for (const [target, source] of aggregationPairs) {
    targets.add(target)
    sources.add(source)
  }

  // Validate no circular dependencies
  for (const target of targets) {
    if (sources.has(target)) {
      throw new Error(
        `[apex-state] Circular aggregation: "${target}" cannot be both target and source`,
      )
    }
  }

  // Group by target for multi-source aggregations
  const byTarget = new Map<string, string[]>()
  for (const [target, source] of aggregationPairs) {
    const existing = byTarget.get(target) ?? []
    existing.push(source)
    byTarget.set(target, existing)
  }

  // Track aggregations we create for cleanup
  const createdAggregations: Aggregation[] = []

  // Create all aggregation effects together
  for (const [targetPath, sourcePaths] of byTarget) {
    const aggregation: Aggregation = {
      targetPath,
      sourcePaths,
      id: `${id}:${targetPath}`, // Optional: for debugging
    }

    // Add to store's aggregations map (used by pipeline)
    const existing = aggregations.get(targetPath) ?? []
    existing.push(aggregation)
    aggregations.set(targetPath, existing)

    // Track for cleanup
    createdAggregations.push(aggregation)

    // Create reactive effect for this aggregation
    const dispose = effect(() => {
      // Early exit: stop as soon as we find a mismatch
      if (sourcePaths.length === 0) {
        deepSetUnsafe(store.state, targetPath, undefined)
        return
      }

      const first = deepGetUnsafe(store.state, sourcePaths[0]!)

      let allEqual = true
      for (let i = 1; i < sourcePaths.length && allEqual; i++) {
        if (!deepEqual(deepGetUnsafe(store.state, sourcePaths[i]!), first)) {
          allEqual = false
        }
      }

      // Set target: value if all equal, undefined otherwise
      const result = allEqual ? first : undefined
      deepSetUnsafe(store.state, targetPath, result)
    })

    disposeCallbacks.push(dispose)
  }

  // Return cleanup function (concerns pattern)
  return () => {
    // Dispose all effects
    disposeCallbacks.forEach((dispose) => dispose())

    // Remove aggregations from map
    for (const aggregation of createdAggregations) {
      const items = aggregations.get(aggregation.targetPath) ?? []
      const filtered = items.filter((item) => item !== aggregation)

      if (filtered.length === 0) {
        aggregations.delete(aggregation.targetPath)
      } else {
        aggregations.set(aggregation.targetPath, filtered)
      }
    }
  }
}
