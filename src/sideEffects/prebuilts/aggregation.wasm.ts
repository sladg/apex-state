/**
 * WASM Implementation - Aggregation Registration
 *
 * Thin TypeScript wrapper: passes raw pairs to WASM, applies returned changes.
 * WASM handles: validation, grouping, initial value computation.
 */

import { applyBatch } from '../../pipeline/applyBatch'
import { wasm } from '../../wasm/bridge'

export const registerAggregations: typeof import('./aggregation').registerAggregations =
  (store, _id, aggregationPairs) => {
    // Pass raw pairs to WASM — Rust handles validation and grouping
    const initialChanges = wasm.registerAggregationBatch(aggregationPairs)

    // Apply initial changes directly to state (not through pipeline to avoid write direction loop)
    if (initialChanges.length > 0) {
      applyBatch(
        initialChanges.map((c) => [c.path, c.value, {}]) as any,
        store.state,
      )
    }

    // Return cleanup function
    return () => {
      // Extract unique target paths from pairs
      const targetPaths = Array.from(
        new Set(aggregationPairs.map(([target]) => target)),
      )
      wasm.unregisterAggregationBatch(targetPaths)
    }
  }
