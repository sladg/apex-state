/**
 * AggregationsRegistry for managing aggregation registrations
 *
 * Allows registering aggregation rules where multiple source paths
 * aggregate into a single target path.
 * Validates no cycles are created before accepting registration.
 */

import type { DeepKey } from '../../types'
import { createAggregationGraph, type AggregationGraph } from './graph'

/**
 * Registry state for aggregation management
 */
interface AggregationsRegistryState<DATA extends object> {
  graph: AggregationGraph<DATA>
}

/**
 * Registry for aggregation registrations
 */
export interface AggregationsRegistry<DATA extends object> {
  /**
   * Access to the underlying graph for synchronizer
   */
  readonly graph: AggregationGraph<DATA>

  /**
   * Register an aggregation relationship: sourcePaths → targetPath
   * @throws Error if registration would create a cycle
   */
  register(
    id: string,
    targetPath: DeepKey<DATA>,
    sourcePaths: DeepKey<DATA>[]
  ): void

  /**
   * Unregister an aggregation relationship by ID
   */
  unregister(id: string): void
}

/**
 * Create a new AggregationsRegistry instance
 */
export const createAggregationsRegistry = <
  DATA extends object
>(): AggregationsRegistry<DATA> => {
  const state: AggregationsRegistryState<DATA> = {
    graph: createAggregationGraph<DATA>(),
  }

  return {
    get graph() {
      return state.graph
    },

    register(
      id: string,
      targetPath: DeepKey<DATA>,
      sourcePaths: DeepKey<DATA>[]
    ): void {
      // This will throw if it would create a cycle
      state.graph.addAggregation(id, targetPath, sourcePaths)
    },

    unregister(id: string): void {
      state.graph.removeAggregation(id)
    },
  }
}
