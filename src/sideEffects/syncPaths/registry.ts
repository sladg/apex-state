/**
 * SyncPathsRegistry for managing sync path registrations
 *
 * Allows registering pairs of paths that should always have the same value.
 * Validates no cycles are created before accepting registration.
 */

import type { DeepKey } from '../../types'
import { createSyncPathGraph, type SyncPathGraph } from './graph'

/**
 * Registry state for sync path management
 */
interface SyncPathsRegistryState<DATA extends object> {
  graph: SyncPathGraph<DATA>
}

/**
 * Registry for sync path registrations
 */
export interface SyncPathsRegistry<DATA extends object> {
  /**
   * Access to the underlying graph for synchronizer
   */
  readonly graph: SyncPathGraph<DATA>

  /**
   * Register a sync relationship between two paths
   * @throws Error if registration would create a cycle
   */
  register(id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>): void

  /**
   * Unregister a sync relationship by ID
   */
  unregister(id: string): void
}

/**
 * Create a new SyncPathsRegistry instance
 */
export const createSyncPathsRegistry = <
  DATA extends object
>(): SyncPathsRegistry<DATA> => {
  const state: SyncPathsRegistryState<DATA> = {
    graph: createSyncPathGraph<DATA>(),
  }

  return {
    get graph() {
      return state.graph
    },

    register(id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>): void {
      // This will throw if it would create a cycle
      state.graph.addEdge(id, path1, path2)
    },

    unregister(id: string): void {
      state.graph.removeEdge(id)
    },
  }
}
