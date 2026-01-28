/**
 * useJitStore Hook
 *
 * Just-In-Time data access hook for bulk operations.
 * Provides reactive snapshot, batch update function, and non-reactive read.
 *
 * @example
 * ```typescript
 * const { proxyValue, setChanges, getState } = store.useJitStore()
 *
 * // Read reactive value
 * console.log(proxyValue.count)
 *
 * // Batch update multiple paths
 * setChanges([
 *   ['user.name', 'Alice', {}],
 *   ['count', 42, {}]
 * ])
 *
 * // Non-reactive read
 * const currentState = getState()
 * ```
 */

import { useCallback } from 'react'
import { useSnapshot, snapshot } from 'valtio'
import { useStoreContext } from './useStoreContext'
import { executePipeline, applyChanges } from '../pipeline/executor'
import type { ArrayOfChanges, GenericMeta } from '../types'

/**
 * Return type for useJitStore hook
 */
export interface JitStoreReturn<DATA extends object, META extends GenericMeta> {
  /**
   * Reactive snapshot of the store state
   * Re-renders component when accessed properties change
   */
  proxyValue: DATA

  /**
   * Apply batch changes to the store
   * @param changes - Array of [path, value, meta] tuples
   */
  setChanges: (changes: ArrayOfChanges<DATA, META>) => void

  /**
   * Get current state without triggering re-renders
   * @returns Non-reactive snapshot of the store
   */
  getState: () => DATA
}

/**
 * Hook for bulk store operations and Just-In-Time access
 *
 * @returns Object with proxyValue (reactive), setChanges (batch updates), and getState (non-reactive read)
 */
export function useJitStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(): JitStoreReturn<DATA, META> {
  const store = useStoreContext<DATA>()

  // Reactive snapshot - triggers re-renders when accessed properties change
  const proxyValue = useSnapshot(store.state) as DATA

  // Batch update function with pipeline integration
  const setChanges = useCallback(
    (changes: ArrayOfChanges<DATA, META>) => {
      // Execute pipeline to process changes and side-effects
      const finalChanges = executePipeline(
        changes,
        snapshot(store.state) as DATA,
        store.pipelineConfig
      )

      // Apply final changes atomically to the valtio proxy
      // This ensures a single re-render for all changes
      applyChanges(store.state, finalChanges)
    },
    [store]
  )

  // Non-reactive state getter
  const getState = useCallback(() => {
    // Use valtio's snapshot function for non-reactive read
    return snapshot(store.state) as DATA
  }, [store])

  return {
    proxyValue,
    setChanges,
    getState,
  }
}
