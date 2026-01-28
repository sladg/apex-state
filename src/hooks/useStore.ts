/**
 * useStore Hook
 *
 * A useState-like hook for accessing and updating specific paths in the store.
 * Returns a tuple of [value, setValue] similar to React's useState.
 *
 * @example
 * ```typescript
 * const [count, setCount] = store.useStore('count')
 * const [userName, setUserName] = store.useStore('user.name')
 *
 * // Update value
 * setCount(42)
 * setUserName('Alice', { sender: 'user-123' })
 * ```
 */

import { useCallback } from 'react'
import { useSnapshot, snapshot } from 'valtio'
import { useStoreContext } from './useStoreContext'
import { deepGet } from '../store/utils/deepAccess'
import { executePipeline, applyChanges } from '../pipeline/executor'
import type { DeepKey, DeepValue, GenericMeta, ArrayOfChanges } from '../types'

/**
 * Hook for accessing and updating a specific path in the store
 *
 * @param path - The deep path to access (e.g., 'user.name' or 'items[0].title')
 * @returns Tuple of [value, setValue] like useState
 */
export const useStore = <
  DATA extends object,
  P extends DeepKey<DATA>,
  META extends GenericMeta = GenericMeta
>(
  path: P
): [
  DeepValue<DATA, P>,
  (value: DeepValue<DATA, P>, meta?: META) => void
] => {
  const store = useStoreContext<DATA>()

  // Use valtio's useSnapshot for reactive state
  const snap = useSnapshot(store.state) as DATA

  // Get the value at the specified path
  const value = deepGet(snap, path) as DeepValue<DATA, P>

  // Setter function to update the value with pipeline integration
  const setValue = useCallback(
    (newValue: DeepValue<DATA, P>, meta?: META) => {
      // Create a change tuple for the pipeline
      const changes: ArrayOfChanges<DATA, META> = [
        [path, newValue, (meta || {}) as META]
      ]

      // Execute pipeline to process changes and side-effects
      const finalChanges = executePipeline(
        changes,
        snapshot(store.state) as DATA,
        store.pipelineConfig
      )

      // Apply final changes atomically to the valtio proxy
      applyChanges(store.state, finalChanges)
    },
    [store, path]
  )

  return [value, setValue]
}
