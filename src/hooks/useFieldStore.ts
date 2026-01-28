/**
 * useFieldStore - Convenient hook for form field management
 *
 * Wraps useStore to provide an object-based API {value, setValue}
 * instead of array-based [value, setValue]. Better DX for forms.
 *
 * @example
 * ```typescript
 * function EmailField() {
 *   const emailField = store.useFieldStore('user.email')
 *
 *   return (
 *     <input
 *       value={emailField.value}
 *       onChange={e => emailField.setValue(e.target.value)}
 *     />
 *   )
 * }
 * ```
 */

import { useCallback } from 'react'
import { useSnapshot, snapshot } from 'valtio'
import type { DeepKey, DeepValue, GenericMeta, ArrayOfChanges } from '../types'
import { useStoreContext } from './useStoreContext'
import { deepGet } from '../store/utils/deepAccess'
import { executePipeline, applyChanges } from '../pipeline/executor'

/**
 * Hook for form field management with convenient object API.
 *
 * @param path - Path to the field in state
 * @returns Object with value and setValue
 */
export const useFieldStore = <
  DATA extends object,
  P extends DeepKey<DATA>,
  META extends GenericMeta = GenericMeta
>(
  path: P
): {
  value: DeepValue<DATA, P>
  setValue: (newValue: DeepValue<DATA, P>, meta?: META) => void
} => {
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

  return {
    value,
    setValue,
  }
}
