/**
 * useFieldTransformedStore - Hook for fields with bidirectional transformations
 *
 * Useful for handling display format vs storage format differences:
 * - Dates: store ISO string, display formatted date
 * - Numbers: store number, display formatted string with separators
 * - Any case where UI representation differs from storage
 *
 * @example
 * ```typescript
 * function BirthDateField() {
 *   const dateField = store.useFieldTransformedStore('birthDate', {
 *     toTemporary: (iso: string) => format(new Date(iso), 'MM/DD/YYYY'),
 *     fromTemporary: (display: string) => parse(display, 'MM/DD/YYYY').toISOString()
 *   })
 *
 *   return (
 *     <input
 *       value={dateField.value}
 *       onChange={e => dateField.setValue(e.target.value)}
 *     />
 *   )
 * }
 * ```
 */

import { useState, useCallback, useEffect } from 'react'
import { useSnapshot, snapshot } from 'valtio'
import type { DeepKey, DeepValue, GenericMeta, ArrayOfChanges } from '../types'
import { useStoreContext } from './useStoreContext'
import { deepGet } from '../store/utils/deepAccess'
import { executePipeline, applyChanges } from '../pipeline/executor'

/**
 * Configuration for field transformations
 */
export interface FieldTransformConfig<VAL, CTX> {
  /**
   * Transform from stored value to temporary (display) value
   */
  toTemporary: (val: VAL, context?: CTX) => CTX

  /**
   * Transform from temporary (display) value to stored value
   */
  fromTemporary: (ctx: CTX, context?: CTX) => VAL

  /**
   * Optional context for transformations (e.g., locale, format options)
   */
  context?: CTX
}

/**
 * Hook for form fields with bidirectional transformations.
 *
 * Maintains local state for responsive UI updates, syncs with store when
 * transformations complete. Prevents flicker during user input.
 *
 * @param path - Path to the field in state
 * @param config - Transformation configuration
 * @returns Object with transformed value and setValue
 */
export const useFieldTransformedStore = <
  DATA extends object,
  P extends DeepKey<DATA>,
  VAL extends DeepValue<DATA, P>,
  CTX,
  META extends GenericMeta = GenericMeta
>(
  path: P,
  config: FieldTransformConfig<VAL, CTX>
): {
  value: CTX
  setValue: (newContext: CTX) => void
} => {
  const store = useStoreContext<DATA>()
  const { toTemporary, fromTemporary, context } = config

  // Use valtio's useSnapshot for reactive state
  const snap = useSnapshot(store.state) as DATA

  // Get the stored value at the specified path
  const storedValue = deepGet(snap, path) as DeepValue<DATA, P>

  // Transform stored value to temporary (display) format
  const temporaryValue = toTemporary(storedValue as VAL, context)

  // Local state for temporary value (to avoid re-transforming on every render)
  const [localValue, setLocalValue] = useState<CTX>(temporaryValue)

  // Update local value when stored value changes externally
  useEffect(() => {
    setLocalValue(toTemporary(storedValue as VAL, context))
  }, [storedValue, toTemporary, context])

  const setValue = useCallback(
    (newContext: CTX) => {
      // Update local state immediately for responsive UI
      setLocalValue(newContext)

      // Transform back and update stored value
      const newStoredValue = fromTemporary(newContext, context)

      // Create a change tuple for the pipeline
      const changes: ArrayOfChanges<DATA, META> = [
        [path, newStoredValue as DeepValue<DATA, P>, {} as META]
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
    [store, path, fromTemporary, context]
  )

  return {
    value: localValue,
    setValue,
  }
}
