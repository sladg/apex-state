import { useCallback, useEffect, useState } from 'react'

import type {
  DeepKey,
  DeepValue,
  FieldTransformConfig,
  GenericMeta,
} from '../types'

/**
 * A hook that wraps useFieldStore with bidirectional value transformations.
 * Maintains a local state for the transformed value and syncs with the store.
 *
 * @param useFieldStoreHook - The store's useFieldStore hook
 * @param path - The path to the field in the store
 * @param config - Transformation configuration (toTemporary/fromTemporary)
 * @returns Object with transformed value and setValue function
 *
 * @example
 * ```typescript
 * const { value, setValue } = useFieldTransformedStore(
 *   Store.useFieldStore,
 *   'user.birthdate',
 *   {
 *     toTemporary: (iso) => format(new Date(iso), 'MM/DD/YYYY'),
 *     fromTemporary: (display) => parse(display, 'MM/DD/YYYY').toISOString()
 *   }
 * )
 * ```
 */
export const useFieldTransformedStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  P extends DeepKey<DATA> = DeepKey<DATA>,
  VAL extends DeepValue<DATA, P> = DeepValue<DATA, P>,
  CTX = any,
>(
  useFieldStoreHook: (path: P) => {
    value: VAL
    setValue: (newValue: VAL, meta?: META) => void
  },
  path: P,
  config: FieldTransformConfig<VAL, CTX>,
): {
  value: CTX
  setValue: (newContext: CTX, meta?: META) => void
} => {
  const { value: storedValue, setValue: setStoredValue } =
    useFieldStoreHook(path)
  const { toTemporary, fromTemporary, context } = config

  // Transform stored value to temporary (display) value
  const temporaryValue = toTemporary(storedValue, context)
  const [localValue, setLocalValue] = useState<CTX>(temporaryValue)

  // Sync local value when stored value changes
  useEffect(() => {
    setLocalValue(toTemporary(storedValue, context))
  }, [storedValue, toTemporary, context])

  // Update both local and stored values
  const setValue = useCallback(
    (newContext: CTX, meta?: META) => {
      setLocalValue(newContext)
      const newStoredValue = fromTemporary(newContext, context)
      setStoredValue(newStoredValue, meta)
    },
    [setStoredValue, fromTemporary, context],
  )

  return { value: localValue, setValue }
}
