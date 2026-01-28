/**
 * useStoreContext Hook
 *
 * @internal Package-internal hook for accessing the store from context.
 * This is used internally by other hooks in the store instance.
 * End users should not call this directly - use the hooks returned by createGenericStore instead
 * (e.g., store.useStore, store.useFieldStore, store.useConcerns, etc.).
 *
 * Throws an error if used outside of a Provider.
 */

import { useContext } from 'react'

import { StoreContext } from '../store/StoreContext'
import type { StoreInstance } from '../store/types'
import type { GenericMeta } from '../types'

/**
 * Access the store instance from context.
 *
 * @internal Package-internal. Do not use directly.
 * @throws Error if used outside Provider
 */
export const useStoreContext = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(): StoreInstance<DATA, META> => {
  const store = useContext(StoreContext) as StoreInstance<DATA, META> | null

  if (!store) {
    throw new Error(
      'useStoreContext must be used within a Store Provider. ' +
        'Make sure your component is wrapped in <store.Provider>.',
    )
  }

  return store
}
