/**
 * useStoreContext Hook
 *
 * Internal hook to access the store from context.
 * Throws an error if used outside of a Provider.
 */

import { useContext } from 'react'
import { StoreContext } from '../store/StoreContext'
import type { StoreInstance } from '../store/types'

/**
 * Access the store instance from context
 * @throws Error if used outside Provider
 */
export function useStoreContext<DATA extends object>(): StoreInstance<DATA> {
  const store = useContext(StoreContext) as StoreInstance<DATA> | null

  if (!store) {
    throw new Error(
      'useStoreContext must be used within a Store Provider. ' +
        'Make sure your component is wrapped in <store.Provider>.'
    )
  }

  return store
}
