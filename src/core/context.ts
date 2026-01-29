/**
 * Store React Context and Hook
 *
 * Core primitives for providing and accessing the store instance.
 * This module has minimal dependencies to avoid circular imports.
 */

import { createContext, useContext } from 'react'

import type { GenericMeta } from '../types'
import type { StoreInstance } from './types'

/**
 * React Context for the store instance
 * Null by default - will be populated by Provider
 */
export const StoreContext = createContext<StoreInstance<
  any,
  GenericMeta
> | null>(null)

StoreContext.displayName = 'StoreContext'

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
