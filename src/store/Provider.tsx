/**
 * Store Provider Component
 *
 * React component that initializes and provides the store to child components.
 *
 * Architecture: Unified _internal pattern
 * - state: User data (tracked by valtio)
 * - _concerns: Computed concern values (tracked by valtio)
 * - _internal: Graphs, registrations, processing queue (NOT tracked - wrapped in ref())
 */

import { useMemo } from 'react'
import { proxy, ref } from 'valtio'
import Graph from 'graphology'
import { StoreContext } from './StoreContext'
import type { ProviderProps, StoreInstance, InternalState, ConcernValues } from './types'
import type { GenericMeta } from '../types'

/**
 * Creates the initial internal state structure
 * Wrapped in ref() to prevent valtio tracking
 */
const createInternalState = <DATA extends object, META extends GenericMeta = GenericMeta>(): InternalState<DATA, META> => ({
  graphs: {
    sync: new Graph({ type: 'undirected' }),
    flip: new Graph({ type: 'undirected' }),
    aggregations: new Graph({ type: 'directed', allowSelfLoops: false }),
    listeners: new Map(),
  },
  registrations: {
    concerns: new Map(),
    effectCleanups: new Set(),
    sideEffectCleanups: new Map(),
  },
  processing: {
    queue: [],
    isProcessing: false,
  },
})

/**
 * Creates a Provider component for a specific data type
 */
export const createProvider = <DATA extends object, META extends GenericMeta = GenericMeta>() => {
  const Provider = ({ initialState, errorStorePath = '_errors', children }: ProviderProps<DATA>) => {
    const store = useMemo<StoreInstance<DATA, META>>(() => {
      // 1. state: Application data (tracked by valtio)
      //    - User actions WRITE to this
      //    - Effects READ from this
      const state = proxy(initialState)

      // 2. _concerns: Computed concern values (tracked by valtio)
      //    - Effects WRITE to this
      //    - UI components READ from this
      //    - Separate from state to prevent infinite loops
      const _concerns = proxy<ConcernValues>({})

      // 3. _internal: Graphs, registrations, processing (NOT tracked)
      //    - Wrapped in ref() to prevent valtio proxy tracking
      //    - Contains side-effect graphs, concern registrations, change queue
      const _internal = ref(createInternalState<DATA, META>())

      return {
        state,
        _concerns,
        _internal,
        config: {
          errorStorePath,
          maxIterations: 100,
        },
      }
      // Only initialize once - ignore changes to initialState after mount
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return <StoreContext.Provider value={store as any}>{children}</StoreContext.Provider>
  }

  Provider.displayName = 'StoreProvider'

  return Provider
}
