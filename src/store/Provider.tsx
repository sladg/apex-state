/**
 * Store Provider Component
 *
 * React component that initializes and provides the store to child components.
 *
 * Architecture: Two-Proxy Pattern
 * - state: User data proxy (tracked by valtio)
 * - _concerns: Computed concern values proxy (tracked by valtio)
 * - _internal: Graphs, registrations, processing queue (NOT tracked)
 * - config: Store configuration
 *
 * state and _concerns are independent proxies to prevent infinite loops
 * during concern evaluation (Two-Proxy Pattern).
 */

import { useMemo } from 'react'

import Graph from 'graphology'
import { proxy, ref } from 'valtio'

import type { GenericMeta } from '../types'
import { StoreContext } from './StoreContext'
import type { InternalState, ProviderProps, StoreInstance } from './types'

/**
 * Creates the initial internal state structure
 * Wrapped in ref() to prevent valtio tracking
 */
const createInternalState = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(): InternalState<DATA, META> => ({
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
export const createProvider = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>() => {
  const Provider = ({
    initialState,
    errorStorePath = '_errors',
    children,
  }: ProviderProps<DATA>) => {
    const store = useMemo<StoreInstance<DATA, META>>(
      () => ({
        // state: Application data (tracked by valtio)
        // User actions WRITE to this, effects READ from this
        state: proxy(initialState),

        // _concerns: Computed concern values (tracked by valtio)
        // Effects WRITE to this, UI components READ from this
        _concerns: proxy({} as Record<string, Record<string, unknown>>),

        // _internal: Graphs, registrations, processing (NOT tracked)
        // Wrapped in ref() to prevent tracking even if store is later wrapped in a proxy
        _internal: ref(createInternalState<DATA, META>()),

        // config: Store configuration
        config: {
          errorStorePath,
          maxIterations: 100,
        },
      }),
      // Only initialize once - ignore changes to initialState after mount
      [],
    )

    return (
      <StoreContext.Provider
        value={store as unknown as StoreInstance<any, GenericMeta>}
      >
        {children}
      </StoreContext.Provider>
    )
  }

  Provider.displayName = 'StoreProvider'

  return Provider
}
