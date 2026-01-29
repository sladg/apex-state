import { useMemo } from 'react'

import Graph from 'graphology'
import { proxy, ref } from 'valtio'

import { StoreContext } from '../core/context'
import type { InternalState, ProviderProps, StoreInstance } from '../core/types'
import type { GenericMeta } from '../types'

const createInternalState = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(): InternalState<DATA, META> => ({
  graphs: {
    sync: new Graph({ type: 'undirected' }),
    flip: new Graph({ type: 'undirected' }),
    listeners: new Map(),
    sortedListenerPaths: [],
  },
  registrations: {
    concerns: new Map(),
    effectCleanups: new Set(),
    sideEffectCleanups: new Map(),
    aggregations: new Map(),
  },
  processing: {
    queue: [],
  },
})

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
