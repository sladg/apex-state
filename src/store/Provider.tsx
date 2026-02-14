import { useMemo } from 'react'

import { proxy, ref } from 'valtio'

import type { InternalState } from '../_internal'
import { _internal } from '../_internal'
import { StoreContext } from '../core/context'
import { DEFAULT_STORE_CONFIG } from '../core/defaults'
import type { ProviderProps, StoreConfig, StoreInstance } from '../core/types'
import type { DeepRequired, GenericMeta } from '../types'
import { createGraph } from '../utils/graph'
import { createTopicRouter } from '../utils/topicRouter'

const createInternalState = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  config: DeepRequired<StoreConfig>,
): InternalState<DATA, META> => ({
  graphs: {
    sync: createGraph(),
    flip: createGraph(),
    topicRouter: createTopicRouter(),
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
  timing: _internal.createTiming(config.debug),
  config,
})

export const createProvider = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  storeConfig?: StoreConfig,
) => {
  // Resolve config with defaults at factory time
  const resolvedConfig = _internal.deepMerge(DEFAULT_STORE_CONFIG, storeConfig)

  const Provider = ({ initialState, children }: ProviderProps<DATA>) => {
    const store = useMemo<StoreInstance<DATA, META>>(() => {
      return {
        // state: Application data (tracked by valtio)
        // User actions WRITE to this, effects READ from this
        state: proxy(initialState),

        // _concerns: Computed concern values (tracked by valtio)
        // Effects WRITE to this, UI components READ from this
        _concerns: proxy({} as Record<string, Record<string, unknown>>),

        // _internal: Graphs, registrations, processing (NOT tracked)
        // Wrapped in ref() to prevent tracking even if store is later wrapped in a proxy
        _internal: ref(createInternalState<DATA, META>(resolvedConfig)),
      }
      // Only initialize once - ignore changes to initialState after mount
    }, [])

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
