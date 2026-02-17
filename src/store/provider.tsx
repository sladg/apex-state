import { useEffect, useMemo, useState } from 'react'

import { proxy, ref } from 'valtio'

import { StoreContext } from '../core/context'
import { DEFAULT_STORE_CONFIG } from '../core/defaults'
import { createPathGroups } from '../core/path-groups'
import type {
  DebugTrack,
  InternalState,
  ProviderProps,
  StoreConfig,
  StoreInstance,
} from '../core/types'
import type { DeepRequired, GenericMeta } from '../types'
import { deepMerge } from '../utils/deep-merge'
import {
  attachComputedGetters,
  prepareInitialState,
} from '../utils/derive-values'
import { createTiming } from '../utils/timing'
import { initPipeline, isWasmLoaded, loadWasm } from '../wasm/lifecycle'

export const createInternalState = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  config: DeepRequired<StoreConfig>,
): InternalState<DATA, META> => ({
  graphs: {
    sync: createPathGroups('sync'),
    flip: createPathGroups('flip'),
    listeners: new Map(),
    sortedListenerPaths: [],
    listenerHandlers: new Map(),
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
  timing: createTiming(config.debug),
  config,
  pipeline: null,
})

export const createProvider = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  storeConfig?: StoreConfig,
) => {
  // Resolve config with defaults at factory time
  const resolvedConfig = deepMerge(DEFAULT_STORE_CONFIG, storeConfig)
  const isLegacy = resolvedConfig.useLegacyImplementation

  const Provider = ({
    initialState: rawInitialState,
    children,
  }: ProviderProps<DATA>) => {
    const [wasmReady, setWasmReady] = useState(isLegacy || isWasmLoaded())

    // Prepare getter-free initial state once (used by both useMemo and useEffect)
    const prepared = useMemo(
      () => prepareInitialState(rawInitialState),
      // Only initialize once - ignore changes to initialState after mount
      [],
    )

    const store = useMemo<StoreInstance<DATA, META>>(() => {
      const internal = createInternalState<DATA, META>(resolvedConfig)

      // CRITICAL: If WASM is already loaded, create pipeline and init shadow state
      // BEFORE proxy creation, so registration hooks can compute initial sync changes.
      // Each Provider gets its own isolated pipeline (no global reset needed).
      if (!isLegacy && isWasmLoaded()) {
        initPipeline(internal, prepared.initialState)
      }

      const debugTrack: DebugTrack | null = resolvedConfig.debug.track
        ? {
            calls: [],
            clear: () => {
              debugTrack!.calls.length = 0
            },
          }
        : null

      const stateProxy = proxy(prepared.initialState)
      attachComputedGetters(stateProxy, prepared.getterMap)

      return {
        // state: Application data (tracked by valtio)
        // User actions WRITE to this, effects READ from this
        state: stateProxy,

        // _concerns: Computed concern values (tracked by valtio)
        // Effects WRITE to this, UI components READ from this
        _concerns: proxy({} as Record<string, Record<string, unknown>>),

        // _internal: Graphs, registrations, processing (NOT tracked)
        // Wrapped in ref() to prevent tracking even if store is later wrapped in a proxy
        _internal: ref(internal),

        // _debug: Tracking data for testing/debugging (only when debug.track enabled)
        _debug: debugTrack ? ref(debugTrack) : null,
      }
    }, [])

    // Load WASM asynchronously if not already loaded (production first-render path)
    useEffect(() => {
      if (isLegacy || isWasmLoaded()) return

      loadWasm()
        .then(() => {
          initPipeline(
            store._internal,
            prepared.initialState as Record<string, unknown>,
          )
          setWasmReady(true)
        })
        .catch((error) => {
          console.error('[apex-state] Failed to load WASM:', error)
          throw error
        })
    }, [])

    // Cleanup: destroy pipeline on unmount
    useEffect(() => {
      return () => {
        store._internal.pipeline?.destroy()
        store._internal.pipeline = null
      }
    }, [])

    // Block rendering until WASM is ready (if using WASM mode)
    if (!wasmReady) {
      return null
    }

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
