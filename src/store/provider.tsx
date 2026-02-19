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
    // Prepare getter-free initial state once (used by both useMemo and useEffect)
    const prepared = useMemo(
      () => prepareInitialState(rawInitialState),
      // Only initialize once - ignore changes to initialState after mount
      [],
    )

    const store = useMemo<StoreInstance<DATA, META>>(() => {
      const internal = createInternalState<DATA, META>(resolvedConfig)

      // Fast path: If WASM is already loaded (tests, cached), create pipeline
      // synchronously so children can render on the very first frame.
      // This is a pure optimization — the useEffect below handles all other cases.
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

    // Derive initial readiness from actual pipeline state, not a separate boolean.
    // If useMemo fast-path created a pipeline, ready starts true (zero-flash rendering).
    // Otherwise starts false and the effect below will set it true after WASM loads.
    const [ready, setReady] = useState(
      () => isLegacy || store._internal.pipeline !== null,
    )

    // Single effect handles ALL pipeline lifecycle: WASM loading, pipeline init,
    // StrictMode restore, and cleanup — always paired in one place.
    useEffect(() => {
      if (isLegacy) return

      let cancelled = false

      const ensurePipeline = () => {
        if (cancelled) return
        if (!store._internal.pipeline) {
          initPipeline(
            store._internal,
            prepared.initialState as Record<string, unknown>,
          )
        }
        setReady(true)
      }

      if (isWasmLoaded()) {
        // WASM already loaded (tests, cached, or StrictMode re-mount) — init synchronously
        ensurePipeline()
      } else {
        // Production first-load — wait for WASM, then init
        loadWasm()
          .then(ensurePipeline)
          .catch((error) => {
            console.error('[apex-state] Failed to load WASM:', error)
          })
      }

      return () => {
        cancelled = true
        store._internal.pipeline?.destroy()
        store._internal.pipeline = null
        setReady(false)
      }
    }, [])

    // Block rendering until pipeline is ready (WASM loaded + pipeline initialized)
    if (!ready) {
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
