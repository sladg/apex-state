import { useEffect, useMemo, useRef, useState } from 'react'

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
import { createTiming } from '../utils/timing'
import { isWasmLoaded, loadWasm, wasm } from '../wasm/bridge'

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
})

export const createProvider = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  storeConfig?: StoreConfig,
) => {
  // Resolve config with defaults at factory time
  const resolvedConfig = deepMerge(DEFAULT_STORE_CONFIG, storeConfig)

  const Provider = ({ initialState, children }: ProviderProps<DATA>) => {
    // Track if shadow state has been initialized (only init once)
    const shadowInitialized = useRef(false)

    // Initialize wasmReady based on config and current WASM load state
    const [wasmReady, setWasmReady] = useState(
      resolvedConfig.useLegacyImplementation || isWasmLoaded(),
    )

    // CRITICAL FIX: If WASM is already loaded, initialize shadow state immediately
    // BEFORE store creation, so registration hooks can compute initial sync changes.
    // Reset the pipeline first to clear stale registrations (sync pairs, BoolLogic, etc.)
    // from previous Provider mounts — each Provider represents a fresh store.
    if (
      !resolvedConfig.useLegacyImplementation &&
      isWasmLoaded() &&
      !shadowInitialized.current
    ) {
      wasm.pipelineReset()
      wasm.shadowInit(initialState as Record<string, unknown>)
      shadowInitialized.current = true
    }

    const store = useMemo<StoreInstance<DATA, META>>(() => {
      const debugTrack: DebugTrack | null = resolvedConfig.debug.track
        ? {
            calls: [],
            clear: () => {
              debugTrack!.calls.length = 0
            },
          }
        : null

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

        // _debug: Tracking data for testing/debugging (only when debug.track enabled)
        _debug: debugTrack ? ref(debugTrack) : null,
      }
      // Only initialize once - ignore changes to initialState after mount
    }, [])

    // Load WASM and initialize shadow state (blocking in WASM mode)
    useEffect(() => {
      if (resolvedConfig.useLegacyImplementation) {
        // Legacy mode - no WASM needed
        return
      }

      // WASM mode - load WASM and initialize shadow state
      const initWasm = async () => {
        await loadWasm()

        // Only init shadow if not already done (e.g., by synchronous path above)
        if (!shadowInitialized.current) {
          wasm.pipelineReset()
          wasm.shadowInit(initialState as Record<string, unknown>)
          shadowInitialized.current = true
        }

        setWasmReady(true)
      }

      initWasm().catch((error) => {
        console.error('[apex-state] Failed to load WASM:', error)
        throw error
      })
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
