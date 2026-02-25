import { useLayoutEffect, useRef } from 'react'

import { proxy, ref } from 'valtio'

import pkg from '../../package.json'
import { StoreContext } from '../core/context'
import { DEFAULT_STORE_CONFIG } from '../core/defaults'
import type {
  DebugTrack,
  InternalState,
  ProviderProps,
  StoreConfig,
  StoreInstance,
} from '../core/types'
import type { DeepRequired, GenericMeta } from '../types'
import type { DevToolsRef } from '../utils/debug-log'
import {
  connectPipelineDevTools,
  createPipelineObserver,
} from '../utils/debug-log'
import { deepClone } from '../utils/deep-clone'
import { deepMerge } from '../utils/deep-merge'
import {
  attachComputedGetters,
  prepareInitialState,
} from '../utils/derive-values'
import { createTiming } from '../utils/timing'
import { initPipeline, WasmGate } from '../wasm/lifecycle'
import { useStoreDevtools } from './devtools'

let storeIdCounter = 0

export const createInternalState = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  config: DeepRequired<StoreConfig>,
  devtools: DevToolsRef,
): InternalState<DATA, META> => ({
  graphs: {
    listenerHandlers: new Map(),
  },
  registrations: {
    concerns: new Map(),
    effectCleanups: new Set(),
    sideEffectCleanups: new Map(),
  },
  timing: createTiming(config.debug),
  observer: createPipelineObserver(config.debug, devtools),
  config,
  _devtools: devtools,
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

  // Stable DevTools ref for this Provider factory.
  // Shared across StrictMode remounts — no duplicates.
  storeIdCounter++
  const prefix = `apex-state@${pkg.version}:${resolvedConfig.name}-${String(storeIdCounter)}`
  const devtoolsRef: DevToolsRef = {
    prefix,
    pipeline: resolvedConfig.debug.devtools
      ? connectPipelineDevTools(prefix)
      : null,
  }

  // Build store instance from raw initialState + resolved config.
  // Pure function — no hooks, no side effects, safe to call during render.
  const buildStore = (rawInitialState: DATA): StoreInstance<DATA, META> => {
    const prepared = prepareInitialState(deepClone(rawInitialState))
    const internal = createInternalState<DATA, META>(
      resolvedConfig,
      devtoolsRef,
    )

    // Always create pipeline — WasmGate guarantees WASM is loaded.
    initPipeline(internal, prepared.initialState)

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
  }

  // Inner provider: WASM is guaranteed loaded when this renders (WasmGate blocks).
  // useRef runs exactly once — NOT double-invoked by StrictMode, no effects, no cleanup.
  const StoreProvider = ({ initialState, children }: ProviderProps<DATA>) => {
    const storeRef = useRef(buildStore(initialState))
    const internal = storeRef.current._internal

    // Redux DevTools: connect state and concerns proxies for inspection.
    useStoreDevtools(
      storeRef.current,
      resolvedConfig.debug.devtools,
      devtoolsRef,
    )

    // Deferred pipeline destroy: schedules cleanup on unmount, cancels on StrictMode re-mount.
    // StrictMode does unmount→mount synchronously — the 10s timer won't fire in between.
    // Real unmount: timer fires after 10s, destroying the WASM pipeline to prevent leaks.
    useLayoutEffect(() => {
      if (internal._destroyTimer) {
        clearTimeout(internal._destroyTimer)
        internal._destroyTimer = undefined
      }

      return () => {
        internal._destroyTimer = setTimeout(() => {
          internal.pipeline?.destroy()
          internal.pipeline = null
          internal.observer.destroy()
          internal._destroyTimer = undefined
        }, 10_000)
      }
    }, [internal])

    return (
      <StoreContext.Provider
        value={storeRef.current as unknown as StoreInstance<any, GenericMeta>}
      >
        {children}
      </StoreContext.Provider>
    )
  }

  StoreProvider.displayName = 'StoreProvider'

  // Always wrap with WasmGate — WASM is loaded regardless of mode.
  const Provider = (props: ProviderProps<DATA>) => (
    <WasmGate>
      <StoreProvider {...props} />
    </WasmGate>
  )

  Provider.displayName = 'StoreProvider'

  return Provider
}
