import { useLayoutEffect, useRef } from 'react'

import { proxy, ref } from 'valtio'

import { StoreContext } from '../core/context'
import { DEFAULT_STORE_CONFIG } from '../core/defaults'
import type {
  DebugTrack,
  InternalState,
  ProviderProps,
  StoreConfig,
  StoreInstance,
} from '../core/types'
import type { DeepRequired } from '../types'
import { deepClone } from '../utils/deep-clone'
import { deepMerge } from '../utils/deep-merge'
import {
  attachComputedGetters,
  prepareInitialState,
} from '../utils/derive-values'
import { createLogger } from '../utils/log'
import { initPipeline, WasmGate } from '../wasm/lifecycle'
import { attachDevtools } from './devtools'

export const createInternalState = (
  config: DeepRequired<StoreConfig>,
): InternalState => ({
  registrations: {
    concerns: new Map(),
    effectCleanups: new Set(),
    sideEffectCleanups: new Map(),
    listenerHandlers: new Map(),
  },
  logger: createLogger(config.debug),
  config,
  devtools: null,
  pipeline: null,
})

export const createProvider = <DATA extends object>(
  storeConfig?: StoreConfig,
) => {
  // Resolve config with defaults at factory time
  const resolvedConfig = deepMerge(DEFAULT_STORE_CONFIG, storeConfig)

  // Build store instance from raw initialState + resolved config.
  // Pure function — no hooks, no side effects, safe to call during render.
  const buildStore = (rawInitialState: DATA): StoreInstance<DATA> => {
    const prepared = prepareInitialState(deepClone(rawInitialState))
    const internal = createInternalState(resolvedConfig)

    // Always create pipeline — WasmGate guarantees WASM is loaded.
    initPipeline(internal, prepared.initialState, {
      debug: resolvedConfig.debug.log ?? false,
    })

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

    const concernsProxy = proxy({} as Record<string, Record<string, unknown>>)

    internal.devtools = attachDevtools(
      resolvedConfig,
      stateProxy,
      concernsProxy,
    )

    return {
      // state: Application data (tracked by valtio)
      // User actions WRITE to this, effects READ from this
      state: stateProxy,

      // _concerns: Computed concern values (tracked by valtio)
      // Effects WRITE to this, UI components READ from this
      _concerns: concernsProxy,

      // _internal: Graphs, registrations, processing (NOT tracked)
      // Wrapped in ref() to prevent tracking even if store is later wrapped in a proxy
      _internal: ref(internal),

      // _debug: Tracking data for testing/debugging (only when debug.track enabled)
      _debug: debugTrack ? ref(debugTrack) : null,
    }
  }

  // Inner provider: WASM is guaranteed loaded when this renders (WasmGate blocks).
  // useRef runs exactly once — NOT double-invoked by StrictMode, no effects, no cleanup.
  //
  // Pipeline lifecycle: No deferred destroy timer. Benchmarking showed the StrictMode
  // register → unregister → register cycle takes ~1.5ms vs ~2.5ms with full rebuild —
  // negligible difference that doesn't justify the timer complexity. Instead, the pipeline
  // is destroyed immediately on unmount and rebuilt during render if needed.
  const StoreProvider = ({ initialState, children }: ProviderProps<DATA>) => {
    const storeRef = useRef(buildStore(initialState))
    const internal = storeRef.current._internal

    // Guard 1 — render phase (actual remount after timer fired).
    // Handles: Provider truly unmounts and remounts (e.g. conditional rendering) after the
    // deferred destroy timer fires and sets pipeline = null.
    // Why here: render is parent-first and runs BEFORE any effects. Children's
    // useLayoutEffects (useSideEffects, useConcerns registration) fire bottom-up AFTER
    // the render phase, so placing init here guarantees the pipeline exists before any
    // child effect runs. Moving this into useLayoutEffect would be too late.
    if (!internal.pipeline) {
      const { initialState: cleanState } = prepareInitialState(
        deepClone(initialState),
      )
      initPipeline(internal, cleanState, {
        debug: resolvedConfig.debug.log ?? false,
      })
    }

    // Guard 2 — deferred destroy (React StrictMode).
    // Handles: StrictMode's double-effect cycle (cleanup → re-run) without destroying
    // the pipeline during the gap.
    //
    // Problem: effects fire bottom-up (children before parent). If we destroy in cleanup,
    // children's useSideEffects/useConcerns re-register with null pipeline before the
    // parent's useLayoutEffect body can rebuild it. useInsertionEffect doesn't reliably
    // fire during StrictMode re-run in JSDOM (empirically confirmed).
    //
    // Solution: defer destruction with setTimeout(0). StrictMode re-mounts synchronously
    // cancel the timer before it fires, so the pipeline survives the cycle intact.
    // Actual unmounts let the timer fire and destroy.
    const destroyTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null)

    useLayoutEffect(() => {
      // Cancel deferred destruction — we're re-mounting (StrictMode or conditional render)
      if (destroyTimerRef.current !== null) {
        clearTimeout(destroyTimerRef.current)
        destroyTimerRef.current = null
      }

      return () => {
        const pipelineToDestroy = internal.pipeline
        const devtoolsToDestroy = internal.devtools

        destroyTimerRef.current = setTimeout(() => {
          pipelineToDestroy?.destroy()
          devtoolsToDestroy?.destroy()
          // Only null out if not rebuilt by a remount (Guard 1 creates a new instance)
          if (internal.pipeline === pipelineToDestroy) internal.pipeline = null
          if (internal.devtools === devtoolsToDestroy) internal.devtools = null
          destroyTimerRef.current = null
        }, 0)
      }
    }, [internal])

    return (
      <StoreContext.Provider
        value={storeRef.current as unknown as StoreInstance<any>}
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
