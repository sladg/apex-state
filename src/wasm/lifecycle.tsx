/**
 * WASM Lifecycle — Loading, instance management, gating, and teardown.
 *
 * Owns the singleton WASM instance. All loading and reset logic lives here.
 * bridge.ts imports `getWasmInstance()` to power the `wasm` namespace.
 *
 * - Production: `WasmGate` component loads WASM before rendering children
 * - Tests: `loadWasm()` called in `beforeEach`
 *
 * @module wasm/lifecycle
 */

import { useEffect, useState } from 'react'

import type * as WasmExports from '../../rust/pkg/apex_state_wasm'
import type { InternalState } from '../core/types'
import { createWasmPipeline } from './bridge'

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

let wasmInstance: typeof WasmExports | null = null
let loadingPromise: Promise<void> | null = null

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * Load the WASM module (async, call once at startup).
 * Deduplicates concurrent calls — safe to call from multiple components.
 */
export const loadWasm = async (): Promise<typeof WasmExports> => {
  if (wasmInstance) return wasmInstance
  if (loadingPromise) {
    await loadingPromise
    return wasmInstance!
  }

  loadingPromise = (async () => {
    const wasmModule =
      (await import('../../rust/pkg/apex_state_wasm.js')) as unknown as typeof WasmExports & {
        default?: () => Promise<void>
      }

    if (typeof wasmModule.default === 'function') {
      await wasmModule.default()
    }
    wasmInstance = wasmModule
  })()

  await loadingPromise
  return wasmInstance!
}

/** Check if WASM module is loaded and ready for sync calls. */
export const isWasmLoaded = (): boolean => wasmInstance !== null

/** Create a per-store WASM pipeline and initialize its shadow state. */
export const initPipeline = (
  internal: InternalState,
  initialState: object,
  options?: { debug?: boolean },
): void => {
  const pipeline = createWasmPipeline(options)
  pipeline.shadowInit(initialState)
  internal.pipeline = pipeline
}

// ---------------------------------------------------------------------------
// WasmGate — React component that blocks children until WASM is loaded.
//
// Separates async WASM loading from Provider's synchronous store setup.
// Provider can assume WASM is always available — no async logic needed.
// ---------------------------------------------------------------------------

/**
 * Blocks rendering of children until the WASM module is loaded.
 * Renders `null` while loading, then renders children once ready.
 */
export const WasmGate = ({ children }: { children: React.ReactNode }) => {
  const [loaded, setLoaded] = useState(isWasmLoaded)

  useEffect(() => {
    if (loaded) return

    let cancelled = false
    loadWasm()
      .then(() => {
        if (!cancelled) setLoaded(true)
      })
      .catch((error) => {
        console.error('[apex-state] Failed to load WASM:', error)
      })

    return () => {
      cancelled = true
    }
  }, [loaded])

  if (!loaded) return null

  return <>{children}</>
}

WasmGate.displayName = 'WasmGate'

// ---------------------------------------------------------------------------
// Internal — used by bridge.ts
// ---------------------------------------------------------------------------

/** Get the loaded WASM instance or throw. */
export const getWasmInstance = (): typeof WasmExports => {
  if (!wasmInstance) {
    throw new Error('WASM not loaded. Call loadWasm() first.')
  }
  return wasmInstance
}
