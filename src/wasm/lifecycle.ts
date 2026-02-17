/**
 * WASM Lifecycle — Loading, instance management, and teardown.
 *
 * Owns the singleton WASM instance. All loading and reset logic lives here.
 * bridge.ts imports `getWasmInstance()` to power the `wasm` namespace.
 *
 * - Production: `loadWasm()` called in `<Provider />` setup
 * - Tests: `loadWasm()` called in `beforeEach`, `resetWasm()` in `afterEach`
 *
 * @module wasm/lifecycle
 */

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
    const wasmModule = (await import(
      /* @vite-ignore */
      '../../rust/pkg/apex_state_wasm.js'
    )) as unknown as typeof WasmExports & { default?: () => Promise<void> }

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
  internal: InternalState<any, any>,
  initialState: object,
): void => {
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(initialState)
  internal.pipeline = pipeline
}

/** Reset WASM module and pipeline state (testing only). */
export const resetWasm = (): void => {
  try {
    wasmInstance?.pipeline_reset_all()
  } catch {
    // Instance may not be loaded — safe to ignore
  }
  wasmInstance = null
  loadingPromise = null
}

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
