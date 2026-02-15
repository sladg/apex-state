/**
 * WASM Bridge - Thin wrapper over Rust/WASM exports
 *
 * Minimal serialization/deserialization layer. All heavy computation
 * lives in Rust — this bridge just handles JSON encoding at the boundary.
 *
 * WASM must be loaded once before use via `loadWasm()`:
 * - In production: called in `<Provider />` setup
 * - In tests: called in `beforeEach` / `beforeAll`
 *
 * After loading, all bridge functions are synchronous.
 *
 * @module wasm/bridge
 */

import type * as WasmExports from '../../rust/pkg-node/apex_state_wasm'

/** A single state change (input or output). */
export interface Change {
  path: string
  value: unknown
}

let wasmInstance: typeof WasmExports | null = null
let loadingPromise: Promise<void> | null = null

/**
 * Load the WASM module (async, call once at startup).
 * In production: dynamic import of bundler target.
 * In tests: use `initWasm()` to inject the node target synchronously.
 */
export const loadWasm = async (): Promise<void> => {
  if (wasmInstance) return
  if (loadingPromise) {
    await loadingPromise
    return
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
}

/**
 * Inject a pre-loaded WASM module (for testing with nodejs target).
 * Synchronous — no async loading needed.
 */
export const initWasm = (module: unknown): void => {
  wasmInstance = module as typeof WasmExports
}

/** Check if WASM module is loaded and ready for sync calls. */
export const isWasmLoaded = (): boolean => wasmInstance !== null

/** Reset WASM module and pipeline state (testing only). */
export const resetWasm = (): void => {
  if (wasmInstance) {
    wasmInstance.pipeline_reset()
  }
  wasmInstance = null
  loadingPromise = null
}

const wasm = (): typeof WasmExports => {
  if (!wasmInstance) {
    throw new Error('WASM not loaded. Call loadWasm() first.')
  }
  return wasmInstance
}

// ---------------------------------------------------------------------------
// Bridge functions — SYNC, thin wrappers with JSON serialization
// ---------------------------------------------------------------------------

/** Initialize shadow state from a JS object. */
export const shadowInit = (state: Record<string, unknown>): void => {
  wasm().shadow_init(JSON.stringify(state))
}

/** Register a BoolLogic expression. Returns logic_id for cleanup. */
export const registerBoolLogic = (outputPath: string, tree: unknown): number =>
  wasm().register_boollogic(outputPath, JSON.stringify(tree))

/** Unregister a BoolLogic expression by logic_id. */
export const unregisterBoolLogic = (logicId: number): void => {
  wasm().unregister_boollogic(logicId)
}

/** Process a batch of state changes through the WASM pipeline. */
export const processChanges = (changes: Change[]): Change[] => {
  const changesJson = JSON.stringify(
    changes.map((c) => ({
      path: c.path,
      value_json: JSON.stringify(c.value),
    })),
  )

  const resultJson = wasm().process_changes(changesJson)
  const result = JSON.parse(resultJson) as {
    changes: { path: string; value_json: string }[]
  }

  return result.changes.map((c) => ({
    path: c.path,
    value: JSON.parse(c.value_json) as unknown,
  }))
}

/** Dump shadow state as JS object (debug/testing). */
export const shadowDump = (): unknown =>
  JSON.parse(wasm().shadow_dump()) as unknown

/** Get a value from shadow state at a dot-path (debug/testing). */
export const shadowGet = (path: string): unknown => {
  const json = wasm().shadow_get(path)
  return json !== undefined ? (JSON.parse(json) as unknown) : undefined
}

/** Number of interned paths (debug/testing). */
export const internCount = (): number => wasm().intern_count()
