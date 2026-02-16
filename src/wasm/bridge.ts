/**
 * WASM Bridge - Thin wrapper over Rust/WASM exports
 *
 * Uses serde-wasm-bindgen for hot-path functions (processChanges,
 * createDispatchPlan, routeProducedChanges, shadowInit) — JS objects
 * cross the boundary directly without JSON string intermediary.
 *
 * Registration functions still use JSON strings (cold path, simpler).
 *
 * WASM must be loaded once before use via `loadWasm()`:
 * - In production: called in `<Provider />` setup
 * - In tests: called in `beforeEach` / `beforeAll`
 *
 * After loading, all bridge functions are synchronous.
 *
 * @module wasm/bridge
 */

import type { z } from 'zod'

import type * as WasmExports from '../../rust/pkg/apex_state_wasm'

// ---------------------------------------------------------------------------
// Types — exported for downstream use
// ---------------------------------------------------------------------------

/** A single state change (input or output). */
export interface Change {
  path: string
  value: unknown
}

/** A single dispatch entry with sequential ID and input change references. */
export interface DispatchEntry {
  dispatch_id: number
  subscriber_id: number
  scope_path: string
  /** Indexes into ProcessResult.changes array. */
  input_change_ids: number[]
}

/** A group of dispatches to execute sequentially. */
export interface DispatchGroup {
  dispatches: DispatchEntry[]
}

/** A target for propagating produced changes from child to parent dispatch. */
export interface PropagationTarget {
  target_dispatch_id: number
  /** Prefix to prepend to child's relative paths for the target's scope. */
  remap_prefix: string
}

/** Pre-computed execution plan with propagation map. */
export interface FullExecutionPlan {
  groups: DispatchGroup[]
  /** propagation_map[dispatch_id] = targets to forward produced changes to. */
  propagation_map: PropagationTarget[][]
}

// ---------------------------------------------------------------------------
// Legacy types — kept for backward compat with createDispatchPlan/routeProducedChanges
// ---------------------------------------------------------------------------

/** @deprecated Use FullExecutionPlan instead. */
export interface ListenerDispatch {
  subscriber_id: number
  scope_path: string
  changes: Change[]
  ancestors?: string[]
}

/** @deprecated Use FullExecutionPlan instead. */
export interface DispatchLevel {
  depth: number
  dispatches: ListenerDispatch[]
}

/** @deprecated Use FullExecutionPlan instead. */
export interface DispatchPlan {
  levels: DispatchLevel[]
}

// ---------------------------------------------------------------------------
// Registration types
// ---------------------------------------------------------------------------

/** An aggregation registration entry: target path + source paths. */
export interface AggregationEntry {
  target: string
  sources: string[]
}

/** A listener registration entry for topic-based dispatch. */
export interface ListenerEntry {
  subscriber_id: number
  topic_path: string
  scope_path: string
}

/** A validator registration entry for validation orchestration. */
export interface ValidatorEntry {
  validator_id: number
  output_path: string
  dependency_paths: string[]
}

/** A generic function registration entry (concerns, validators, listeners). */
export interface FunctionEntry {
  function_id: number
  dependency_paths: string[]
  scope: string
  output_path?: string
}

/** Validator dispatch info for JS-side execution. */
export interface ValidatorDispatch {
  validator_id: number
  output_path: string
  dependency_values: Record<string, string>
}

// ---------------------------------------------------------------------------
// Internal WASM change format (path + value_json string)
// ---------------------------------------------------------------------------

interface WasmChange {
  path: string
  value_json: string
}

// ---------------------------------------------------------------------------
// Module loading
// ---------------------------------------------------------------------------

let wasmInstance: typeof WasmExports | null = null
let loadingPromise: Promise<void> | null = null

/**
 * Load the WASM module (async, call once at startup).
 * In production: dynamic import of bundler target.
 * In tests: use `initWasm()` to inject the node target synchronously.
 * Returns the loaded WASM instance for inspection/testing.
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

/**
 * Inject a pre-loaded WASM module (for testing with nodejs target).
 * Synchronous — no async loading needed.
 */
export const initWasm = (module: unknown): void => {
  wasmInstance = module as typeof WasmExports
}

/** Check if WASM module is loaded and ready for sync calls. */
export const isWasmLoaded = (): boolean => wasmInstance !== null

/**
 * Check if WASM should be used for this store instance.
 * Throws if WASM mode is requested but WASM is not loaded.
 */
export const shouldUseWasm = (store: {
  _internal: { config: { useLegacyImplementation: boolean } }
}): boolean => {
  const useWasm = !store._internal.config.useLegacyImplementation

  if (useWasm && !isWasmLoaded()) {
    throw new Error(
      'WASM mode requested (useLegacyImplementation: false) but WASM is not loaded. Call loadWasm() before creating the store.',
    )
  }

  return useWasm
}

/** Reset WASM module and pipeline state (testing only). */
export const resetWasm = (): void => {
  if (wasmInstance) {
    wasmInstance.pipeline_reset()
  }
  wasmInstance = null
  loadingPromise = null
}

const getWasmInstance = (): typeof WasmExports => {
  if (!wasmInstance) {
    throw new Error('WASM not loaded. Call loadWasm() first.')
  }
  return wasmInstance
}

// ---------------------------------------------------------------------------
// Validator schema storage (Zod schemas can't cross WASM boundary)
// ---------------------------------------------------------------------------

/**
 * JS-side validator schema storage.
 * Maps validator_id (from WASM) to ZodSchema for execution.
 */
export const validatorSchemas = new Map<number, z.ZodSchema>()

// ---------------------------------------------------------------------------
// Helpers — conversion between JS Change[] and WASM WasmChange[]
// ---------------------------------------------------------------------------

/** Convert JS Change[] to WASM's { path, value_json }[] for serde-wasm-bindgen. */
const changesToWasm = (changes: Change[]): WasmChange[] =>
  changes.map((c) => ({
    path: c.path,
    value_json: JSON.stringify(c.value),
  }))

/** Convert WASM's { path, value_json }[] back to JS Change[]. */
const wasmChangesToJs = (wasmChanges: WasmChange[]): Change[] =>
  wasmChanges.map((c) => ({
    path: c.path,
    value: JSON.parse(c.value_json) as unknown,
  }))

// ---------------------------------------------------------------------------
// WASM namespace — Single export boundary
// ---------------------------------------------------------------------------

/**
 * All WASM functions accessible through this single namespace.
 * This is the primary interface for crossing the JS↔WASM boundary.
 *
 * Usage: `import { wasm } from './wasm/bridge'` then `wasm.processChanges(...)`
 */
export const wasm = {
  // -- Shadow state ---------------------------------------------------------

  /** Initialize shadow state from a JS object (no JSON serialization — direct JsValue). */
  shadowInit: (state: Record<string, unknown>): void => {
    getWasmInstance().shadow_init(state as never)
  },

  /** Dump shadow state as JS object (debug/testing). */
  shadowDump: (): unknown =>
    JSON.parse(getWasmInstance().shadow_dump()) as unknown,

  /** Get a value from shadow state at a dot-path (debug/testing). */
  shadowGet: (path: string): unknown => {
    const json = getWasmInstance().shadow_get(path)
    return json !== undefined ? (JSON.parse(json) as unknown) : undefined
  },

  // -- BoolLogic ------------------------------------------------------------

  /** Register a BoolLogic expression. Returns logic_id for cleanup. */
  registerBoolLogic: (outputPath: string, tree: unknown): number =>
    getWasmInstance().register_boollogic(outputPath, JSON.stringify(tree)),

  /** Unregister a BoolLogic expression by logic_id. */
  unregisterBoolLogic: (logicId: number): void => {
    getWasmInstance().unregister_boollogic(logicId)
  },

  // -- Aggregation (EP2) ----------------------------------------------------

  /**
   * Register aggregations from raw [target, source] pairs.
   * Rust handles validation, grouping, and initial value computation.
   * Returns initial changes to apply.
   */
  registerAggregationBatch: (pairs: [string, string][]): Change[] => {
    const wasmModule = getWasmInstance() as any
    const resultJson = wasmModule.register_aggregation_batch(
      JSON.stringify(pairs),
    ) as string
    const wasmChanges = JSON.parse(resultJson) as WasmChange[]
    return wasmChangesToJs(wasmChanges)
  },

  /** Unregister a batch of aggregations by target paths. */
  unregisterAggregationBatch: (targets: string[]): void => {
    getWasmInstance().unregister_aggregation_batch(JSON.stringify(targets))
  },

  // -- Sync graph (EP2) -----------------------------------------------------

  /**
   * Register a batch of sync pairs.
   * Computes initial sync changes from shadow state, updates shadow, and returns changes.
   * Returns initial changes to apply to valtio.
   */
  registerSyncBatch: (pairs: [string, string][]): Change[] => {
    const wasmModule = getWasmInstance() as any
    const resultJson = wasmModule.register_sync_batch(
      JSON.stringify(pairs),
    ) as string

    // Handle case where WASM returns "undefined" (incomplete implementation)
    if (!resultJson || resultJson === 'undefined') {
      return []
    }

    const wasmChanges = JSON.parse(resultJson) as WasmChange[]
    return wasmChangesToJs(wasmChanges)
  },

  /** Unregister a batch of sync pairs. */
  unregisterSyncBatch: (pairs: [string, string][]): void => {
    getWasmInstance().unregister_sync_batch(JSON.stringify(pairs))
  },

  // -- Flip graph (EP2) -----------------------------------------------------

  /** Register a batch of flip pairs. */
  registerFlipBatch: (pairs: [string, string][]): void => {
    getWasmInstance().register_flip_batch(JSON.stringify(pairs))
  },

  /** Unregister a batch of flip pairs. */
  unregisterFlipBatch: (pairs: [string, string][]): void => {
    getWasmInstance().unregister_flip_batch(JSON.stringify(pairs))
  },

  // -- Pipeline (EP2) -------------------------------------------------------

  /** Reset the pipeline to a fresh state (testing only). */
  pipelineReset: (): void => {
    getWasmInstance().pipeline_reset()
  },

  // -- Process changes (Phase 1) ------------------------------------------------

  /**
   * Process a batch of state changes through the WASM pipeline (Phase 1).
   *
   * Always diffs incoming changes against shadow state to filter out no-ops before
   * entering the pipeline. Early exits if all changes are no-ops.
   *
   * Updates shadow state during processing (needed for BoolLogic evaluation).
   * Returns readonly context for JS listener execution + validators + execution plan + work flag.
   *
   * Uses serde-wasm-bindgen: passes JS objects directly (no JSON.stringify wrapper).
   */
  processChanges: (
    changes: Change[],
  ): {
    state_changes: Change[]
    changes: Change[] // Backwards compat alias
    validators_to_run: ValidatorDispatch[]
    execution_plan: FullExecutionPlan | null
    has_work: boolean
  } => {
    const result = getWasmInstance().process_changes(
      changesToWasm(changes) as never,
    ) as unknown as {
      state_changes: WasmChange[]
      validators_to_run?: ValidatorDispatch[]
      execution_plan?: FullExecutionPlan
      has_work?: boolean
    }

    const stateChanges = wasmChangesToJs(result.state_changes)
    return {
      state_changes: stateChanges,
      changes: stateChanges, // Backwards compat alias
      validators_to_run: result.validators_to_run ?? [],
      execution_plan: result.execution_plan ?? null,
      has_work: result.has_work ?? false,
    }
  },

  /**
   * Finalize pipeline with JS changes (listeners + validators mixed) (Phase 2).
   *
   * Merges js_changes with pending buffers, diffs against shadow state,
   * updates shadow, returns final changes for valtio.
   *
   * Input: Single flat array mixing listener output + validator output (with _concerns. prefix)
   * Output: { state_changes } - all changes including those with _concerns. prefix
   *
   * Uses serde-wasm-bindgen: passes JS objects directly (no JSON.stringify wrapper).
   */
  pipelineFinalize: (jsChanges: Change[]): { state_changes: Change[] } => {
    const wasmModule = getWasmInstance()
    const result = wasmModule.pipeline_finalize(
      changesToWasm(jsChanges) as never,
    ) as unknown as {
      state_changes: WasmChange[]
    }

    return {
      state_changes: wasmChangesToJs(result.state_changes),
    }
  },

  // -- Listener dispatch (EP3) ----------------------------------------------

  /** Register a batch of listeners for topic-based dispatch. */
  registerListenersBatch: (listeners: ListenerEntry[]): void => {
    getWasmInstance().register_listeners_batch(JSON.stringify(listeners))
  },

  /** Unregister a batch of listeners by subscriber IDs. */
  unregisterListenersBatch: (subscriberIds: number[]): void => {
    getWasmInstance().unregister_listeners_batch(JSON.stringify(subscriberIds))
  },

  /** Create a dispatch plan for the given changes (serde-wasm-bindgen). */
  createDispatchPlan: (changes: Change[]): DispatchPlan => {
    const raw = getWasmInstance().create_dispatch_plan(
      changesToWasm(changes) as never,
    ) as unknown as {
      levels: {
        depth: number
        dispatches: {
          subscriber_id: number
          scope_path: string
          changes: WasmChange[]
        }[]
      }[]
    }

    return {
      levels: raw.levels.map((level) => ({
        depth: level.depth,
        dispatches: level.dispatches.map((d) => ({
          subscriber_id: d.subscriber_id,
          scope_path: d.scope_path,
          changes: wasmChangesToJs(d.changes),
        })),
      })),
    }
  },

  /** Route produced changes from a depth level to downstream topics (serde-wasm-bindgen). */
  routeProducedChanges: (
    depth: number,
    producedChanges: Change[],
  ): DispatchPlan | null => {
    const raw = getWasmInstance().route_produced_changes(
      depth,
      changesToWasm(producedChanges) as never,
    ) as unknown as {
      levels: {
        depth: number
        dispatches: {
          subscriber_id: number
          scope_path: string
          changes: WasmChange[]
        }[]
      }[]
    } | null

    if (!raw || raw.levels.length === 0) return null

    return {
      levels: raw.levels.map((level) => ({
        depth: level.depth,
        dispatches: level.dispatches.map((d) => ({
          subscriber_id: d.subscriber_id,
          scope_path: d.scope_path,
          changes: wasmChangesToJs(d.changes),
        })),
      })),
    }
  },

  // -- Generic function registry (EP6) --------------------------------------

  /** Register a batch of generic functions (concerns, validators, listeners). */
  registerFunctionsBatch: (functions: FunctionEntry[]): void => {
    getWasmInstance().register_functions_batch(JSON.stringify(functions))
  },

  /** Unregister a batch of functions by function IDs. */
  unregisterFunctionsBatch: (functionIds: number[]): void => {
    getWasmInstance().unregister_functions_batch(JSON.stringify(functionIds))
  },

  // -- Debug/testing --------------------------------------------------------

  /** Number of interned paths (debug/testing). */
  internCount: (): number => getWasmInstance().intern_count(),
}
