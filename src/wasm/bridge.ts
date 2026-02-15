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

  /** Register a batch of aggregations. */
  registerAggregationBatch: (aggregations: AggregationEntry[]): void => {
    getWasmInstance().register_aggregation_batch(JSON.stringify(aggregations))
  },

  /** Unregister a batch of aggregations by target paths. */
  unregisterAggregationBatch: (targets: string[]): void => {
    getWasmInstance().unregister_aggregation_batch(JSON.stringify(targets))
  },

  // -- Sync graph (EP2) -----------------------------------------------------

  /** Register a batch of sync pairs. */
  registerSyncBatch: (pairs: [string, string][]): void => {
    getWasmInstance().register_sync_batch(JSON.stringify(pairs))
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

  // -- Process changes ------------------------------------------------------

  /**
   * Process a batch of state changes through the WASM pipeline.
   * Uses serde-wasm-bindgen: passes JS objects directly (no JSON.stringify wrapper).
   * Returns state changes, concern changes, validators to run, and a pre-computed execution plan.
   */
  processChanges: (
    changes: Change[],
  ): {
    changes: Change[]
    concern_changes: Change[]
    validators_to_run: ValidatorDispatch[]
    execution_plan: FullExecutionPlan | null
  } => {
    // Pass WasmChange[] directly via serde-wasm-bindgen (no outer JSON.stringify)
    const result = getWasmInstance().process_changes(
      changesToWasm(changes) as never,
    ) as unknown as {
      changes: WasmChange[]
      concern_changes: WasmChange[]
      validators_to_run?: ValidatorDispatch[]
      execution_plan?: FullExecutionPlan
    }

    return {
      changes: wasmChangesToJs(result.changes),
      concern_changes: wasmChangesToJs(result.concern_changes ?? []),
      validators_to_run: result.validators_to_run ?? [],
      execution_plan: result.execution_plan ?? null,
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

  // -- Validator registry (EP4) ---------------------------------------------

  /** Register a batch of validators. */
  registerValidatorsBatch: (validators: ValidatorEntry[]): void => {
    getWasmInstance().register_validators_batch(JSON.stringify(validators))
  },

  /** Unregister a batch of validators by validator IDs. */
  unregisterValidatorsBatch: (validatorIds: number[]): void => {
    getWasmInstance().unregister_validators_batch(JSON.stringify(validatorIds))
  },

  // -- Debug/testing --------------------------------------------------------

  /** Number of interned paths (debug/testing). */
  internCount: (): number => getWasmInstance().intern_count(),
}
