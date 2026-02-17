/**
 * WASM Bridge — Thin namespace over Rust/WASM exports.
 *
 * Uses serde-wasm-bindgen for hot-path functions (processChanges,
 * shadowInit) — JS objects cross the boundary directly without JSON
 * string intermediary. Registration functions still use JSON strings
 * (cold path, simpler).
 *
 * Loading is handled by `wasm/lifecycle.ts`. After loading, all bridge
 * functions are synchronous.
 *
 * @module wasm/bridge
 */

import type { z } from 'zod'

import { createFastJson } from '../utils/json'
import { getWasmInstance } from './lifecycle'

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
// Consolidated registration types
// ---------------------------------------------------------------------------

/** Consolidated registration input for side effects (sync, flip, aggregation, clear, listeners). */
export interface SideEffectsRegistration {
  registration_id: string
  sync_pairs?: [string, string][]
  flip_pairs?: [string, string][]
  aggregation_pairs?: [string, string][]
  clear_paths?: { triggers: string[]; targets: string[] }[]
  listeners?: {
    subscriber_id: number
    topic_path: string
    scope_path: string
  }[]
}

/** Consolidated registration output from side effects registration. */
export interface SideEffectsResult {
  sync_changes: Change[]
  aggregation_changes: Change[]
  registered_listener_ids: number[]
}

/** Consolidated registration input for concerns (BoolLogic, validators, and ValueLogic). */
export interface ConcernsRegistration {
  registration_id: string
  bool_logics?: {
    output_path: string
    tree_json: string
  }[]
  validators?: {
    validator_id: number
    output_path: string
    dependency_paths: string[]
    scope: string
  }[]
  value_logics?: {
    output_path: string
    tree_json: string
  }[]
}

/** Consolidated registration output from concerns registration. */
export interface ConcernsResult {
  bool_logic_changes: Change[]
  registered_logic_ids: number[]
  registered_validator_ids: number[]
  value_logic_changes: Change[]
  registered_value_logic_ids: number[]
}

// ---------------------------------------------------------------------------
// Internal WASM change format (path + value_json string)
// ---------------------------------------------------------------------------

interface WasmChange {
  path: string
  value_json: string
}

// ---------------------------------------------------------------------------
// Helpers — conversion between JS Change[] and WASM WasmChange[]
// ---------------------------------------------------------------------------

/**
 * Sentinel for JS `undefined` crossing the WASM boundary.
 * JSON has no `undefined` concept, so we encode it as a JSON string sentinel.
 * Never collides with user data — no sane user value matches this marker.
 */
const UNDEFINED_SENTINEL_JSON = '"__APEX_UNDEFINED__"'

const { stringify: fastStringify, parse: fastParse } = createFastJson([
  { value: undefined, encoded: UNDEFINED_SENTINEL_JSON },
])

/** Convert JS Change[] to WASM's { path, value_json }[] for serde-wasm-bindgen. */
const changesToWasm = (changes: Change[]): WasmChange[] =>
  changes.map(({ path, value }) => ({ path, value_json: fastStringify(value) }))

/** Convert WASM's { path, value_json }[] back to JS Change[]. */
const wasmChangesToJs = (wasmChanges: WasmChange[]): Change[] =>
  wasmChanges.map(({ path, value_json }) => ({
    path,
    value: fastParse(value_json),
  }))

// ---------------------------------------------------------------------------
// ProcessChanges result type
// ---------------------------------------------------------------------------

/** Result of processChanges (Phase 1). */
export interface ProcessChangesResult {
  state_changes: Change[]
  changes: Change[] // Backwards compat alias
  validators_to_run: ValidatorDispatch[]
  execution_plan: FullExecutionPlan | null
  has_work: boolean
}

// ---------------------------------------------------------------------------
// WasmPipeline — per-store isolated pipeline instance
// ---------------------------------------------------------------------------

/**
 * An isolated WASM pipeline instance.
 * Each store gets its own pipeline so multiple Providers don't interfere.
 * All methods are pre-bound to the pipeline's ID — consumers never pass IDs.
 */
export interface WasmPipeline {
  readonly id: number
  shadowInit: (state: object) => void
  shadowDump: () => unknown
  processChanges: (changes: Change[]) => ProcessChangesResult
  pipelineFinalize: (jsChanges: Change[]) => { state_changes: Change[] }
  registerSideEffects: (reg: SideEffectsRegistration) => SideEffectsResult
  unregisterSideEffects: (registrationId: string) => void
  registerConcerns: (reg: ConcernsRegistration) => ConcernsResult
  unregisterConcerns: (registrationId: string) => void
  registerBoolLogic: (outputPath: string, tree: unknown) => number
  unregisterBoolLogic: (logicId: number) => void
  pipelineReset: () => void
  destroy: () => void
  /** Per-instance storage for Zod schemas (can't cross WASM boundary). */
  validatorSchemas: Map<number, z.ZodSchema>
}

/**
 * Create a new isolated WASM pipeline instance.
 * Each store should call this once and store the result.
 * Call pipeline.destroy() on cleanup.
 */
export const createWasmPipeline = (): WasmPipeline => {
  const wasm = getWasmInstance()
  const id = wasm.pipeline_create()
  const schemas = new Map<number, z.ZodSchema>()

  return {
    id,

    shadowInit: (state) => {
      wasm.shadow_init(id, state)
    },

    shadowDump: () => JSON.parse(wasm.shadow_dump(id)) as unknown,

    processChanges: (changes) => {
      const result = wasm.process_changes(
        id,
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
        changes: stateChanges,
        validators_to_run: result.validators_to_run ?? [],
        execution_plan: result.execution_plan ?? null,
        has_work: result.has_work ?? false,
      }
    },

    pipelineFinalize: (jsChanges) => {
      const result = wasm.pipeline_finalize(
        id,
        changesToWasm(jsChanges) as never,
      ) as unknown as {
        state_changes: WasmChange[]
      }

      return {
        state_changes: wasmChangesToJs(result.state_changes),
      }
    },

    registerSideEffects: (reg) => {
      const resultJson = wasm.register_side_effects(
        id,
        JSON.stringify(reg),
      ) as unknown as {
        sync_changes: WasmChange[]
        aggregation_changes: WasmChange[]
        registered_listener_ids: number[]
      }
      return {
        sync_changes: wasmChangesToJs(resultJson.sync_changes),
        aggregation_changes: wasmChangesToJs(resultJson.aggregation_changes),
        registered_listener_ids: resultJson.registered_listener_ids,
      }
    },

    unregisterSideEffects: (registrationId) => {
      wasm.unregister_side_effects(id, registrationId)
    },

    registerConcerns: (reg) => {
      const resultJson = wasm.register_concerns(
        id,
        JSON.stringify(reg),
      ) as unknown as {
        bool_logic_changes: WasmChange[]
        registered_logic_ids: number[]
        registered_validator_ids: number[]
        value_logic_changes: WasmChange[]
        registered_value_logic_ids: number[]
      }
      return {
        bool_logic_changes: wasmChangesToJs(resultJson.bool_logic_changes),
        registered_logic_ids: resultJson.registered_logic_ids,
        registered_validator_ids: resultJson.registered_validator_ids,
        value_logic_changes: wasmChangesToJs(
          resultJson.value_logic_changes ?? [],
        ),
        registered_value_logic_ids: resultJson.registered_value_logic_ids ?? [],
      }
    },

    unregisterConcerns: (registrationId) => {
      wasm.unregister_concerns(id, registrationId)
    },

    registerBoolLogic: (outputPath, tree) =>
      wasm.register_boollogic(id, outputPath, JSON.stringify(tree)),

    unregisterBoolLogic: (logicId) => {
      wasm.unregister_boollogic(id, logicId)
    },

    pipelineReset: () => {
      wasm.pipeline_reset(id)
    },

    destroy: () => {
      wasm.pipeline_destroy(id)
      schemas.clear()
    },

    validatorSchemas: schemas,
  }
}
