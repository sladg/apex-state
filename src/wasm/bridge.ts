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

import type { Change } from '../types/changes'
import type { ValidationSchema } from '../types/concerns'
import type { GenericMeta } from '../types/meta'
import { createFastJson } from '../utils/json'
import type * as Wasm from './generated/types'
import { getWasmInstance } from './lifecycle'

export type { Change }

/** Map a Wasm wire result type to JS-facing type: Wasm.Change[] → Change[], rest untouched. */
type WireToJs<T> = {
  [K in keyof T]: T[K] extends Wasm.Change[] ? Change[] : T[K]
}

// ---------------------------------------------------------------------------
// Result types — Wasm wire types with parsed Change values
// ---------------------------------------------------------------------------

/** JS-facing side effects result — same shape as Wasm wire type but with parsed Change values. */
export type SideEffectsResult = WireToJs<Wasm.SideEffectsResult>

/** JS-facing concerns result — same shape as Wasm wire type but with parsed Change values. */
export type ConcernsResult = WireToJs<Wasm.ConcernsResult>

// ---------------------------------------------------------------------------
// Helpers — conversion between JS Change[] and WASM WasmChangeWire[]
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

/** Convert JS Change[] to WASM wire format for serde-wasm-bindgen. */
const changesToWasm = (changes: Change[]) =>
  changes.map(({ path, value, meta }) => ({
    path,
    value_json: fastStringify(value),
    meta,
  }))

/** Map a WASM lineage stage to the appropriate GenericMeta boolean flag. */
const lineageToMeta = (lineage: Wasm.Lineage): Partial<GenericMeta> => {
  if (lineage === 'Input') return {}
  switch (lineage.Derived.via) {
    case 'sync':
      return { isSyncPathChange: true }
    case 'flip':
      return { isFlipPathChange: true }
    case 'aggregation_write':
    case 'aggregation_read':
      return { isAggregationChange: true }
    case 'listeners':
      return { isListenerChange: true }
    case 'clear_path':
      return { isClearPathChange: true }
    case 'computation':
      return { isComputationChange: true }
    default:
      return { isProgramaticChange: true }
  }
}

/** Convert WASM wire format back to JS Change[], transforming lineage into GenericMeta flags. */
const wasmChangesToJs = (wasmChanges: Wasm.Change[]): Change[] =>
  wasmChanges.map(({ path, value_json, meta, lineage }) => ({
    path,
    value: fastParse(value_json),
    meta: { ...(meta as GenericMeta), ...lineageToMeta(lineage) },
  }))

/**
 * Create a new isolated WASM pipeline instance.
 * Each store should call this once and store the result.
 * Call pipeline.destroy() on cleanup.
 */
export const createWasmPipeline = (options?: { debug?: boolean }) => {
  const wasm = getWasmInstance()
  const id = wasm.pipeline_create(options)
  const schemas = new Map<number, ValidationSchema>()
  let destroyed = false

  return {
    id,
    get destroyed() {
      return destroyed
    },

    shadowInit: (state: object) => {
      wasm.shadow_init(id, fastStringify(state))
    },
    shadowDump: () => fastParse(wasm.shadow_dump(id)),
    processChanges: (changes: Change[]) => {
      const result = wasm.process_changes(
        id,
        changesToWasm(changes),
      ) as unknown as Wasm.PrepareResult

      const listenerChanges = wasmChangesToJs(result.listener_changes)
      return {
        listener_changes: listenerChanges,
        validators_to_run: result.validators_to_run,
        execution_plan: result.execution_plan,
        has_work: result.has_work,
      }
    },

    pipelineFinalize: (changes: Change[]) => {
      const result = wasm.pipeline_finalize(
        id,
        changesToWasm(changes),
      ) as unknown as Wasm.FinalizeResult

      return {
        state_changes: wasmChangesToJs(result.state_changes),
        trace: result.trace ?? null,
      }
    },

    registerSideEffects: (reg: Partial<Wasm.SideEffectsRegistration>) => {
      const result = wasm.register_side_effects(
        id,
        JSON.stringify(reg),
      ) as unknown as Wasm.SideEffectsResult

      return {
        sync_changes: wasmChangesToJs(result.sync_changes),
        aggregation_changes: wasmChangesToJs(result.aggregation_changes),
        computation_changes: wasmChangesToJs(result.computation_changes),
        registered_listener_ids: result.registered_listener_ids,
      }
    },

    unregisterSideEffects: (registrationId: string) => {
      if (destroyed) return
      wasm.unregister_side_effects(id, registrationId)
    },

    registerConcerns: (reg: Partial<Wasm.ConcernsRegistration>) => {
      const result = wasm.register_concerns(
        id,
        JSON.stringify(reg),
      ) as unknown as Wasm.ConcernsResult

      return {
        bool_logic_changes: wasmChangesToJs(result.bool_logic_changes),
        registered_logic_ids: result.registered_logic_ids,
        registered_validator_ids: result.registered_validator_ids,
        value_logic_changes: wasmChangesToJs(result.value_logic_changes),
        registered_value_logic_ids: result.registered_value_logic_ids,
      }
    },

    unregisterConcerns: (registrationId: string) => {
      if (destroyed) return
      wasm.unregister_concerns(id, registrationId)
    },

    registerBoolLogic: (outputPath: string, tree: unknown) =>
      wasm.register_boollogic(id, outputPath, JSON.stringify(tree)),

    unregisterBoolLogic: (logicId: number) => {
      if (destroyed) return
      wasm.unregister_boollogic(id, logicId)
    },

    pipelineReset: () => {
      wasm.pipeline_reset(id)
    },

    destroy: () => {
      if (destroyed) return
      destroyed = true
      wasm.pipeline_destroy(id)
      schemas.clear()
    },

    getGraphSnapshot: () => {
      if (destroyed) return { pipelines: {} } as unknown as Wasm.GraphSnapshot
      return wasm.get_graph_snapshot(id) as unknown as Wasm.GraphSnapshot
    },

    validatorSchemas: schemas,
  }
}

export type WasmPipeline = ReturnType<typeof createWasmPipeline>
export type { Wasm }
