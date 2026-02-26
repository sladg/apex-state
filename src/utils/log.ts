/**
 * Apex State Logger — Simplified debug logging.
 *
 * Two log functions:
 * 1. logPipeline — called once per processChanges with trace + listener data
 * 2. logRegistration — called once per register/unregister with graph snapshot
 *
 * Zero runtime cost when log flag is false (returns no-op logger).
 */

import type { DebugConfig } from '../core/types'
import type { Change, Wasm } from '../wasm/bridge'

// ---------------------------------------------------------------------------
// Logger types
// ---------------------------------------------------------------------------

export interface ListenerLogEntry {
  subscriberId: number
  fnName: string
  scope: string
  input: [string, unknown, unknown][]
  output: Change[]
  durationMs: number
  slow: boolean
}

export interface PipelineLogData {
  initialChanges: Change[]
  trace: Wasm.PipelineTrace | null
  listenerLog: ListenerLogEntry[]
  durationMs: number
  /** All state + concern changes passed to applyBatch (early + late). */
  appliedChanges?: Change[]
  /** Frozen valtio snapshot of state after all changes applied. */
  stateSnapshot?: unknown
}

/** Maps SideEffectsResult field names to trace stage names. */
const RESULT_TO_STAGE: Record<string, string> = {
  sync_changes: 'sync',
  aggregation_changes: 'aggregation_read',
  computation_changes: 'computation',
}

/** Extract { stageName: Change[] } from a SideEffectsResult-shaped object. */
const extractProducedValues = (
  result: Record<string, unknown>,
): Record<string, Change[]> => {
  const out: Record<string, Change[]> = {}
  for (const [field, stage] of Object.entries(RESULT_TO_STAGE)) {
    const arr = result[field]
    if (Array.isArray(arr) && arr.length > 0) {
      out[stage] = arr as Change[]
    }
  }
  return out
}

export interface ApexLogger {
  logPipeline: (data: PipelineLogData) => void
  logRegistration: (
    type: 'register' | 'unregister',
    id: string,
    snapshot: Wasm.GraphSnapshot,
    trace?: Wasm.PipelineTrace,
    /** Registration result — changes are shown as { path: value } in produced. */
    result?: Record<string, unknown>,
  ) => void
  destroy: () => void
}

// ---------------------------------------------------------------------------
// No-op singleton (zero overhead when log is false)
// ---------------------------------------------------------------------------

const noop = () => {
  // no-op
}

const NOOP_LOGGER: ApexLogger = {
  logPipeline: noop,
  logRegistration: noop,
  destroy: noop,
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const PREFIX = 'apex-state'

/** Format a change list as { path: value } for compact display. */
const formatChanges = (changes: Change[]): Record<string, unknown> | string => {
  if (changes.length === 0) return '(none)'
  const obj: Record<string, unknown> = {}
  for (const c of changes) {
    obj[c.path] = c.value
  }
  return obj
}

/** Build a short label from input change paths. */
const buildPathLabel = (paths: string[]): string => {
  if (paths.length === 0) return '(empty)'
  if (paths.length <= 3) return paths.join(', ')
  return `${paths[0]} +${String(paths.length - 1)} more`
}

/** Add trace entries to a summary/tree object. @internal */
export const addTraceSummary = (
  obj: Record<string, unknown>,
  trace: Wasm.PipelineTrace,
  _producedValues?: Record<string, Change[]>,
): void => {
  if (trace.stages.length === 0) return
  const stagesSummary: Record<string, unknown> = {}
  for (const s of trace.stages) {
    const detail = {
      ...s,
      duration_ms: `${(s.duration_us / 1000).toFixed(2)}ms`,
    }
    // Show all stages (even empty ones) for full pipeline visibility
    stagesSummary[s.stage] = Object.keys(detail).length > 0 ? detail : '(idle)'
  }
  if (trace.stages.length > 0) {
    obj['stages'] = stagesSummary
  }
  if (trace.total_duration_us > 0) {
    obj['wasmDuration'] = `${(trace.total_duration_us / 1000).toFixed(2)}ms`
  }
}

/** Build console summary object for a pipeline run. @internal */
export const buildConsoleSummary = (
  data: PipelineLogData,
): Record<string, unknown> => {
  const summary: Record<string, unknown> = {
    input: formatChanges(data.initialChanges ?? []),
    duration: `${data.durationMs.toFixed(2)}ms`,
  }
  if (data.trace) addTraceSummary(summary, data.trace)
  for (const [i, entry] of data.listenerLog.entries()) {
    const name = entry.fnName || '(anonymous)'
    const dur = entry.durationMs > 0 ? ` ${entry.durationMs.toFixed(2)}ms` : ''
    const slow = entry.slow ? ' [SLOW]' : ''
    const key = `[${String(i).padStart(2, '0')}] listener:${String(entry.subscriberId)} ${name}${dur}${slow}`
    summary[key] = {
      scope: entry.scope || '(root)',
      input: entry.input,
      output: formatChanges(entry.output),
    }
  }
  return {
    ...summary,
    applied: formatChanges(data.appliedChanges ?? []),
    stateAfterChanges: data.stateSnapshot,
  }
}

// ---------------------------------------------------------------------------
// Factory
// ---------------------------------------------------------------------------

/**
 * Create a logger for the store.
 * Returns no-op when log flag is disabled (zero overhead).
 * Pure console logging only — DevTools is handled separately.
 */
export const createLogger = (config: DebugConfig): ApexLogger => {
  const { log = false } = config

  if (!log) return NOOP_LOGGER

  return {
    logPipeline: (data) => {
      const label = buildPathLabel(data.initialChanges.map((c) => c.path))
      const summary = buildConsoleSummary(data)
      console.groupCollapsed(`${PREFIX}:pipeline | ${label}`)
      console.log(summary)
      console.groupEnd()
    },

    logRegistration: (type, id, snapshot, trace, result) => {
      const action = type === 'register' ? 'add' : 'remove'
      const summary: Record<string, unknown> = { snapshot }

      if (trace) {
        const values = result ? extractProducedValues(result) : undefined
        addTraceSummary(summary, trace, values)
      }

      console.groupCollapsed(`${PREFIX}:registration | ${action} ${id}`)
      console.log(summary)
      console.groupEnd()
    },

    destroy: noop,
  }
}
