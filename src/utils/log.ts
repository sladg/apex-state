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
  input: Change[]
  trace: Wasm.PipelineTrace | null
  listeners: ListenerLogEntry[]
  durationMs: number
}

export interface ApexLogger {
  logPipeline: (data: PipelineLogData) => void
  logRegistration: (
    type: 'register' | 'unregister',
    id: string,
    snapshot: Wasm.GraphSnapshot,
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

/** Add trace entries to a summary/tree object. */
const addTraceSummary = (
  obj: Record<string, unknown>,
  trace: Wasm.PipelineTrace,
): void => {
  if (trace.stages.length === 0) return
  const stagesSummary: Record<string, unknown> = {}
  for (const s of trace.stages) {
    if (s.accepted.length > 0 || s.produced.length > 0) {
      stagesSummary[s.stage] = {
        accepted: s.accepted.length,
        produced: s.produced.length,
        skipped: s.skipped.length,
        ...(s.duration_us > 0
          ? { duration: `${(s.duration_us / 1000).toFixed(2)}ms` }
          : {}),
      }
    }
  }
  if (Object.keys(stagesSummary).length > 0) {
    obj['stages'] = stagesSummary
  }
  if (trace.total_duration_us > 0) {
    obj['wasmDuration'] = `${(trace.total_duration_us / 1000).toFixed(2)}ms`
  }
}

/** Build console summary object for a pipeline run. */
const buildConsoleSummary = (
  data: PipelineLogData,
): Record<string, unknown> => {
  const summary: Record<string, unknown> = {
    input: formatChanges(data.input),
    duration: `${data.durationMs.toFixed(2)}ms`,
  }
  if (data.trace) addTraceSummary(summary, data.trace)
  for (const [i, entry] of data.listeners.entries()) {
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
  return summary
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
      const label = buildPathLabel(data.input.map((c) => c.path))
      const summary = buildConsoleSummary(data)
      console.groupCollapsed(`${PREFIX}:pipeline | ${label}`)
      console.log(summary)
      console.groupEnd()
    },

    logRegistration: (type, id, snapshot) => {
      const action = type === 'register' ? 'add' : 'remove'
      console.log(`${PREFIX}:registration | ${action} ${id}`, snapshot)
    },

    destroy: noop,
  }
}
