/**
 * Apex State Logger — Simplified debug logging with colored console output.
 *
 * Two log functions:
 * 1. logPipeline — called once per processChanges with unified trace
 * 2. logRegistration — called once per register/unregister with graph snapshot
 *
 * Zero runtime cost when log flag is false (returns no-op logger).
 */

import type { DebugConfig } from '../core/types'
import type { Change, SideEffectsResult, Wasm } from '../wasm/bridge'

// ---------------------------------------------------------------------------
// Unified trace types
// ---------------------------------------------------------------------------

/** Per-listener dispatch trace (JS-measured). */
export interface ListenerDispatchTrace {
  dispatchId: number
  subscriberId: number
  fnName: string
  scope: string
  topic: string
  registrationId: string
  input: [string, unknown, unknown][]
  output: Change[]
  currentState: unknown
  durationMs: number
  slow: boolean
}

/** Universal trace — single object for console, devtools, any observability tool. */
export interface UnifiedPipelineTrace {
  wasm: Wasm.PipelineTrace
  listeners: ListenerDispatchTrace[]
  totalDurationMs: number
  wasmDurationMs: number
  listenerDurationMs: number
}

// ---------------------------------------------------------------------------
// Logger types
// ---------------------------------------------------------------------------

/** @deprecated Use ListenerDispatchTrace instead. Kept for backward compatibility. */
export interface ListenerLogEntry {
  subscriberId: number
  fnName: string
  scope: string
  input: [string, unknown, unknown][]
  output: Change[]
  currentState: unknown
  durationMs: number
  slow: boolean
}

export interface PipelineLogData {
  initialChanges: Change[]
  trace: UnifiedPipelineTrace | null
  /** All state + concern changes passed to applyBatch (early + late). */
  appliedChanges?: Change[]
  /** Frozen valtio snapshot of state after all changes applied. */
  stateSnapshot?: unknown
}

export interface RegistrationLogData {
  result?: SideEffectsResult
  appliedChanges?: Change[]
  stateSnapshot?: unknown
  durationMs?: number
}

export interface ApexLogger {
  logPipeline: (data: PipelineLogData) => void
  logRegistration: (
    type: 'register' | 'unregister',
    id: string,
    snapshot: Wasm.GraphSnapshot,
    data?: RegistrationLogData,
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
// Color scheme
// ---------------------------------------------------------------------------

const COLORS = {
  input: '#4A90D9',
  transform: '#7B68EE',
  diff: '#5B9BD5',
  graph: '#50C878',
  logic: '#9370DB',
  listener: '#E67E22',
  slow: '#E74C3C',
  skipped: '#999',
  produced: '#2C3E50',
  timing: '#666',
  label: '#888',
} as const

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const PREFIX = 'apex-state'

/** Strip valtio Proxy wrappers from a value for safe logging. */
const unwrap = (value: unknown): unknown => {
  try {
    return JSON.parse(JSON.stringify(value))
  } catch {
    return value
  }
}

/** Build a short label from input change paths. */
const buildPathLabel = (paths: string[]): string => {
  if (paths.length === 0) return '(empty)'
  if (paths.length <= 3) return paths.join(', ')
  return `${paths[0]} +${String(paths.length - 1)} more`
}

/** Format ms value with 2 decimals. */
const fmtMs = (ms: number): string => `${ms.toFixed(2)}ms`

// ---------------------------------------------------------------------------
// Console rendering — single log line per pipeline run
// ---------------------------------------------------------------------------

/** Build a detail object for expanding inside a single console.log. */
const buildDetail = (data: PipelineLogData): Record<string, unknown> => {
  const { trace } = data
  if (!trace) return { inputChanges: data.initialChanges }

  const displayStages = trace.wasm.stages.filter((s) => s.stage !== 'input')
  const stages = Object.fromEntries(
    displayStages.map((s, i) => [
      `[${String(i).padStart(2, '0')}] ${s.stage}`,
      {
        ms: fmtMs(s.duration_us / 1000),
        matched: s.matched,
        ...(s.produced.length > 0 ? { produced: s.produced } : {}),
        ...(s.skipped.length > 0 ? { skipped: s.skipped } : {}),
      },
    ]),
  )

  const listeners =
    trace.listeners.length > 0
      ? Object.fromEntries(
          trace.listeners.map((e) => [
            `${e.fnName || '(anonymous)'}  scope:${e.scope}${e.registrationId ? '  [reg: ' + e.registrationId + ']' : ''}`,
            {
              ms: fmtMs(e.durationMs),
              input: [e.input, unwrap(e.currentState)],
              ...(e.output.length > 0
                ? {
                    output: e.output.map((c) => [
                      c.path,
                      unwrap(c.value),
                      ...(c.meta ? [unwrap(c.meta)] : []),
                    ]),
                  }
                : {}),
              ...(e.slow ? { slow: true } : {}),
            },
          ]),
        )
      : undefined

  const anchors =
    trace.wasm.anchor_states && Object.keys(trace.wasm.anchor_states).length > 0
      ? trace.wasm.anchor_states
      : undefined

  return {
    inputChanges: unwrap(data.initialChanges),
    stages,
    ...(listeners ? { listeners } : {}),
    ...(data.appliedChanges && data.appliedChanges.length > 0
      ? {
          finalChanges: data.appliedChanges.map((c) => [
            c.path,
            unwrap(c.value),
            ...(c.meta ? [unwrap(c.meta)] : []),
          ]),
        }
      : {}),
    ...(data.stateSnapshot !== undefined
      ? { finalState: unwrap(data.stateSnapshot) }
      : {}),
    ...(anchors ? { anchors } : {}),
    timing: `wasm: ${fmtMs(trace.wasmDurationMs)} | listeners: ${fmtMs(trace.listenerDurationMs)} | total: ${fmtMs(trace.totalDurationMs)}`,
  }
}

/** Render a unified pipeline trace as a single groupCollapsed with flat console.log lines inside. */
const renderTrace = (data: PipelineLogData): void => {
  const pathLabel = buildPathLabel(data.initialChanges.map((c) => c.path))
  const totalMs = data.trace ? fmtMs(data.trace.totalDurationMs) : '0.00ms'

  console.groupCollapsed(
    `%c${PREFIX}:pipeline%c | %c${pathLabel}%c  ${totalMs}`,
    `color:${COLORS.label};font-weight:bold`,
    'color:inherit',
    'color:inherit;font-weight:bold',
    `color:${COLORS.timing}`,
  )

  const detail = buildDetail(data)
  const keyColor: Record<string, string> = {
    inputChanges: COLORS.input,
    stages: COLORS.label,
    listeners: COLORS.listener,
    finalChanges: COLORS.produced,
    finalState: COLORS.graph,
    anchors: COLORS.logic,
    timing: COLORS.timing,
  }
  for (const [key, value] of Object.entries(detail)) {
    const color = keyColor[key] ?? COLORS.label
    console.log(`%c${key}`, `color:${color};font-weight:bold`, value)
  }

  console.groupEnd()
}

// ---------------------------------------------------------------------------
// Legacy helpers (kept as @internal exports for backward compatibility in tests)
// ---------------------------------------------------------------------------

/** Format a change list as { path: value } for compact display. @internal */
const formatChanges = (changes: Change[]): Record<string, unknown> | string =>
  changes.length === 0
    ? '(none)'
    : Object.fromEntries(changes.map((c) => [c.path, c.value]))

/** Format initial changes as [path, value] tuples. @internal */
const formatInput = (changes: Change[]): [string, unknown][] | string =>
  changes.length === 0 ? '(none)' : changes.map((c) => [c.path, c.value])

/** Build trace summary entries from a pipeline trace, with listeners nested. @internal */
export const buildTraceSummary = (
  trace: Wasm.PipelineTrace,
  listeners: ListenerDispatchTrace[] = [],
): Record<string, unknown> => {
  // Skip 'input' stage — redundant with top-level input field
  const displayStages = trace.stages.filter((s) => s.stage !== 'input')
  if (displayStages.length === 0) return {}

  const stages = Object.fromEntries(
    displayStages.map((s, i) => {
      const prefix = `[${String(i).padStart(2, '0')}]`
      const detail: Record<string, unknown> = {
        ...s,
        duration_ms: `${(s.duration_us / 1000).toFixed(2)}ms`,
      }
      // Nest listener runs inside the listeners stage
      if (s.stage === 'listeners' && listeners.length > 0) {
        detail['runs'] = Object.fromEntries(
          listeners.map((entry, li) => {
            const name = entry.fnName || '(anonymous)'
            const dur =
              entry.durationMs > 0 ? ` ${entry.durationMs.toFixed(2)}ms` : ''
            const slow = entry.slow ? ' [SLOW]' : ''
            const key = `[${String(li).padStart(2, '0')}] listener:${String(entry.subscriberId)} ${name}${dur}${slow}`
            return [
              key,
              {
                scope: entry.scope || '(root)',
                input: entry.input,
                output: formatChanges(entry.output),
              },
            ]
          }),
        )
      }
      return [`${prefix} ${s.stage}`, detail]
    }),
  )

  return {
    ...(displayStages.length > 0 ? { stages } : {}),
    ...(trace.total_duration_us > 0
      ? { wasmDuration: `${(trace.total_duration_us / 1000).toFixed(2)}ms` }
      : {}),
  }
}

/** Build console summary object for a pipeline run. @internal */
export const buildConsoleSummary = (
  data: PipelineLogData,
): Record<string, unknown> => ({
  input: formatInput(data.initialChanges ?? []),
  duration: data.trace
    ? `${data.trace.totalDurationMs.toFixed(2)}ms`
    : '0.00ms',
  ...(data.trace?.wasm
    ? buildTraceSummary(data.trace.wasm, data.trace.listeners)
    : {}),
  applied: formatChanges(data.appliedChanges ?? []),
  stateAfterChanges: data.stateSnapshot,
})

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
      renderTrace(data)
    },

    logRegistration: (type, id, graphSnapshot, data) => {
      const action = type === 'register' ? 'add' : 'remove'
      const actionColor = type === 'register' ? COLORS.graph : COLORS.slow
      const timingLabel =
        data?.durationMs != null ? `  ${fmtMs(data.durationMs)}` : ''

      console.groupCollapsed(
        `%c${PREFIX}:registration%c | %c${action}%c ${id}%c${timingLabel}`,
        `color:${COLORS.label};font-weight:bold`,
        'color:inherit',
        `color:${actionColor};font-weight:bold`,
        'color:inherit;font-weight:bold',
        `color:${COLORS.timing}`,
      )

      // Graph snapshot — split into labeled sections
      const gs = graphSnapshot
      const graphEntries: [string, string, unknown][] = [
        ['syncPairs', COLORS.graph, gs.sync_pairs],
        ['flipPairs', COLORS.graph, gs.flip_pairs],
        ['listeners', COLORS.listener, gs.listeners],
        ['boolLogics', COLORS.logic, gs.bool_logics],
        ['valueLogics', COLORS.logic, gs.value_logics],
        ['aggregations', COLORS.transform, gs.aggregations],
        ['computations', COLORS.transform, gs.computations],
      ]
      for (const [label, color, value] of graphEntries) {
        const arr = value as unknown[]
        if (arr.length > 0) {
          console.log(`%c${label}`, `color:${color};font-weight:bold`, arr)
        }
      }

      // Side-effect changes from registration
      const result = data?.result
      if (result) {
        if (result.sync_changes.length > 0)
          console.log(
            '%csyncChanges',
            `color:${COLORS.graph};font-weight:bold`,
            result.sync_changes,
          )
        if (result.aggregation_changes.length > 0)
          console.log(
            '%caggregationChanges',
            `color:${COLORS.transform};font-weight:bold`,
            result.aggregation_changes,
          )
        if (result.computation_changes.length > 0)
          console.log(
            '%ccomputationChanges',
            `color:${COLORS.transform};font-weight:bold`,
            result.computation_changes,
          )
      }

      // Final changes + state
      if (data?.appliedChanges && data.appliedChanges.length > 0) {
        console.log(
          '%cfinalChanges',
          `color:${COLORS.produced};font-weight:bold`,
          data.appliedChanges.map((c) => [
            c.path,
            unwrap(c.value),
            ...(c.meta ? [unwrap(c.meta)] : []),
          ]),
        )
      }
      if (data?.stateSnapshot !== undefined) {
        console.log(
          '%cfinalState',
          `color:${COLORS.graph};font-weight:bold`,
          unwrap(data.stateSnapshot),
        )
      }

      // Timing
      if (data?.durationMs != null) {
        console.log(
          `%ctiming: ${fmtMs(data.durationMs)}`,
          `color:${COLORS.timing}`,
        )
      }

      console.groupEnd()
    },

    destroy: noop,
  }
}
