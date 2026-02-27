/**
 * Logging — Pipeline trace display and buildConsoleSummary output
 *
 * Tests the logger display functions directly with synthetic trace data.
 * This validates formatting logic independent of WASM trace population.
 */

import { afterEach, describe, expect, it, vi } from 'vitest'

import { pairs } from '../../src/types/pairs'
import type {
  ListenerDispatchTrace,
  PipelineLogData,
  UnifiedPipelineTrace,
} from '../../src/utils/log'
import {
  buildConsoleSummary,
  buildTraceSummary,
  createLogger,
} from '../../src/utils/log'
import {
  createWasmPipeline,
  type Wasm,
  type WasmPipeline,
} from '../../src/wasm/bridge'

// eslint-disable-next-line @typescript-eslint/no-empty-function
const noop = () => {}

// ---------------------------------------------------------------------------
// Helpers — synthetic data builders
// ---------------------------------------------------------------------------

const makeProduced = (
  path: string,
  value: string,
  registration_id: string | null = null,
  source_path: string | null = null,
): Wasm.StageTrace['produced'][0] => ({
  path,
  value,
  registration_id,
  source_path,
})

const makeSkipped = (
  overrides: Partial<Wasm.StageTrace['skipped'][0]> = {},
): Wasm.StageTrace['skipped'][0] => ({
  path: 'unknown',
  kind: 'real',
  reason: 'wrong_kind',
  detail: '',
  registration_id: null,
  anchor_path: null,
  ...overrides,
})

const makeStage = (
  overrides: Partial<Wasm.StageTrace> = {},
): Wasm.StageTrace => ({
  stage: 'diff',
  duration_us: 0,
  matched: [],
  skipped: [],
  produced: [],
  followup: [],
  ...overrides,
})

const makeWasmTrace = (
  stages: Wasm.StageTrace[],
  total_duration_us = 0,
): Wasm.PipelineTrace => ({
  total_duration_us,
  stages,
  anchor_states: {},
})

const makeListenerDispatch = (
  overrides: Partial<ListenerDispatchTrace> = {},
): ListenerDispatchTrace => ({
  dispatchId: 0,
  subscriberId: 0,
  fnName: '',
  scope: '',
  topic: '',
  registrationId: '',
  input: [],
  output: [],
  currentState: undefined,
  durationMs: 0,
  slow: false,
  ...overrides,
})

const makeUnifiedTrace = (
  wasmTrace: Wasm.PipelineTrace,
  listeners: ListenerDispatchTrace[] = [],
  totalDurationMs = 1.5,
): UnifiedPipelineTrace => ({
  wasm: wasmTrace,
  listeners,
  totalDurationMs,
  wasmDurationMs: wasmTrace.total_duration_us / 1000,
  listenerDurationMs: listeners.reduce((sum, e) => sum + e.durationMs, 0),
})

/** Find a stage entry in the prefixed stages object by stage name. */
const findStage = (
  stages: Record<string, unknown>,
  name: string,
): Record<string, unknown> | undefined => {
  const key = Object.keys(stages).find((k) => k.endsWith(` ${name}`))
  return key ? (stages[key] as Record<string, unknown>) : undefined
}

const makePipelineData = (
  overrides: Partial<PipelineLogData> = {},
): PipelineLogData => ({
  initialChanges: [{ path: 'user.name', value: 'Alice' }],
  trace: null,
  ...overrides,
})

// ---------------------------------------------------------------------------
// buildConsoleSummary — no trace
// ---------------------------------------------------------------------------

describe('buildConsoleSummary', () => {
  describe('without trace', () => {
    it('should include input as [path, value] tuples and duration', () => {
      // Basic summary with input changes as tuples and total duration
      const summary = buildConsoleSummary(makePipelineData())
      expect(summary['input']).toEqual([['user.name', 'Alice']])
      expect(summary['duration']).toBe('0.00ms')
    })

    it('should not have stages key when trace is null', () => {
      // No trace means no stages in output
      const summary = buildConsoleSummary(makePipelineData())
      expect(summary['stages']).toBeUndefined()
      expect(summary['wasmDuration']).toBeUndefined()
    })

    it('should show "(none)" for empty input', () => {
      // Empty input array formatted as "(none)"
      const summary = buildConsoleSummary(
        makePipelineData({ initialChanges: [] }),
      )
      expect(summary['input']).toBe('(none)')
    })
  })

  // ---------------------------------------------------------------------------
  // buildConsoleSummary — with trace
  // ---------------------------------------------------------------------------

  describe('with trace', () => {
    it('should include stages from trace with prefixed keys', () => {
      // Trace with active stages shows stage details keyed by [NN] stageName
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'diff',
          matched: [['user.name', '']],
          produced: [makeProduced('user.name', 'Alice')],
        }),
        makeStage({
          stage: 'sync',
          matched: [['user.name', '']],
          produced: [makeProduced('profile.name', 'Bob')],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages).toBeDefined()
      expect(findStage(stages, 'diff')).toEqual(
        expect.objectContaining({
          matched: [['user.name', '']],
          produced: [
            expect.objectContaining({ path: 'user.name', value: 'Alice' }),
          ],
        }),
      )
      expect(findStage(stages, 'sync')).toEqual(
        expect.objectContaining({
          matched: [['user.name', '']],
          produced: [
            expect.objectContaining({ path: 'profile.name', value: 'Bob' }),
          ],
        }),
      )
    })

    it('should include empty stages with raw data', () => {
      // Stages with no activity still appear with all fields (spread)
      const wasmTrace = makeWasmTrace([
        makeStage({ stage: 'diff', matched: [['x', '']] }),
        makeStage({ stage: 'flip' }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(findStage(stages, 'flip')).toEqual(
        expect.objectContaining({
          stage: 'flip',
          matched: [],
          produced: [],
        }),
      )
    })

    it('should include wasmDuration when total_duration_us > 0', () => {
      // Total WASM duration formatted in ms
      const wasmTrace = makeWasmTrace(
        [makeStage({ stage: 'diff', matched: [['x', '']] })],
        2500,
      )
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['wasmDuration']).toBe('2.50ms')
    })

    it('should omit wasmDuration when total_duration_us is 0', () => {
      // Zero total duration not shown
      const wasmTrace = makeWasmTrace(
        [makeStage({ stage: 'diff', matched: [['x', '']] })],
        0,
      )
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['wasmDuration']).toBeUndefined()
    })

    it('should handle empty stages array', () => {
      // Trace with no stages — no stages key added
      const wasmTrace = makeWasmTrace([])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['stages']).toBeUndefined()
    })

    it('should include stages with only skipped entries', () => {
      // Stage with only skipped changes should still appear with raw skipped data
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'sync',
          skipped: [
            makeSkipped({
              path: 'user.name',
              kind: 'redundant',
              reason: 'wrong_kind',
            }),
          ],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(findStage(stages, 'sync')).toEqual(
        expect.objectContaining({
          skipped: [
            expect.objectContaining({
              path: 'user.name',
              kind: 'redundant',
              reason: 'wrong_kind',
            }),
          ],
        }),
      )
    })
  })

  // ---------------------------------------------------------------------------
  // buildConsoleSummary — listener entries
  // ---------------------------------------------------------------------------

  describe('listener entries', () => {
    /** Helper: extract listener runs from summary stages. */
    const getListenerRuns = (
      summary: Record<string, unknown>,
    ): Record<string, unknown> => {
      const stages = summary['stages'] as Record<string, unknown>
      const listenersStage = findStage(stages, 'listeners') as Record<
        string,
        unknown
      >
      return listenersStage['runs'] as Record<string, unknown>
    }

    it('should format listener entries with scope/input/output', () => {
      // Listener runs are nested inside the "listeners" stage's runs key
      const wasmTrace = makeWasmTrace([
        makeStage({ stage: 'listeners', matched: [['user.name', '']] }),
      ])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 42,
          fnName: 'onNameChange',
          scope: 'user',
          input: [['user.name', 'Alice', 'Bob']],
          output: [{ path: 'user.greeting', value: 'Hello Bob' }],
          durationMs: 0.8,
        }),
      ])
      const data = makePipelineData({ trace })
      const summary = buildConsoleSummary(data)
      const runs = getListenerRuns(summary)
      const runKey = Object.keys(runs).find((k) => k.includes('listener:42'))
      expect(runKey).toBeDefined()
      expect(runKey).toContain('onNameChange')
      expect(runKey).toContain('0.80ms')

      const entry = runs[runKey!] as Record<string, unknown>
      expect(entry['scope']).toBe('user')
      expect(entry['input']).toEqual([['user.name', 'Alice', 'Bob']])
      expect(entry['output']).toEqual({ 'user.greeting': 'Hello Bob' })
    })

    it('should mark slow listeners with [SLOW]', () => {
      // Slow listener flagged in the key
      const wasmTrace = makeWasmTrace([makeStage({ stage: 'listeners' })])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 1,
          fnName: 'heavyComputation',
          durationMs: 50,
          slow: true,
        }),
      ])
      const data = makePipelineData({ trace })
      const summary = buildConsoleSummary(data)
      const runs = getListenerRuns(summary)
      const runKey = Object.keys(runs).find((k) => k.includes('listener:1'))
      expect(runKey).toContain('[SLOW]')
    })

    it('should use "(anonymous)" for unnamed listeners', () => {
      // Missing fnName defaults to "(anonymous)"
      const wasmTrace = makeWasmTrace([makeStage({ stage: 'listeners' })])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 5,
          fnName: '',
          scope: 'root',
        }),
      ])
      const data = makePipelineData({ trace })
      const summary = buildConsoleSummary(data)
      const runs = getListenerRuns(summary)
      const runKey = Object.keys(runs).find((k) => k.includes('listener:5'))
      expect(runKey).toContain('(anonymous)')
    })

    it('should use "(root)" for empty scope', () => {
      // Empty scope string formatted as "(root)"
      const wasmTrace = makeWasmTrace([makeStage({ stage: 'listeners' })])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 3,
          fnName: 'handler',
          scope: '',
        }),
      ])
      const data = makePipelineData({ trace })
      const summary = buildConsoleSummary(data)
      const runs = getListenerRuns(summary)
      const runKey = Object.keys(runs).find((k) => k.includes('listener:3'))
      const entry = runs[runKey!] as Record<string, unknown>
      expect(entry['scope']).toBe('(root)')
    })

    it('should order listener entries by index', () => {
      // Multiple listeners keyed with zero-padded index
      const wasmTrace = makeWasmTrace([makeStage({ stage: 'listeners' })])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({ subscriberId: 1, fnName: 'first' }),
        makeListenerDispatch({ subscriberId: 2, fnName: 'second' }),
      ])
      const data = makePipelineData({ trace })
      const summary = buildConsoleSummary(data)
      const runs = getListenerRuns(summary)
      const keys = Object.keys(runs).filter((k) => k.includes('listener:'))
      expect(keys[0]).toContain('[00]')
      expect(keys[1]).toContain('[01]')
    })
  })

  // ---------------------------------------------------------------------------
  // buildConsoleSummary — full pipeline trace (all stages)
  // ---------------------------------------------------------------------------

  describe('full pipeline trace', () => {
    /** All 12 stage names the WASM pipeline emits. */
    const ALL_STAGES: Wasm.StageTrace['stage'][] = [
      'input',
      'aggregation_write',
      'computation',
      'diff',
      'clear_path',
      'sync',
      'flip',
      'aggregation_read',
      'computation',
      'bool_logic',
      'value_logic',
      'listeners',
      'apply',
    ]

    /** Build a full pipeline trace with all 13 stage entries (computation appears twice). */
    const makeFullTrace = (): UnifiedPipelineTrace =>
      makeUnifiedTrace(
        makeWasmTrace(ALL_STAGES.map((stage) => makeStage({ stage }))),
      )

    it('should display all non-input stages', () => {
      // WASM emits all stages — buildTraceSummary filters out 'input', rest present with prefixed keys
      const trace = makeFullTrace()
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages).toBeDefined()

      // All unique stage names except 'input' should be present (input is filtered)
      for (const stageName of new Set(ALL_STAGES)) {
        if (stageName === 'input') continue
        expect(findStage(stages, stageName)).toBeDefined()
      }
    })

    it('should include raw stage data for empty stages', () => {
      // A full trace where all stages are empty should still include stage fields
      const trace = makeFullTrace()
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // All non-input stages are empty but still have raw fields
      for (const stageName of new Set(ALL_STAGES)) {
        if (stageName === 'input') continue
        const detail = findStage(stages, stageName) as Record<string, unknown>
        expect(detail).toEqual(
          expect.objectContaining({
            stage: stageName,
            matched: [],
            produced: [],
          }),
        )
      }
    })

    it('should show a mix of active and empty stages', () => {
      // Realistic trace: some stages have activity, others are empty
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'input',
          matched: [['user.name', '']],
        }),
        makeStage({ stage: 'aggregation_write' }),
        makeStage({ stage: 'computation' }),
        makeStage({
          stage: 'diff',
          matched: [['user.name', '']],
        }),
        makeStage({ stage: 'clear_path' }),
        makeStage({
          stage: 'sync',
          matched: [['user.name', '']],
          produced: [makeProduced('profile.name', 'value1')],
        }),
        makeStage({ stage: 'flip' }),
        makeStage({ stage: 'aggregation_read' }),
        makeStage({ stage: 'computation' }),
        makeStage({
          stage: 'bool_logic',
          matched: [['1', '']],
          produced: [makeProduced('_concerns.user.name.disabled', 'true')],
        }),
        makeStage({ stage: 'value_logic' }),
        makeStage({
          stage: 'listeners',
          matched: [
            ['user.name', ''],
            ['profile.name', ''],
          ],
        }),
        makeStage({
          stage: 'apply',
          matched: [['4 changes', '']],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // 'input' stage is filtered out by buildTraceSummary
      expect(findStage(stages, 'input')).toBeUndefined()

      // Active stages have detail with data (accessed via findStage with prefixed keys)
      expect(findStage(stages, 'sync')).toEqual(
        expect.objectContaining({
          matched: [['user.name', '']],
          produced: [
            expect.objectContaining({ path: 'profile.name', value: 'value1' }),
          ],
        }),
      )
      expect(findStage(stages, 'bool_logic')).toEqual(
        expect.objectContaining({
          matched: [['1', '']],
          produced: [
            expect.objectContaining({
              path: '_concerns.user.name.disabled',
              value: 'true',
            }),
          ],
        }),
      )

      // Empty stages still present with raw data
      expect(findStage(stages, 'aggregation_write')).toEqual(
        expect.objectContaining({ matched: [], produced: [] }),
      )
      expect(findStage(stages, 'flip')).toEqual(
        expect.objectContaining({ matched: [], produced: [] }),
      )
      expect(findStage(stages, 'value_logic')).toEqual(
        expect.objectContaining({ matched: [], produced: [] }),
      )
    })

    it('should handle duplicate computation stage (both get unique prefixed keys)', () => {
      // Computation appears twice — each gets its own [NN] prefixed key
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'computation',
          skipped: [
            makeSkipped({
              path: 'total',
              kind: 'real',
              reason: 'guard_failed',
            }),
          ],
        }),
        makeStage({
          stage: 'computation',
          matched: [
            ['price', ''],
            ['quantity', ''],
          ],
          produced: [makeProduced('total', '100')],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // Both computation entries are present with different prefixed keys
      const compKeys = Object.keys(stages).filter((k) =>
        k.includes('computation'),
      )
      expect(compKeys).toHaveLength(2)

      // Second computation entry has produced data
      const secondComp = stages[compKeys[1]!] as Record<string, unknown>
      expect(secondComp).toEqual(
        expect.objectContaining({
          matched: [
            ['price', ''],
            ['quantity', ''],
          ],
          produced: [expect.objectContaining({ path: 'total', value: '100' })],
        }),
      )
    })

    it('should show skipped entries in aggregation_read as raw data', () => {
      // AggregationRead can skip paths due to disabled anchors
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'aggregation_read',
          matched: [
            ['cart.item1.price', ''],
            ['cart.item2.price', ''],
          ],
          produced: [makeProduced('cart.total', '150')],
          skipped: [
            makeSkipped({
              path: 'cart.summary',
              kind: 'real',
              reason: 'guard_failed',
            }),
          ],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(findStage(stages, 'aggregation_read')).toEqual(
        expect.objectContaining({
          matched: [
            ['cart.item1.price', ''],
            ['cart.item2.price', ''],
          ],
          produced: [
            expect.objectContaining({ path: 'cart.total', value: '150' }),
          ],
          skipped: [
            expect.objectContaining({
              path: 'cart.summary',
              kind: 'real',
              reason: 'guard_failed',
            }),
          ],
        }),
      )
    })

    it('should show apply stage with change count', () => {
      // Apply stage shows the final change count as matched
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'apply',
          matched: [['7 changes', '']],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(findStage(stages, 'apply')).toEqual(
        expect.objectContaining({
          matched: [['7 changes', '']],
        }),
      )
    })
  })
})

// ---------------------------------------------------------------------------
// buildTraceSummary — direct tests
// ---------------------------------------------------------------------------

describe('buildTraceSummary', () => {
  it('should return stages key for non-empty trace', () => {
    // Verifies buildTraceSummary returns an object with stages (prefixed keys)
    const wasmTrace = makeWasmTrace([
      makeStage({ stage: 'diff', matched: [['user.name', '']] }),
    ])
    const result = buildTraceSummary(wasmTrace)
    expect(result['stages']).toBeDefined()
    const stages = result['stages'] as Record<string, unknown>
    expect(findStage(stages, 'diff')).toEqual(
      expect.objectContaining({ matched: [['user.name', '']] }),
    )
  })

  it('should return empty object for empty trace', () => {
    // Empty stages array → empty object
    const result = buildTraceSummary(makeWasmTrace([]))
    expect(result).toEqual({})
  })

  it('should include wasmDuration when total_duration_us > 0', () => {
    // Non-zero total duration includes wasmDuration key
    const result = buildTraceSummary(makeWasmTrace([makeStage()], 5000))
    expect(result['wasmDuration']).toBe('5.00ms')
  })

  it('should not include wasmDuration when total_duration_us is 0', () => {
    // Zero total duration → no wasmDuration key
    const result = buildTraceSummary(makeWasmTrace([makeStage()], 0))
    expect(result['wasmDuration']).toBeUndefined()
  })

  it('should show all non-input stages including empty ones', () => {
    // Even empty stages appear when included in the trace (input is filtered)
    const wasmTrace = makeWasmTrace([
      makeStage({ stage: 'input', matched: [['x', '']] }),
      makeStage({ stage: 'sync' }),
      makeStage({ stage: 'flip' }),
    ])
    const result = buildTraceSummary(wasmTrace)
    const stages = result['stages'] as Record<string, unknown>
    // 'input' stage is filtered out
    expect(findStage(stages, 'input')).toBeUndefined()
    expect(findStage(stages, 'sync')).toEqual(
      expect.objectContaining({ stage: 'sync', matched: [], produced: [] }),
    )
    expect(findStage(stages, 'flip')).toEqual(
      expect.objectContaining({ stage: 'flip', matched: [], produced: [] }),
    )
  })

  it('should compose with spread to preserve existing keys', () => {
    // buildTraceSummary result can be spread into an existing object
    const result: Record<string, unknown> = {
      existing: 'value',
      ...buildTraceSummary(makeWasmTrace([makeStage({ stage: 'diff' })])),
    }
    expect(result['existing']).toBe('value')
    expect(result['stages']).toBeDefined()
  })
})

// ---------------------------------------------------------------------------
// createLogger — actual console output verification
// ---------------------------------------------------------------------------

describe('createLogger — console output', () => {
  const spyGroupCollapsed = vi
    .spyOn(console, 'groupCollapsed')
    .mockImplementation(noop)
  const spyLog = vi.spyOn(console, 'log').mockImplementation(noop)
  const spyGroupEnd = vi.spyOn(console, 'groupEnd').mockImplementation(noop)

  afterEach(() => {
    spyGroupCollapsed.mockClear()
    spyLog.mockClear()
    spyGroupEnd.mockClear()
  })

  describe('logPipeline — no trace', () => {
    it('should emit groupCollapsed → log detail keys → groupEnd when trace is null', () => {
      // Pipeline log with no trace shows inputChanges detail key
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      // When no trace, buildDetail returns { inputChanges: [...] } → 1 log call
      expect(spyLog).toHaveBeenCalledTimes(1)
      const firstLogLabel = spyLog.mock.calls[0]![0] as string
      expect(firstLogLabel).toContain('inputChanges')
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)
    })

    it('should use "apex-state:pipeline" prefix with %c color codes', () => {
      // Group label uses CSS color codes for styled output
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toContain('%c')
      expect(label).toContain('apex-state:pipeline')
    })

    it('should include input paths in group label', () => {
      // Single input path shown in label
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toContain('user.name')
    })

    it('should truncate multiple paths in group label', () => {
      // More than 3 input paths get truncated with "+N more"
      const logger = createLogger({ log: true })
      logger.logPipeline(
        makePipelineData({
          initialChanges: [
            { path: 'a', value: 1 },
            { path: 'b', value: 2 },
            { path: 'c', value: 3 },
            { path: 'd', value: 4 },
            { path: 'e', value: 5 },
          ],
        }),
      )

      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toContain('a')
      expect(label).toContain('+4 more')
    })
  })

  describe('logPipeline — with trace (renderTrace)', () => {
    it('should render as single groupCollapsed with flat log lines', () => {
      // renderTrace uses one groupCollapsed + flat console.log per detail key
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([
        makeStage({ stage: 'diff', matched: [['x', '']] }),
      ])
      const trace = makeUnifiedTrace(wasmTrace, [], 1.5)

      logger.logPipeline(makePipelineData({ trace }))

      // Single groupCollapsed (the outer pipeline group)
      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      const outerLabel = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(outerLabel).toContain('apex-state:pipeline')

      // Flat log lines: inputChanges, stages, timing
      expect(spyLog.mock.calls.length).toBeGreaterThanOrEqual(2)

      // Single groupEnd
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)
    })

    it('should log stages as a single detail object', () => {
      // Stages are rendered as a single console.log with "stages" key
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([
        makeStage({ stage: 'diff', matched: [['user.name', '']] }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)

      logger.logPipeline(makePipelineData({ trace }))

      // Find the stages log call (label contains '%cstages')
      const stagesCall = spyLog.mock.calls.find(
        (c) => typeof c[0] === 'string' && (c[0] as string).includes('stages'),
      )
      expect(stagesCall).toBeDefined()
      // The value is the stages object
      const stagesObj = stagesCall![2] as Record<string, unknown>
      expect(stagesObj).toBeDefined()
    })

    it('should include produced and skipped in stage detail objects', () => {
      // Produced and skipped entries are included in the stages detail object
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([
        makeStage({
          stage: 'diff',
          skipped: [
            makeSkipped({
              path: 'user.name',
              reason: 'redundant',
              detail: 'redundant: value unchanged',
            }),
          ],
        }),
        makeStage({
          stage: 'sync',
          produced: [
            makeProduced('target', '"hello"', 'sideEffects-sync', 'source'),
          ],
        }),
      ])
      const trace = makeUnifiedTrace(wasmTrace)

      logger.logPipeline(makePipelineData({ trace }))

      // Find the stages log call
      const stagesCall = spyLog.mock.calls.find(
        (c) => typeof c[0] === 'string' && (c[0] as string).includes('stages'),
      )
      expect(stagesCall).toBeDefined()
      const stagesObj = stagesCall![2] as Record<string, unknown>

      // Diff stage has skipped
      const diffKey = Object.keys(stagesObj).find((k) => k.includes('diff'))
      expect(diffKey).toBeDefined()
      const diffDetail = stagesObj[diffKey!] as Record<string, unknown>
      expect(diffDetail['skipped']).toBeDefined()

      // Sync stage has produced
      const syncKey = Object.keys(stagesObj).find((k) => k.includes('sync'))
      expect(syncKey).toBeDefined()
      const syncDetail = stagesObj[syncKey!] as Record<string, unknown>
      expect(syncDetail['produced']).toBeDefined()
    })

    it('should include listeners in detail object', () => {
      // Listener dispatches appear as a "listeners" detail key
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 42,
          fnName: 'onNameChange',
          scope: 'user',
          topic: 'user.name',
          registrationId: 'sideEffects-test',
          durationMs: 0.8,
        }),
      ])

      logger.logPipeline(makePipelineData({ trace }))

      // Only 1 groupCollapsed (outer pipeline group, no nested listener groups)
      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)

      // Find the listeners log call
      const listenersCall = spyLog.mock.calls.find(
        (c) =>
          typeof c[0] === 'string' && (c[0] as string).includes('listeners'),
      )
      expect(listenersCall).toBeDefined()
      // Value contains listener keyed by name/scope/regId
      const listenersObj = listenersCall![2] as Record<string, unknown>
      const listenerKey = Object.keys(listenersObj).find((k) =>
        k.includes('onNameChange'),
      )
      expect(listenerKey).toBeDefined()
      expect(listenerKey).toContain('scope:user')
      expect(listenerKey).toContain('[reg: sideEffects-test]')
    })

    it('should include slow listener flag in listener key', () => {
      // Slow listeners get [SLOW] in the key but still within flat detail
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 1,
          fnName: 'slowHandler',
          durationMs: 50,
          slow: true,
        }),
      ])

      logger.logPipeline(makePipelineData({ trace }))

      const listenersCall = spyLog.mock.calls.find(
        (c) =>
          typeof c[0] === 'string' && (c[0] as string).includes('listeners'),
      )
      expect(listenersCall).toBeDefined()
      const listenersObj = listenersCall![2] as Record<string, unknown>
      const key = Object.keys(listenersObj).find((k) =>
        k.includes('slowHandler'),
      )
      expect(key).toContain('slow')
    })

    it('should include listener input and output in detail value', () => {
      // Listener input/output are included in the detail value, not separate log calls
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([])
      const trace = makeUnifiedTrace(wasmTrace, [
        makeListenerDispatch({
          subscriberId: 1,
          fnName: 'handler',
          input: [['name', 'Bob', {}]],
          output: [{ path: 'greeting', value: 'Hello' }],
        }),
      ])

      logger.logPipeline(makePipelineData({ trace }))

      const listenersCall = spyLog.mock.calls.find(
        (c) =>
          typeof c[0] === 'string' && (c[0] as string).includes('listeners'),
      )
      expect(listenersCall).toBeDefined()
      const listenersObj = listenersCall![2] as Record<string, unknown>
      const key = Object.keys(listenersObj).find((k) => k.includes('handler'))
      expect(key).toBeDefined()
      const entry = listenersObj[key!] as Record<string, unknown>
      expect(entry['input']).toBeDefined()
      expect(entry['output']).toBeDefined()
    })

    it('should log anchor states when present', () => {
      // Trace with anchor states logs them as a detail key
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([])
      wasmTrace.anchor_states = { 'items.0': true, 'user.profile': false }
      const trace = makeUnifiedTrace(wasmTrace)

      logger.logPipeline(makePipelineData({ trace }))

      const anchorCall = spyLog.mock.calls.find(
        (c) => typeof c[0] === 'string' && (c[0] as string).includes('anchors'),
      )
      expect(anchorCall).toBeDefined()
      expect(anchorCall![2]).toEqual({
        'items.0': true,
        'user.profile': false,
      })
    })

    it('should log timing summary at the end', () => {
      // Timing summary as last detail key with wasm, listeners, and total durations
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([], 1500)
      const trace = makeUnifiedTrace(
        wasmTrace,
        [makeListenerDispatch({ durationMs: 0.8 })],
        2.3,
      )

      logger.logPipeline(makePipelineData({ trace }))

      // Last log call is timing detail — the timing VALUE is in args[2]
      const lastLogCall = spyLog.mock.calls[spyLog.mock.calls.length - 1]!
      const timingLabel = lastLogCall[0] as string
      expect(timingLabel).toContain('timing')
      const timingValue = lastLogCall[2] as string
      expect(timingValue).toContain('wasm:')
      expect(timingValue).toContain('listeners:')
      expect(timingValue).toContain('total:')
    })

    it('should use color codes in detail key labels', () => {
      // Each detail key label includes %c for CSS color coding
      const logger = createLogger({ log: true })
      const wasmTrace = makeWasmTrace([makeStage({ stage: 'diff' })])
      const trace = makeUnifiedTrace(wasmTrace)

      logger.logPipeline(makePipelineData({ trace }))

      // All log calls use %c colored key labels
      for (const call of spyLog.mock.calls) {
        const label = call[0] as string
        expect(label).toContain('%c')
      }

      // CSS argument is the color style
      const cssArg = spyLog.mock.calls[0]![1] as string
      expect(cssArg).toContain('color:')
      expect(cssArg).toContain('font-weight:bold')
    })
  })

  describe('logRegistration', () => {
    it('should log registration with "add" prefix using groupCollapsed', () => {
      // Register event uses "add" action inside a groupCollapsed section
      const logger = createLogger({ log: true })
      const snapshot = {
        sync_pairs: [['user.name', 'profile.name'] as [string, string]],
        directed_sync_pairs: [],
        flip_pairs: [],
        listeners: [],
        bool_logics: [],
        value_logics: [],
        aggregations: [],
        computations: [],
      }
      logger.logRegistration('register', 'concern-123', snapshot)

      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toContain('apex-state:registration')
      expect(label).toContain('add')
      expect(label).toContain('concern-123')
      // Non-empty graph entry (syncPairs) is logged
      expect(spyLog).toHaveBeenCalled()
      const syncCall = spyLog.mock.calls.find(
        (c) =>
          typeof c[0] === 'string' && (c[0] as string).includes('syncPairs'),
      )
      expect(syncCall).toBeDefined()
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)
    })

    it('should display directed pair with → arrow and bidirectional pair with ↔ arrow', () => {
      // Directed pairs must show → (one-way), bidirectional pairs must show ↔
      // This verifies the log display correctly distinguishes directed vs bidirectional
      const logger = createLogger({ log: true })

      const snapshot = {
        sync_pairs: [['user.name', 'profile.name'] as [string, string]],
        directed_sync_pairs: [['source', 'target'] as [string, string]],
        flip_pairs: [],
        listeners: [],
        bool_logics: [],
        value_logics: [],
        aggregations: [],
        computations: [],
      }
      logger.logRegistration('register', 'test-arrows', snapshot)

      const syncCall = spyLog.mock.calls.find(
        (c) =>
          typeof c[0] === 'string' && (c[0] as string).includes('syncPairs'),
      )
      expect(syncCall).toBeDefined()

      const syncPairsValue = syncCall![2] as unknown[]

      // Find the directed entry — must contain → arrow
      const directedEntry = syncPairsValue.find(
        (e) =>
          Array.isArray(e) &&
          (e as unknown[])[0] === 'source' &&
          (e as unknown[])[2] === 'target',
      ) as unknown[] | undefined
      expect(directedEntry).toBeDefined()
      // Arrow must be → (one-way), not ↔ (bidirectional)
      expect(directedEntry![1]).toBe('→')
      expect(directedEntry![1]).not.toBe('↔')

      // Find the bidirectional entry — must contain ↔ arrow
      const biDirEntry = syncPairsValue.find(
        (e) =>
          Array.isArray(e) &&
          (e as unknown[])[0] === 'user.name' &&
          (e as unknown[])[2] === 'profile.name',
      ) as unknown[] | undefined
      expect(biDirEntry).toBeDefined()
      expect(biDirEntry![1]).toBe('↔')
      expect(biDirEntry![1]).not.toBe('→')
    })

    it('should log unregistration with "remove" prefix', () => {
      // Unregister event uses "remove" action inside a groupCollapsed section
      const logger = createLogger({ log: true })
      logger.logRegistration('unregister', 'concern-456', {
        sync_pairs: [],
        directed_sync_pairs: [],
        flip_pairs: [],
        listeners: [],
        bool_logics: [],
        value_logics: [],
        aggregations: [],
        computations: [],
      })

      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toContain('apex-state:registration')
      expect(label).toContain('remove')
      expect(label).toContain('concern-456')
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)
    })
  })

  describe('noop logger (log: false)', () => {
    it('should not emit any console calls', () => {
      // When log is false, no console output at all
      const logger = createLogger({ log: false })
      logger.logPipeline(makePipelineData())
      logger.logRegistration('register', 'test', {
        sync_pairs: [],
        directed_sync_pairs: [],
        flip_pairs: [],
        listeners: [],
        bool_logics: [],
        value_logics: [],
        aggregations: [],
        computations: [],
      })

      expect(spyGroupCollapsed).not.toHaveBeenCalled()
      expect(spyLog).not.toHaveBeenCalled()
      expect(spyGroupEnd).not.toHaveBeenCalled()
    })
  })
})

// ---------------------------------------------------------------------------
// E2E — WASM pipeline trace → logger display
// ---------------------------------------------------------------------------

describe('E2E: WASM pipeline trace → logger display', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    pipeline?.destroy()
  })

  /** Helper: processChanges + pipelineFinalize, returning trace from finalize. */
  const processAndFinalize = (
    p: WasmPipeline,
    changes: { path: string; value: unknown }[],
  ) => {
    p.processChanges(changes)
    return p.pipelineFinalize([])
  }

  it('should return populated trace when debug is enabled', () => {
    // Create pipeline with debug: true, process a change, verify trace is not null
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice', age: 25 } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Bob' },
    ])

    expect(result.trace).not.toBeNull()
    expect(result.trace!.stages.length).toBeGreaterThan(0)
  })

  it('should NOT return trace when debug is disabled', () => {
    // Default pipeline (no debug) should return trace: null
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ user: { name: 'Alice' } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Bob' },
    ])

    expect(result.trace).toBeFalsy()
  })

  it('should include all expected pipeline stages in trace', () => {
    // A simple field change should produce traces for the core stages
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice' } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Bob' },
    ])
    const stageNames = result.trace!.stages.map((s: Wasm.StageTrace) => s.stage)

    // At minimum these stages should be present for any change
    expect(stageNames).toContain('input')
    expect(stageNames).toContain('diff')
    expect(stageNames).toContain('apply')
  })

  it('should show matched paths in the input stage', () => {
    // The input stage should show which paths were matched after diff filtering
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice' } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Bob' },
    ])
    const inputStage = result.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'input',
    )

    expect(inputStage).toBeDefined()
    expect(inputStage!.matched.some(([p]) => p === 'user.name')).toBe(true)
  })

  it('should show skipped changes when value is unchanged', () => {
    // Sending same value should be filtered in input stage as skipped
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice', age: 25 } })

    // When all changes are redundant, processChanges returns early with no trace
    // So we test with a mix: one real change + one redundant
    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Alice' }, // redundant
      { path: 'user.age', value: 30 }, // real change
    ])

    expect(result.trace).not.toBeNull()
    const inputStage = result.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'input',
    )
    expect(inputStage).toBeDefined()
    // user.age matched, user.name skipped (redundant)
    expect(inputStage!.matched.some(([p]) => p === 'user.age')).toBe(true)
    expect(inputStage!.skipped.some((s) => s.path === 'user.name')).toBe(true)
  })

  it('should show sync stage producing synced paths with ProducedChange', () => {
    // Register sync pairs and verify the sync stage shows produced paths as ProducedChange objects
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ source: 'hello', target: '' })

    pipeline.registerSideEffects({
      registration_id: 'test-sync',
      sync_pairs: [['source', 'target']],
    })

    const result = processAndFinalize(pipeline, [
      { path: 'source', value: 'world' },
    ])

    expect(result.trace).not.toBeNull()
    const syncStage = result.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'sync',
    )
    expect(syncStage).toBeDefined()
    // ProducedChange has { path, value, registration_id, source_path }
    expect(syncStage!.produced).toContainEqual(
      expect.objectContaining({ path: 'target' }),
    )
  })

  it('should include enriched skipped change fields', () => {
    // Skipped changes should have detail, registration_id, anchor_path fields
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice', age: 25 } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Alice' }, // redundant
      { path: 'user.age', value: 30 }, // real change
    ])

    expect(result.trace).not.toBeNull()
    const inputStage = result.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'input',
    )
    const skipped = inputStage!.skipped.find((s) => s.path === 'user.name')
    expect(skipped).toBeDefined()
    // Enriched fields
    expect(skipped!.reason).toBe('redundant')
    expect(skipped!.detail).toBeDefined()
    expect(typeof skipped!.detail).toBe('string')
  })

  it('should flow trace through buildConsoleSummary correctly', () => {
    // End-to-end: WASM trace → wrap in UnifiedPipelineTrace → buildConsoleSummary → readable output
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ source: 'hello', target: '' })

    pipeline.registerSideEffects({
      registration_id: 'test-sync',
      sync_pairs: [['source', 'target']],
    })

    const result = processAndFinalize(pipeline, [
      { path: 'source', value: 'world' },
    ])

    // Wrap WASM trace in UnifiedPipelineTrace
    const trace: UnifiedPipelineTrace = {
      wasm: result.trace!,
      listeners: [],
      totalDurationMs: 1.0,
      wasmDurationMs: result.trace!.total_duration_us / 1000,
      listenerDurationMs: 0,
    }

    const logData: PipelineLogData = {
      initialChanges: [{ path: 'source', value: 'world' }],
      trace,
    }
    const summary = buildConsoleSummary(logData)

    // Summary should have stages from real WASM trace (input filtered out)
    expect(summary['stages']).toBeDefined()
    const stages = summary['stages'] as Record<string, unknown>

    // Input stage is filtered out by buildTraceSummary
    expect(findStage(stages, 'input')).toBeUndefined()

    // Sync stage should show produced target (accessed via prefixed key)
    const syncDetail = findStage(stages, 'sync') as Record<string, unknown>
    expect(syncDetail).toBeDefined()
    expect(syncDetail['produced']).toBeDefined()
  })

  it('should populate directed_sync_pairs in getGraphSnapshot when directed pair is registered', () => {
    // Directed (oneWay) pair must appear in directed_sync_pairs — NOT in sync_pairs
    // This is the field that logRegistration reads to display → arrows vs ↔ arrows
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ source: 'A', target: '' })

    pipeline.registerSideEffects({
      registration_id: 'test-directed',
      directed_sync_pairs: [['source', 'target']],
    })

    const snapshot = pipeline.getGraphSnapshot()

    // Directed pair must appear in directed_sync_pairs only
    expect(snapshot.directed_sync_pairs).toHaveLength(1)
    expect(snapshot.directed_sync_pairs[0]).toEqual(['source', 'target'])
    // Must NOT appear in sync_pairs (bidirectional)
    expect(snapshot.sync_pairs).toHaveLength(0)
  })

  it('should correctly classify oneWay syncPaths via syncToWasm into directed_sync_pairs in snapshot', () => {
    // Full path: syncPaths with {oneWay} → syncToWasm → directed_sync_pairs in snapshot
    // This is the actual user-facing path through registration.wasm-impl.ts
    pipeline = createWasmPipeline()
    pipeline.shadowInit({ 'path.to.somewhere': 'A', 'g.123.p.abc.data': '' })

    const { bidirectional, directed } = pairs.syncToWasm([
      ['path.to.somewhere', 'g.123.p.abc.data', { oneWay: '[0]->[1]' }],
    ])

    pipeline.registerSideEffects({
      registration_id: 'test-oneway',
      sync_pairs: bidirectional,
      directed_sync_pairs: directed,
    })

    const snapshot = pipeline.getGraphSnapshot()

    // The pair must appear in directed_sync_pairs (→), NOT sync_pairs (↔)
    expect(snapshot.directed_sync_pairs).toHaveLength(1)
    expect(snapshot.directed_sync_pairs[0]).toEqual([
      'path.to.somewhere',
      'g.123.p.abc.data',
    ])
    expect(snapshot.sync_pairs).toHaveLength(0)
  })

  it('should produce readable console output for createLogger with renderTrace', () => {
    // Full e2e: WASM → trace → createLogger → console spy
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ x: 1, y: 2 })

    const result = processAndFinalize(pipeline, [{ path: 'x', value: 10 }])

    const spyGC = vi.spyOn(console, 'groupCollapsed').mockImplementation(noop)
    const spyL = vi.spyOn(console, 'log').mockImplementation(noop)
    const spyGE = vi.spyOn(console, 'groupEnd').mockImplementation(noop)

    // Wrap WASM trace in UnifiedPipelineTrace
    const trace: UnifiedPipelineTrace = {
      wasm: result.trace!,
      listeners: [],
      totalDurationMs: 0.5,
      wasmDurationMs: result.trace!.total_duration_us / 1000,
      listenerDurationMs: 0,
    }

    const logger = createLogger({ log: true })
    logger.logPipeline({
      initialChanges: [{ path: 'x', value: 10 }],
      trace,
    })

    // Verify outer group label (single groupCollapsed in new flat format)
    expect(spyGC).toHaveBeenCalledTimes(1)
    const outerLabel = spyGC.mock.calls[0]![0] as string
    expect(outerLabel).toContain('apex-state:pipeline')
    expect(outerLabel).toContain('x')

    // Flat log lines should include at least inputChanges, stages, timing
    expect(spyL.mock.calls.length).toBeGreaterThanOrEqual(2)

    // Timing summary is the last log call — value is in args[2]
    const lastLog = spyL.mock.calls[spyL.mock.calls.length - 1]!
    const timingLabel = lastLog[0] as string
    expect(timingLabel).toContain('timing')
    const timingValue = lastLog[2] as string
    expect(timingValue).toContain('wasm:')
    expect(timingValue).toContain('total:')

    spyGC.mockRestore()
    spyL.mockRestore()
    spyGE.mockRestore()
  })
})
