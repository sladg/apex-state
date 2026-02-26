/**
 * Logging — Pipeline trace display and buildConsoleSummary output
 *
 * Tests the logger display functions directly with synthetic trace data.
 * This validates formatting logic independent of WASM trace population.
 */

import { afterEach, describe, expect, it, vi } from 'vitest'

import type { PipelineLogData } from '../../src/utils/log'
import {
  addTraceSummary,
  buildConsoleSummary,
  createLogger,
  formatStageDetail,
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

const makeStage = (
  overrides: Partial<Wasm.StageTrace> = {},
): Wasm.StageTrace => ({
  stage: 'diff',
  duration_us: 0,
  accepted: [],
  skipped: [],
  produced: [],
  followup: [],
  ...overrides,
})

const makeTrace = (
  stages: Wasm.StageTrace[],
  total_duration_us = 0,
): Wasm.PipelineTrace => ({
  total_duration_us,
  stages,
})

const makePipelineData = (
  overrides: Partial<PipelineLogData> = {},
): PipelineLogData => ({
  input: [{ path: 'user.name', value: 'Alice' }],
  trace: null,
  listeners: [],
  durationMs: 1.5,
  ...overrides,
})

// ---------------------------------------------------------------------------
// formatStageDetail
// ---------------------------------------------------------------------------

describe('formatStageDetail', () => {
  it('should return empty object for idle stage', () => {
    // Stage with no activity at all
    const result = formatStageDetail(makeStage())
    expect(result).toEqual({})
  })

  it('should show accepted paths inline when ≤5', () => {
    // Small number of accepted paths shown as array
    const result = formatStageDetail(
      makeStage({ accepted: ['user.name', 'user.email', 'user.age'] }),
    )
    expect(result['accepted']).toEqual(['user.name', 'user.email', 'user.age'])
  })

  it('should truncate accepted paths when >5', () => {
    // Large number of accepted paths collapsed to count + first 3
    const paths = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    const result = formatStageDetail(makeStage({ accepted: paths }))
    expect(result['accepted']).toEqual({
      count: 7,
      paths: ['a', 'b', 'c', '+4 more'],
    })
  })

  it('should show produced paths inline when ≤5', () => {
    // Small number of produced paths shown as array
    const result = formatStageDetail(
      makeStage({ produced: ['_concerns.user.name.disabled'] }),
    )
    expect(result['produced']).toEqual(['_concerns.user.name.disabled'])
  })

  it('should truncate produced paths when >5', () => {
    // Large number of produced paths collapsed to count + first 3
    const paths = ['p1', 'p2', 'p3', 'p4', 'p5', 'p6']
    const result = formatStageDetail(makeStage({ produced: paths }))
    expect(result['produced']).toEqual({
      count: 6,
      paths: ['p1', 'p2', 'p3', '+3 more'],
    })
  })

  it('should show skipped changes with path and reason', () => {
    // Skipped entries formatted as "path (reason)"
    const result = formatStageDetail(
      makeStage({
        skipped: [
          { path: 'user.name', kind: 'redundant', reason: 'wrong_kind' },
          { path: 'cart.total', kind: 'breakdown', reason: 'guard_failed' },
        ],
      }),
    )
    expect(result['skipped']).toEqual([
      'user.name (wrong_kind)',
      'cart.total (guard_failed)',
    ])
  })

  it('should show duration when > 0', () => {
    // duration_us converted to ms with 2 decimals
    const result = formatStageDetail(makeStage({ duration_us: 1500 }))
    expect(result['duration']).toBe('1.50ms')
  })

  it('should omit duration when 0', () => {
    // Zero duration not shown
    const result = formatStageDetail(
      makeStage({ accepted: ['x'], duration_us: 0 }),
    )
    expect(result['duration']).toBeUndefined()
  })

  it('should show exactly 5 paths inline (boundary)', () => {
    // Boundary: exactly 5 paths should be inline, not truncated
    const paths = ['a', 'b', 'c', 'd', 'e']
    const result = formatStageDetail(makeStage({ accepted: paths }))
    expect(result['accepted']).toEqual(['a', 'b', 'c', 'd', 'e'])
  })

  it('should truncate at exactly 6 paths (boundary)', () => {
    // Boundary: 6 paths triggers truncation
    const paths = ['a', 'b', 'c', 'd', 'e', 'f']
    const result = formatStageDetail(makeStage({ accepted: paths }))
    expect(result['accepted']).toEqual({
      count: 6,
      paths: ['a', 'b', 'c', '+3 more'],
    })
  })
})

// ---------------------------------------------------------------------------
// buildConsoleSummary — no trace
// ---------------------------------------------------------------------------

describe('buildConsoleSummary', () => {
  describe('without trace', () => {
    it('should include input and duration', () => {
      // Basic summary with input changes and total duration
      const summary = buildConsoleSummary(makePipelineData())
      expect(summary['input']).toEqual({ 'user.name': 'Alice' })
      expect(summary['duration']).toBe('1.50ms')
    })

    it('should not have stages key when trace is null', () => {
      // No trace means no stages in output
      const summary = buildConsoleSummary(makePipelineData())
      expect(summary['stages']).toBeUndefined()
      expect(summary['wasmDuration']).toBeUndefined()
    })

    it('should show "(none)" for empty input', () => {
      // Empty input array formatted as "(none)"
      const summary = buildConsoleSummary(makePipelineData({ input: [] }))
      expect(summary['input']).toBe('(none)')
    })
  })

  // ---------------------------------------------------------------------------
  // buildConsoleSummary — with trace
  // ---------------------------------------------------------------------------

  describe('with trace', () => {
    it('should include stages from trace', () => {
      // Trace with active stages shows stage details
      const trace = makeTrace([
        makeStage({
          stage: 'diff',
          accepted: ['user.name'],
          produced: ['user.name'],
        }),
        makeStage({
          stage: 'sync',
          accepted: ['user.name'],
          produced: ['profile.name'],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages).toBeDefined()
      expect(stages['diff']).toEqual({
        accepted: ['user.name'],
        produced: ['user.name'],
      })
      expect(stages['sync']).toEqual({
        accepted: ['user.name'],
        produced: ['profile.name'],
      })
    })

    it('should show idle stages as "(idle)"', () => {
      // Stages with no activity shown as "(idle)" for pipeline visibility
      const trace = makeTrace([
        makeStage({ stage: 'diff', accepted: ['x'] }),
        makeStage({ stage: 'flip' }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['flip']).toBe('(idle)')
    })

    it('should include wasmDuration when total_duration_us > 0', () => {
      // Total WASM duration formatted in ms
      const trace = makeTrace(
        [makeStage({ stage: 'diff', accepted: ['x'] })],
        2500,
      )
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['wasmDuration']).toBe('2.50ms')
    })

    it('should omit wasmDuration when total_duration_us is 0', () => {
      // Zero total duration not shown
      const trace = makeTrace(
        [makeStage({ stage: 'diff', accepted: ['x'] })],
        0,
      )
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['wasmDuration']).toBeUndefined()
    })

    it('should handle empty stages array', () => {
      // Trace with no stages — no stages key added
      const trace = makeTrace([])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      expect(summary['stages']).toBeUndefined()
    })

    it('should include stages with only skipped entries', () => {
      // Stage with only skipped changes should still appear (not filtered out)
      const trace = makeTrace([
        makeStage({
          stage: 'sync',
          skipped: [
            { path: 'user.name', kind: 'redundant', reason: 'wrong_kind' },
          ],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['sync']).toEqual({
        skipped: ['user.name (wrong_kind)'],
      })
    })
  })

  // ---------------------------------------------------------------------------
  // buildConsoleSummary — listener entries
  // ---------------------------------------------------------------------------

  describe('listener entries', () => {
    it('should format listener entries with scope/input/output', () => {
      // Listener entries keyed by index, subscriber ID, and function name
      const data = makePipelineData({
        listeners: [
          {
            subscriberId: 42,
            fnName: 'onNameChange',
            scope: 'user',
            input: [['user.name', 'Alice', 'Bob']],
            output: [{ path: 'user.greeting', value: 'Hello Bob' }],
            durationMs: 0.8,
            slow: false,
          },
        ],
      })
      const summary = buildConsoleSummary(data)
      const listenerKey = Object.keys(summary).find((k) =>
        k.includes('listener:42'),
      )
      expect(listenerKey).toBeDefined()
      expect(listenerKey).toContain('onNameChange')
      expect(listenerKey).toContain('0.80ms')

      const entry = summary[listenerKey!] as Record<string, unknown>
      expect(entry['scope']).toBe('user')
      expect(entry['input']).toEqual([['user.name', 'Alice', 'Bob']])
      expect(entry['output']).toEqual({ 'user.greeting': 'Hello Bob' })
    })

    it('should mark slow listeners with [SLOW]', () => {
      // Slow listener flagged in the key
      const data = makePipelineData({
        listeners: [
          {
            subscriberId: 1,
            fnName: 'heavyComputation',
            scope: '',
            input: [],
            output: [],
            durationMs: 50,
            slow: true,
          },
        ],
      })
      const summary = buildConsoleSummary(data)
      const listenerKey = Object.keys(summary).find((k) =>
        k.includes('listener:1'),
      )
      expect(listenerKey).toContain('[SLOW]')
    })

    it('should use "(anonymous)" for unnamed listeners', () => {
      // Missing fnName defaults to "(anonymous)"
      const data = makePipelineData({
        listeners: [
          {
            subscriberId: 5,
            fnName: '',
            scope: 'root',
            input: [],
            output: [],
            durationMs: 0,
            slow: false,
          },
        ],
      })
      const summary = buildConsoleSummary(data)
      const listenerKey = Object.keys(summary).find((k) =>
        k.includes('listener:5'),
      )
      expect(listenerKey).toContain('(anonymous)')
    })

    it('should use "(root)" for empty scope', () => {
      // Empty scope string formatted as "(root)"
      const data = makePipelineData({
        listeners: [
          {
            subscriberId: 3,
            fnName: 'handler',
            scope: '',
            input: [],
            output: [],
            durationMs: 0,
            slow: false,
          },
        ],
      })
      const summary = buildConsoleSummary(data)
      const listenerKey = Object.keys(summary).find((k) =>
        k.includes('listener:3'),
      )
      const entry = summary[listenerKey!] as Record<string, unknown>
      expect(entry['scope']).toBe('(root)')
    })

    it('should order listener entries by index', () => {
      // Multiple listeners keyed with zero-padded index
      const data = makePipelineData({
        listeners: [
          {
            subscriberId: 1,
            fnName: 'first',
            scope: '',
            input: [],
            output: [],
            durationMs: 0,
            slow: false,
          },
          {
            subscriberId: 2,
            fnName: 'second',
            scope: '',
            input: [],
            output: [],
            durationMs: 0,
            slow: false,
          },
        ],
      })
      const summary = buildConsoleSummary(data)
      const keys = Object.keys(summary).filter((k) => k.includes('listener:'))
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
    const makeFullTrace = (): Wasm.PipelineTrace =>
      makeTrace(ALL_STAGES.map((stage) => makeStage({ stage })))

    it('should display all stages including idle ones', () => {
      // WASM now emits all stages — idle stages should appear as "(idle)"
      const trace = makeFullTrace()
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages).toBeDefined()

      // All unique stage names should be present
      for (const stageName of new Set(ALL_STAGES)) {
        expect(stages[stageName]).toBeDefined()
      }
    })

    it('should show idle stages as "(idle)" in full trace', () => {
      // A full trace where all stages are empty should show all as "(idle)"
      const trace = makeFullTrace()
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // All stages are empty → all should be "(idle)"
      for (const stageName of new Set(ALL_STAGES)) {
        expect(stages[stageName]).toBe('(idle)')
      }
    })

    it('should show a mix of active and idle stages', () => {
      // Realistic trace: some stages have activity, others are idle
      const trace = makeTrace([
        makeStage({
          stage: 'input',
          accepted: ['user.name'],
        }),
        makeStage({ stage: 'aggregation_write' }),
        makeStage({ stage: 'computation' }),
        makeStage({
          stage: 'diff',
          accepted: ['user.name'],
        }),
        makeStage({ stage: 'clear_path' }),
        makeStage({
          stage: 'sync',
          accepted: ['user.name'],
          produced: ['profile.name'],
        }),
        makeStage({ stage: 'flip' }),
        makeStage({ stage: 'aggregation_read' }),
        makeStage({ stage: 'computation' }),
        makeStage({
          stage: 'bool_logic',
          accepted: ['1'],
          produced: ['_concerns.user.name.disabled'],
        }),
        makeStage({ stage: 'value_logic' }),
        makeStage({
          stage: 'listeners',
          accepted: ['user.name', 'profile.name'],
        }),
        makeStage({
          stage: 'apply',
          accepted: ['4 changes'],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // Active stages have detail objects
      expect(stages['input']).toEqual({ accepted: ['user.name'] })
      expect(stages['sync']).toEqual({
        accepted: ['user.name'],
        produced: ['profile.name'],
      })
      expect(stages['bool_logic']).toEqual({
        accepted: ['1'],
        produced: ['_concerns.user.name.disabled'],
      })

      // Idle stages show "(idle)"
      expect(stages['aggregation_write']).toBe('(idle)')
      expect(stages['flip']).toBe('(idle)')
      expect(stages['value_logic']).toBe('(idle)')
    })

    it('should handle duplicate computation stage (filter + read)', () => {
      // Computation appears twice: once for filter (skipped), once for reads (produced)
      // The second entry overwrites the first in the stages object (keyed by stage name)
      const trace = makeTrace([
        makeStage({
          stage: 'computation',
          skipped: [
            {
              path: 'total',
              kind: 'real',
              reason: 'guard_failed',
            },
          ],
        }),
        makeStage({
          stage: 'computation',
          accepted: ['price', 'quantity'],
          produced: ['total'],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>

      // Second computation entry wins (last-write-wins for same key)
      expect(stages['computation']).toEqual({
        accepted: ['price', 'quantity'],
        produced: ['total'],
      })
    })

    it('should show anchor-skipped entries in aggregation_read', () => {
      // AggregationRead can skip paths due to disabled anchors
      const trace = makeTrace([
        makeStage({
          stage: 'aggregation_read',
          accepted: ['cart.item1.price', 'cart.item2.price'],
          produced: ['cart.total'],
          skipped: [
            {
              path: 'cart.summary',
              kind: 'real',
              reason: 'guard_failed',
            },
          ],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['aggregation_read']).toEqual({
        accepted: ['cart.item1.price', 'cart.item2.price'],
        produced: ['cart.total'],
        skipped: ['cart.summary (guard_failed)'],
      })
    })

    it('should show apply stage with change count', () => {
      // Apply stage shows the final change count as accepted
      const trace = makeTrace([
        makeStage({
          stage: 'apply',
          accepted: ['7 changes'],
        }),
      ])
      const summary = buildConsoleSummary(makePipelineData({ trace }))
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['apply']).toEqual({
        accepted: ['7 changes'],
      })
    })
  })
})

// ---------------------------------------------------------------------------
// addTraceSummary — direct tests
// ---------------------------------------------------------------------------

describe('addTraceSummary', () => {
  it('should add stages key to target object', () => {
    // Verifies addTraceSummary mutates the target object
    const obj: Record<string, unknown> = {}
    const trace = makeTrace([
      makeStage({ stage: 'diff', accepted: ['user.name'] }),
    ])
    addTraceSummary(obj, trace)
    expect(obj['stages']).toBeDefined()
    const stages = obj['stages'] as Record<string, unknown>
    expect(stages['diff']).toEqual({ accepted: ['user.name'] })
  })

  it('should not add stages key for empty trace', () => {
    // Empty stages array → no stages key
    const obj: Record<string, unknown> = {}
    addTraceSummary(obj, makeTrace([]))
    expect(obj['stages']).toBeUndefined()
  })

  it('should add wasmDuration when total_duration_us > 0', () => {
    // Non-zero total duration adds wasmDuration key
    const obj: Record<string, unknown> = {}
    addTraceSummary(obj, makeTrace([makeStage()], 5000))
    expect(obj['wasmDuration']).toBe('5.00ms')
  })

  it('should not add wasmDuration when total_duration_us is 0', () => {
    // Zero total duration → no wasmDuration key
    const obj: Record<string, unknown> = {}
    addTraceSummary(obj, makeTrace([makeStage()], 0))
    expect(obj['wasmDuration']).toBeUndefined()
  })

  it('should show all stages including idle ones', () => {
    // Even idle stages appear when included in the trace
    const obj: Record<string, unknown> = {}
    const trace = makeTrace([
      makeStage({ stage: 'input', accepted: ['x'] }),
      makeStage({ stage: 'sync' }),
      makeStage({ stage: 'flip' }),
    ])
    addTraceSummary(obj, trace)
    const stages = obj['stages'] as Record<string, unknown>
    expect(stages['input']).toEqual({ accepted: ['x'] })
    expect(stages['sync']).toBe('(idle)')
    expect(stages['flip']).toBe('(idle)')
  })

  it('should preserve existing keys on target object', () => {
    // addTraceSummary should not clobber existing keys
    const obj: Record<string, unknown> = { existing: 'value' }
    addTraceSummary(obj, makeTrace([makeStage({ stage: 'diff' })]))
    expect(obj['existing']).toBe('value')
    expect(obj['stages']).toBeDefined()
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

  describe('logPipeline', () => {
    it('should emit groupCollapsed → log → groupEnd in order', () => {
      // Pipeline log should be a single collapsed group with one log call
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      expect(spyLog).toHaveBeenCalledTimes(1)
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)

      // Verify call order: groupCollapsed before log before groupEnd
      const gcOrder = spyGroupCollapsed.mock.invocationCallOrder[0]
      const logOrder = spyLog.mock.invocationCallOrder[0]
      const geOrder = spyGroupEnd.mock.invocationCallOrder[0]
      expect(gcOrder).toBeLessThan(logOrder!)
      expect(logOrder).toBeLessThan(geOrder!)
    })

    it('should use "apex-state:pipeline" prefix in group label', () => {
      // Group label starts with prefix and includes path summary
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toMatch(/^apex-state:pipeline \| /)
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
          input: [
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

    it('should log summary object with input and duration', () => {
      // The console.log argument is the full summary object
      const logger = createLogger({ log: true })
      logger.logPipeline(makePipelineData())

      const summary = spyLog.mock.calls[0]![0] as Record<string, unknown>
      expect(summary['input']).toEqual({ 'user.name': 'Alice' })
      expect(summary['duration']).toBe('1.50ms')
    })

    it('should include stages in summary when trace is present', () => {
      // With trace data, summary contains stages + wasmDuration
      const logger = createLogger({ log: true })
      const trace = makeTrace(
        [
          makeStage({
            stage: 'diff',
            accepted: ['user.name'],
            produced: ['user.name'],
            duration_us: 120,
          }),
          makeStage({
            stage: 'sync',
            accepted: ['user.name'],
            produced: ['profile.name'],
            duration_us: 80,
          }),
          makeStage({ stage: 'flip' }),
        ],
        350,
      )
      logger.logPipeline(makePipelineData({ trace, durationMs: 2.1 }))

      const summary = spyLog.mock.calls[0]![0] as Record<string, unknown>
      expect(summary['wasmDuration']).toBe('0.35ms')
      expect(summary['stages']).toBeDefined()

      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['diff']).toEqual({
        accepted: ['user.name'],
        produced: ['user.name'],
        duration: '0.12ms',
      })
      expect(stages['sync']).toEqual({
        accepted: ['user.name'],
        produced: ['profile.name'],
        duration: '0.08ms',
      })
      expect(stages['flip']).toBe('(idle)')
    })

    it('should include listener entries in summary', () => {
      // Listener entries appear as keys in the summary object
      const logger = createLogger({ log: true })
      logger.logPipeline(
        makePipelineData({
          listeners: [
            {
              subscriberId: 7,
              fnName: 'onEmailChange',
              scope: 'user',
              input: [['user.email', 'old@test.com', 'new@test.com']],
              output: [{ path: 'user.emailVerified', value: false }],
              durationMs: 1.2,
              slow: false,
            },
          ],
        }),
      )

      const summary = spyLog.mock.calls[0]![0] as Record<string, unknown>
      const listenerKey = Object.keys(summary).find((k) =>
        k.includes('listener:7'),
      )
      expect(listenerKey).toBeDefined()
      expect(listenerKey).toContain('onEmailChange')
      expect(listenerKey).toContain('1.20ms')

      const entry = summary[listenerKey!] as Record<string, unknown>
      expect(entry['scope']).toBe('user')
      expect(entry['output']).toEqual({ 'user.emailVerified': false })
    })

    it('should produce a readable realistic pipeline log', () => {
      // Full realistic scenario: input → trace with mixed stages → listener output
      const logger = createLogger({ log: true })
      const trace = makeTrace(
        [
          makeStage({ stage: 'input', accepted: ['user.name'] }),
          makeStage({ stage: 'aggregation_write' }),
          makeStage({ stage: 'computation' }),
          makeStage({
            stage: 'diff',
            accepted: ['user.name'],
            duration_us: 50,
          }),
          makeStage({ stage: 'clear_path' }),
          makeStage({
            stage: 'sync',
            accepted: ['user.name'],
            produced: ['profile.displayName'],
            duration_us: 30,
          }),
          makeStage({ stage: 'flip' }),
          makeStage({ stage: 'aggregation_read' }),
          makeStage({ stage: 'computation' }),
          makeStage({
            stage: 'bool_logic',
            accepted: ['1'],
            produced: ['_concerns.user.name.disabled'],
            duration_us: 20,
          }),
          makeStage({ stage: 'value_logic' }),
          makeStage({
            stage: 'listeners',
            accepted: ['user.name', 'profile.displayName'],
          }),
          makeStage({ stage: 'apply', accepted: ['3 changes'] }),
        ],
        200,
      )

      logger.logPipeline(
        makePipelineData({
          input: [{ path: 'user.name', value: 'Bob' }],
          trace,
          listeners: [
            {
              subscriberId: 1,
              fnName: 'notifyNameChange',
              scope: 'user',
              input: [['user.name', 'Alice', 'Bob']],
              output: [{ path: 'user.greeting', value: 'Hello Bob' }],
              durationMs: 0.5,
              slow: false,
            },
          ],
          durationMs: 3.2,
        }),
      )

      // Verify group label is readable
      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toBe('apex-state:pipeline | user.name')

      // Verify summary structure has the right shape
      const summary = spyLog.mock.calls[0]![0] as Record<string, unknown>
      const keys = Object.keys(summary)

      // Should have: input, duration, stages, wasmDuration, [00] listener:1 ...
      expect(keys).toContain('input')
      expect(keys).toContain('duration')
      expect(keys).toContain('stages')
      expect(keys).toContain('wasmDuration')
      expect(keys.some((k) => k.includes('listener:1'))).toBe(true)

      // Verify key order: input and duration come first
      expect(keys[0]).toBe('input')
      expect(keys[1]).toBe('duration')

      // Active stages have detail, idle stages are "(idle)"
      const stages = summary['stages'] as Record<string, unknown>
      expect(stages['input']).toEqual({ accepted: ['user.name'] })
      expect(stages['aggregation_write']).toBe('(idle)')
      expect(stages['sync']).toEqual({
        accepted: ['user.name'],
        produced: ['profile.displayName'],
        duration: '0.03ms',
      })
      expect(stages['bool_logic']).toEqual({
        accepted: ['1'],
        produced: ['_concerns.user.name.disabled'],
        duration: '0.02ms',
      })
      expect(stages['flip']).toBe('(idle)')
    })
  })

  describe('logRegistration', () => {
    it('should log registration with "add" prefix', () => {
      // Register event uses "add" action, wrapped in groupCollapsed
      const logger = createLogger({ log: true })
      const snapshot = {
        sync_pairs: [['user.name', 'profile.name'] as [string, string]],
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
      expect(label).toBe('apex-state:registration | add concern-123')
      expect(spyLog).toHaveBeenCalledTimes(1)
      const summary = spyLog.mock.calls[0]![0] as Record<string, unknown>
      expect(summary).toHaveProperty('snapshot', snapshot)
      expect(spyGroupEnd).toHaveBeenCalledTimes(1)
    })

    it('should log unregistration with "remove" prefix', () => {
      // Unregister event uses "remove" action, wrapped in groupCollapsed
      const logger = createLogger({ log: true })
      logger.logRegistration('unregister', 'concern-456', {
        sync_pairs: [],
        flip_pairs: [],
        listeners: [],
        bool_logics: [],
        value_logics: [],
        aggregations: [],
        computations: [],
      })

      expect(spyGroupCollapsed).toHaveBeenCalledTimes(1)
      const label = spyGroupCollapsed.mock.calls[0]![0] as string
      expect(label).toBe('apex-state:registration | remove concern-456')
    })
  })

  describe('noop logger (log: false)', () => {
    it('should not emit any console calls', () => {
      // When log is false, no console output at all
      const logger = createLogger({ log: false })
      logger.logPipeline(makePipelineData())
      logger.logRegistration('register', 'test', {
        sync_pairs: [],
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

  it('should show accepted paths in the input stage', () => {
    // The input stage should show which paths were accepted after diff filtering
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice' } })

    const result = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Bob' },
    ])
    const inputStage = result.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'input',
    )

    expect(inputStage).toBeDefined()
    expect(inputStage!.accepted).toContain('user.name')
  })

  it('should show skipped changes when value is unchanged', () => {
    // Sending same value should be filtered in input stage as skipped
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice' } })

    processAndFinalize(pipeline, [{ path: 'user.name', value: 'Alice' }])

    // Pipeline returns early on no-op, but if trace exists, input should show skip
    // When all changes are redundant, processChanges returns early with no trace
    // So we test with a mix: one real change + one redundant
    pipeline.destroy()

    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ user: { name: 'Alice', age: 25 } })

    const result2 = processAndFinalize(pipeline, [
      { path: 'user.name', value: 'Alice' }, // redundant
      { path: 'user.age', value: 30 }, // real change
    ])

    expect(result2.trace).not.toBeNull()
    const inputStage = result2.trace!.stages.find(
      (s: Wasm.StageTrace) => s.stage === 'input',
    )
    expect(inputStage).toBeDefined()
    // user.age accepted, user.name skipped (redundant)
    expect(inputStage!.accepted).toContain('user.age')
    expect(inputStage!.skipped.some((s) => s.path === 'user.name')).toBe(true)
  })

  it('should show sync stage producing synced paths', () => {
    // Register sync pairs and verify the sync stage shows produced paths
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
    expect(syncStage!.produced).toContain('target')
  })

  it('should flow trace through buildConsoleSummary correctly', () => {
    // End-to-end: WASM trace → buildConsoleSummary → readable output
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ source: 'hello', target: '' })

    pipeline.registerSideEffects({
      registration_id: 'test-sync',
      sync_pairs: [['source', 'target']],
    })

    const result = processAndFinalize(pipeline, [
      { path: 'source', value: 'world' },
    ])

    // Build the console summary as the logger would
    const logData: PipelineLogData = {
      input: [{ path: 'source', value: 'world' }],
      trace: result.trace,
      listeners: [],
      durationMs: 1.0,
    }
    const summary = buildConsoleSummary(logData)

    // Summary should have stages from real WASM trace
    expect(summary['stages']).toBeDefined()
    const stages = summary['stages'] as Record<string, unknown>

    // Input stage should have accepted paths
    const inputDetail = stages['input'] as Record<string, unknown>
    expect(inputDetail).toBeDefined()
    expect(inputDetail).not.toBe('(idle)')

    // Sync stage should show produced target
    const syncDetail = stages['sync'] as Record<string, unknown>
    expect(syncDetail).toBeDefined()
    expect(syncDetail).not.toBe('(idle)')
  })

  it('should produce readable console output for createLogger', () => {
    // Full e2e: WASM → trace → createLogger → console spy
    pipeline = createWasmPipeline({ debug: true })
    pipeline.shadowInit({ x: 1, y: 2 })

    const result = processAndFinalize(pipeline, [{ path: 'x', value: 10 }])

    const spyGC = vi.spyOn(console, 'groupCollapsed').mockImplementation(noop)
    const spyL = vi.spyOn(console, 'log').mockImplementation(noop)
    const spyGE = vi.spyOn(console, 'groupEnd').mockImplementation(noop)

    const logger = createLogger({ log: true })
    logger.logPipeline({
      input: [{ path: 'x', value: 10 }],
      trace: result.trace,
      listeners: [],
      durationMs: 0.5,
    })

    // Verify grouped output
    expect(spyGC).toHaveBeenCalledTimes(1)
    const label = spyGC.mock.calls[0]![0] as string
    expect(label).toBe('apex-state:pipeline | x')

    // Verify summary has real WASM trace data
    const summary = spyL.mock.calls[0]![0] as Record<string, unknown>
    expect(summary['stages']).toBeDefined()
    expect(summary['input']).toEqual({ x: 10 })
    expect(summary['duration']).toBe('0.50ms')

    // Stages should contain real data, not all "(idle)"
    const stages = summary['stages'] as Record<string, unknown>
    const inputStage = stages['input']
    expect(inputStage).not.toBe('(idle)')

    spyGC.mockRestore()
    spyL.mockRestore()
    spyGE.mockRestore()
  })
})
