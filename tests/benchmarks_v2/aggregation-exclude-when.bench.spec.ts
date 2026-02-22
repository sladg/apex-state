/**
 * Benchmarks: Aggregation excludeWhen Performance
 *
 * Measures the performance impact of BoolLogic excludeWhen conditions
 * on aggregation processing across four dimensions:
 * - Registration (cold path — condition parsing overhead)
 * - Read direction (source → target with condition evaluation)
 * - Condition path change (reverse index + re-aggregation)
 * - Write direction (target → sources with condition filtering)
 *
 * Uses createWasmPipeline() directly (no React, no store overhead).
 */

import { bench, describe } from 'vitest'

import { type Change, createWasmPipeline } from '../../src/wasm/bridge'
import { loadWasm } from '../../src/wasm/lifecycle'

// Load WASM before benchmarks run
await loadWasm()

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Build state + aggregation_pairs for N sources, first `excludedCount` having excludeWhen. */
const buildAggregationSetup = (sourceCount: number, excludedCount: number) => {
  const state: Record<string, unknown> = { target: null }
  const pairs: ([string, string] | [string, string, string])[] = []

  for (let i = 0; i < sourceCount; i++) {
    const srcKey = `source_${i}`
    state[srcKey] = 100

    if (i < excludedCount) {
      const disabledKey = `disabled_${i}`
      state[disabledKey] = true
      pairs.push([
        'target',
        srcKey,
        JSON.stringify({ IS_EQUAL: [disabledKey, true] }),
      ])
    } else {
      pairs.push(['target', srcKey])
    }
  }

  return { state, pairs }
}

/** Create a pipeline with aggregation setup, ready for benchmarking. */
const createSetup = (sourceCount: number, excludedCount: number) => {
  const { state, pairs } = buildAggregationSetup(sourceCount, excludedCount)
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state as Record<string, unknown>)
  pipeline.registerSideEffects({
    registration_id: 'bench',
    aggregation_pairs: pairs,
  })
  return pipeline
}

const BENCH_OPTIONS = { iterations: 50, warmupIterations: 5 }

// ---------------------------------------------------------------------------
// A. Registration (cold path — measures condition parsing overhead)
// ---------------------------------------------------------------------------

describe('Registration: excludeWhen parsing overhead', () => {
  for (const [sources, excluded] of [
    [10, 0],
    [10, 5],
    [50, 25],
    [200, 100],
  ] as const) {
    bench(
      `register ${sources} sources, ${excluded} conditions`,
      () => {
        const { state, pairs } = buildAggregationSetup(sources, excluded)
        const pipeline = createWasmPipeline()
        pipeline.shadowInit(state as Record<string, unknown>)
        pipeline.registerSideEffects({
          registration_id: 'bench',
          aggregation_pairs: pairs,
        })
        pipeline.destroy()
      },
      BENCH_OPTIONS,
    )
  }
})

// ---------------------------------------------------------------------------
// B. Read direction — source value change (measures process_aggregation_reads)
// ---------------------------------------------------------------------------

describe('Read direction: source value change with excludeWhen', () => {
  for (const [sources, excluded] of [
    [10, 0],
    [10, 5],
    [50, 25],
    [200, 100],
  ] as const) {
    const pipeline = createSetup(sources, excluded)

    bench(
      `processChanges: ${sources} sources, ${excluded} conditions`,
      () => {
        const changes: Change[] = [
          { path: `source_${sources - 1}`, value: 999 },
        ]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  }
})

// ---------------------------------------------------------------------------
// C. Condition path change (measures condition reverse index + re-aggregation)
// ---------------------------------------------------------------------------

describe('Condition path change: re-aggregation trigger', () => {
  for (const [sources, excluded] of [
    [10, 5],
    [50, 25],
    [200, 100],
  ] as const) {
    const pipeline = createSetup(sources, excluded)

    bench(
      `condition change: ${sources} sources, ${excluded} conditions`,
      () => {
        const changes: Change[] = [{ path: 'disabled_0', value: false }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  }
})

// ---------------------------------------------------------------------------
// D. Write direction — target change distributed to sources
// ---------------------------------------------------------------------------

describe('Write direction: target → sources with excludeWhen filtering', () => {
  for (const [sources, excluded] of [
    [10, 0],
    [10, 5],
    [50, 25],
    [200, 100],
  ] as const) {
    const pipeline = createSetup(sources, excluded)

    bench(
      `write distribution: ${sources} sources, ${excluded} conditions`,
      () => {
        const changes: Change[] = [{ path: 'target', value: 42 }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  }
})
