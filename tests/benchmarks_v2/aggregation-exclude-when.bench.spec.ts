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

import { type Change, createWasmPipeline } from '~/wasm/bridge'
import { loadWasm } from '~/wasm/lifecycle'

import { BENCH_OPTIONS } from './helpers'

// Load WASM before benchmarks run
await loadWasm()

// ---------------------------------------------------------------------------
// File-specific helpers (aggregation-only, not shared)
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
  pipeline.shadowInit(state)
  pipeline.registerSideEffects({
    registration_id: 'bench',
    aggregation_pairs: pairs,
  })
  return pipeline
}

// ---------------------------------------------------------------------------
// A. Registration (cold path — measures condition parsing overhead)
// ---------------------------------------------------------------------------

/**
 * @perf-history — Registration: excludeWhen parsing overhead
 * Hardware: Apple M4 Pro
 * | Date       | Variant                     | Hz (ops/sec)  | Commit  | Note                          |
 * |------------|-----------------------------|---------------|---------|-------------------------------|
 * | 2026-02-22 | 10 sources, 0 conditions    | 70,803        | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 10 sources, 5 conditions    | 53,409        | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 50 sources, 25 conditions   | 10,922        | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 200 sources, 100 conditions | 2,401         | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-25 | 10 sources, 0 conditions    | 69,980        | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 10 sources, 5 conditions    | 50,698        | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 50 sources, 25 conditions   | 10,329        | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 200 sources, 100 conditions | 2,270         | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 */
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
        pipeline.shadowInit(state)
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

/**
 * @perf-history — Read direction: source value change with excludeWhen
 * Hardware: Apple M4 Pro
 * | Date       | Variant                     | Hz (ops/sec)  | Commit  | Note                          |
 * |------------|-----------------------------|---------------|---------|-------------------------------|
 * | 2026-02-22 | 10 sources, 0 conditions    | 921,193       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 10 sources, 5 conditions    | 906,741       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 50 sources, 25 conditions   | 931,031       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 200 sources, 100 conditions | 942,601       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-25 | 10 sources, 0 conditions    | 927,286       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 10 sources, 5 conditions    | 937,107       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 50 sources, 25 conditions   | 932,782       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 200 sources, 100 conditions | 930,469       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 */
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

/**
 * @perf-history — Condition path change: re-aggregation trigger
 * Hardware: Apple M4 Pro
 * | Date       | Variant                     | Hz (ops/sec)  | Commit  | Note                          |
 * |------------|-----------------------------|---------------|---------|-------------------------------|
 * | 2026-02-22 | 10 sources, 5 conditions    | 1,106,931     | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 50 sources, 25 conditions   | 1,118,466     | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 200 sources, 100 conditions | 1,103,534     | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-25 | 10 sources, 5 conditions    | 1,115,183     | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 50 sources, 25 conditions   | 1,115,744     | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 200 sources, 100 conditions | 1,117,752     | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 */
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

/**
 * @perf-history — Write direction: target → sources with excludeWhen filtering
 * Hardware: Apple M4 Pro
 * | Date       | Variant                     | Hz (ops/sec)  | Commit  | Note                          |
 * |------------|-----------------------------|---------------|---------|-------------------------------|
 * | 2026-02-22 | 10 sources, 0 conditions    | 935,250       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 10 sources, 5 conditions    | 913,295       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 50 sources, 25 conditions   | 908,792       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-22 | 200 sources, 100 conditions | 919,062       | 4de0ee8 | baseline — initial measurement |
 * | 2026-02-25 | 10 sources, 0 conditions    | 957,429       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 10 sources, 5 conditions    | 950,639       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 50 sources, 25 conditions   | 953,831       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 * | 2026-02-25 | 200 sources, 100 conditions | 951,551       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
 */
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
