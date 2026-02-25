/**
 * Benchmarks: Profiling & Internal Metrics (DEBUG & INTROSPECTION)
 *
 * Fine-grained metrics for understanding pipeline internals:
 * - Shadow state update costs (WASM internal data structure operations)
 * - Concern evaluation costs (BoolLogic tree walking)
 * - Memory efficiency (allocations, leaks, pressure)
 * - Performance regression detection (baselines to track regressions)
 * - CPU profiling points (breakdowns of critical path)
 *
 * These are used for optimization guidance and regression detection.
 *
 * NOTE: When implementing bench() calls, add a `@perf-history` comment block
 * above each one. See CLAUDE.md § Benchmarks for the required table format.
 */

import { beforeAll, bench, describe } from 'vitest'

import { type Change, createWasmPipeline } from '~/wasm/bridge'
import { loadWasm } from '~/wasm/lifecycle'

import {
  BENCH_OPTIONS,
  buildBatch,
  buildDeepState,
  buildFields,
  createBarePipeline,
  createBoolLogicPipeline,
  createMultiPathListenerPipeline,
} from './helpers'

beforeAll(async () => {
  await loadWasm()
})

describe('WASM Pipeline: Profiling & Introspection', () => {
  describe('Shadow state update cost', () => {
    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 41,497       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'shadow state update (simple field)',
      () => {
        // Setup: Pipeline with 10 simple fields, no effects
        const pipeline = createBarePipeline(10)

        // Measure: Process single flat field change
        const changes: Change[] = [{ path: 'field_0', value: 'updated' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 16,419       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'shadow state update (deep nested field)',
      () => {
        // Setup: Pipeline with deeply nested state (15 levels)
        const deepState = buildDeepState(15)
        const pipeline = createWasmPipeline()
        pipeline.shadowInit(deepState)

        // Measure: Process change at deep path
        const changes: Change[] = [
          {
            path: 'l1.l2.l3.l4.l5.l6.l7.l8.l9.l10.l11.l12.l13.l14.l15.value',
            value: 'deep_update',
          },
        ]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 2,251        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'shadow state update (large object)',
      () => {
        // Setup: Pipeline with 10 fields
        const pipeline = createBarePipeline(10)

        // Measure: Update field with large object (100 key-value pairs)
        const largeObj = buildFields(100, 'key')
        const changes: Change[] = [{ path: 'field_0', value: largeObj }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  describe('Concern evaluation (WASM)', () => {
    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 22,063       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'BoolLogic evaluation (simple: IS_EQUAL)',
      () => {
        // Setup: Pipeline with 1 simple BoolLogic concern
        const pipeline = createWasmPipeline()
        pipeline.shadowInit({ trigger: false })

        pipeline.registerConcerns({
          registration_id: 'bench',
          bool_logics: [
            {
              output_path: 'trigger.disabledWhen',
              tree_json: JSON.stringify({ IS_EQUAL: ['trigger', true] }),
            },
          ],
        })

        // Measure: Process change to trigger field
        const changes: Change[] = [{ path: 'trigger', value: true }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 26,163       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'BoolLogic evaluation (complex: nested AND/OR)',
      () => {
        // Setup: Pipeline with complex nested BoolLogic
        const pipeline = createWasmPipeline()
        pipeline.shadowInit({ a: false, b: false, c: false, d: false })

        const complexTree = {
          AND: [
            { IS_EQUAL: ['a', true] },
            {
              OR: [
                { IS_EQUAL: ['b', true] },
                {
                  AND: [{ IS_EQUAL: ['c', true] }, { EXISTS: 'd' }],
                },
              ],
            },
          ],
        }

        pipeline.registerConcerns({
          registration_id: 'bench',
          bool_logics: [
            {
              output_path: 'a.disabledWhen',
              tree_json: JSON.stringify(complexTree),
            },
          ],
        })

        // Measure: Process change to 'a'
        const changes: Change[] = [{ path: 'a', value: true }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 1,469        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'BoolLogic evaluation (many conditions)',
      () => {
        // Setup: Pipeline with 100 BoolLogic concerns all depending on field_0
        const pipeline = createBoolLogicPipeline(100)

        // Measure: Process change to trigger
        const changes: Change[] = [{ path: 'field_0', value: 'trigger' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  describe('Memory efficiency', () => {
    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 18,996       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'WASM memory allocation per change',
      () => {
        // Setup: Create pipeline, process change, destroy (allocation cycle)
        const pipeline = createWasmPipeline()
        pipeline.shadowInit({ field: 'value' })

        // Measure: Full cycle (create + process + GC)
        const changes: Change[] = [{ path: 'field', value: 'updated' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 486          | aa7e7da | baseline — initial measurement |
     */
    bench(
      'WASM memory with 1000 state items',
      () => {
        // Setup: Create pipeline with 1000 fields
        const pipeline = createBarePipeline(1000)

        // Measure: Process single change to field_500
        const changes: Change[] = [{ path: 'field_500', value: 'updated' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 1,297        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'WASM memory cleanup after processing',
      () => {
        // Setup: Pipeline with 100 fields
        const pipeline = createBarePipeline(100)

        // Measure: Process 100 different changes sequentially
        const changes = buildBatch(100)
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  describe('Performance regression safeguards', () => {
    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 11,964       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'regression: processChanges latency baseline',
      () => {
        // Setup: Bare pipeline with 10 fields, no concerns
        const pipeline = createBarePipeline(10)

        // Measure: Single baseline change
        const changes: Change[] = [{ path: 'field_0', value: 'baseline' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 9,758        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'regression: batching efficiency ratio',
      () => {
        // Setup: Pipeline with 2 fields to test batching
        const pipeline = createWasmPipeline()
        pipeline.shadowInit({ a: true, b: false })

        // Measure: Batch of 10 changes at once
        const changes: Change[] = []
        for (let i = 0; i < 10; i++) {
          changes.push({ path: i % 2 === 0 ? 'a' : 'b', value: i % 2 === 0 })
        }
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 74           | aa7e7da | baseline — initial measurement |
     */
    bench(
      'regression: scaling factor (10x items → latency)',
      () => {
        // Setup: Pipeline with 10,000 fields (10x larger than baseline)
        const pipeline = createBarePipeline(10_000)

        // Measure: Single change (should still be O(1))
        const changes: Change[] = [{ path: 'field_5000', value: 'updated' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  describe('Profile points (for CPU profiling)', () => {
    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 10,187       | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: changeNormalization time',
      () => {
        // Isolate: Normalization + shadow update (no effects)
        const pipeline = createBarePipeline(10)

        const changes: Change[] = [{ path: 'field_0', value: 'normalized' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 3,232        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: shadowStateUpdate time',
      () => {
        // Isolate: Shadow state update (larger state, mid-field change)
        const pipeline = createBarePipeline(100)

        // Update middle field to approximate shadow update cost
        const changes: Change[] = [{ path: 'field_50', value: 'shadow_update' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 446          | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: pathInterner.intern time',
      () => {
        // Isolate: Path interning cost (many different paths)
        const pipeline = createBarePipeline(1000)

        // Process changes to 10 different paths (measures interning)
        const changes: Change[] = []
        for (let i = 0; i < 10; i++) {
          changes.push({ path: `field_${i * 100}`, value: `interned_${i}` })
        }
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 4,960        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: graphEvaluation time',
      () => {
        // Approximate: Graph evaluation (vs bare pipeline)
        const pipeline = createWasmPipeline()
        pipeline.shadowInit({
          a: true,
          b: false,
          c: false,
          d: false,
          e: false,
          f: false,
          g: false,
          h: false,
          i: false,
          j: false,
        })

        // Register 50 sync pairs (significant graph structure)
        const syncPairs: [string, string][] = []
        for (let i = 0; i < 50; i++) {
          const source = String.fromCharCode(97 + (i % 10)) // a-j
          const target = String.fromCharCode(97 + ((i + 1) % 10))
          syncPairs.push([source, target])
        }

        pipeline.registerSideEffects({
          registration_id: 'bench',
          sync_pairs: syncPairs,
        })

        // Change source field (triggers graph evaluation)
        const changes: Change[] = [{ path: 'a', value: false }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 4,097        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: topicRouter.dispatch time',
      () => {
        // Approximate: Topic routing cost (50 listeners on different paths)
        const pipeline = createMultiPathListenerPipeline(50)

        // Process change (routes through topic router)
        const changes: Change[] = [{ path: 'field_0', value: 'routed' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 |              | TBD     | baseline — initial measurement |
     * | 2026-02-25 | 3,832        | aa7e7da | baseline — initial measurement |
     */
    bench(
      'profile: listenerExecution time',
      () => {
        // Approximate: Listener dispatch plan execution (100 listeners on different paths)
        const pipeline = createMultiPathListenerPipeline(100)

        // Process change (emphasizes dispatch plan complexity)
        const changes: Change[] = [{ path: 'field_0', value: 'executed' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })
})
