/**
 * Benchmarks: Pipeline Baseline & Scaling (CRITICAL METRICS)
 *
 * Core performance metrics for the change processing pipeline:
 * - Single change latency (baseline)
 * - Batching efficiency (key optimization)
 * - Scaling with state size
 * - Scaling with side effects
 *
 * These metrics establish the foundation for all other performance targets.
 * Uses createWasmPipeline() directly (no React, no store overhead).
 */

import { bench, describe } from 'vitest'

import { type Change } from '~/wasm/bridge'
import { loadWasm } from '~/wasm/lifecycle'

import {
  BENCH_OPTIONS,
  buildBatch,
  createBarePipeline,
  createFlipPipeline,
  createListenerPipeline,
  createSyncPipeline,
} from './helpers'

// Load WASM before benchmarks run
await loadWasm()

// ---------------------------------------------------------------------------
// Basic pipeline latency
// ---------------------------------------------------------------------------

describe('WASM Pipeline: Baseline & Scaling', () => {
  describe('Basic pipeline latency', () => {
    const barePipeline = createBarePipeline(10)
    const syncPipeline = createSyncPipeline(1)
    const flipPipeline = createFlipPipeline(1)
    const listenerPipeline = createListenerPipeline(1)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 560,889      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 769,702      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'single change through pipeline (no side effects)',
      () => {
        // Measure: processChanges round-trip for a single field change
        const changes: Change[] = [
          { path: 'field_0', value: 'updated', meta: {} },
        ]
        barePipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 548,419      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 776,191      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'single change with sync path',
      () => {
        // Measure: processChanges with sync graph evaluation
        const changes: Change[] = [
          { path: 'field_0', value: 'synced', meta: {} },
        ]
        syncPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 803,108      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 1,117,087    | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'single change with flip path',
      () => {
        // Measure: processChanges with flip graph evaluation
        const changes: Change[] = [{ path: 'bool_0', value: true, meta: {} }]
        flipPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 549,017      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 770,356      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'single change with listener',
      () => {
        // Measure: processChanges with listener routing + execution plan
        const changes: Change[] = [
          { path: 'field_0', value: 'listened', meta: {} },
        ]
        listenerPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Batched changes
  // ---------------------------------------------------------------------------

  describe('Batched changes (the optimization)', () => {
    const batchPipeline = (() => {
      const p = createBarePipeline(1000)
      p.registerSideEffects({
        registration_id: 'bench',
        sync_pairs: [['field_0', 'field_1']],
      })
      return p
    })()

    const batch10 = buildBatch(10)
    const batch50 = buildBatch(50)
    const batch100 = buildBatch(100)
    const batch1000 = buildBatch(1000)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 61,502       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 60,183       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'batch of 10 changes through pipeline',
      () => {
        // Single WASM round-trip for all 10 changes
        batchPipeline.processChanges(batch10)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 18,688       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 18,208       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'batch of 50 changes',
      () => {
        batchPipeline.processChanges(batch50)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 10,119       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 9,846        | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'batch of 100 changes',
      () => {
        batchPipeline.processChanges(batch100)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 1,032        | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 1,053        | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'batch of 1000 changes',
      () => {
        batchPipeline.processChanges(batch1000)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Scaling with state size
  // ---------------------------------------------------------------------------

  describe('Scaling with state size', () => {
    const smallPipeline = createBarePipeline(10)
    const mediumPipeline = createBarePipeline(1_000)
    const largePipeline = createBarePipeline(100_000)

    const singleChange: Change[] = [
      { path: 'field_0', value: 'updated', meta: {} },
    ]

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 789,676      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 766,981      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges on small state (10 items)',
      () => {
        smallPipeline.processChanges(singleChange)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 765,385      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 771,310      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges on medium state (1000 items)',
      () => {
        mediumPipeline.processChanges(singleChange)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 784,546      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 763,254      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges on large state (100,000 items)',
      () => {
        largePipeline.processChanges(singleChange)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Scaling with side effects
  // ---------------------------------------------------------------------------

  describe('Scaling with side effects', () => {
    const sync10Pipeline = createSyncPipeline(10)
    const sync100Pipeline = createSyncPipeline(100)
    const listener10Pipeline = createListenerPipeline(10)
    const listener100Pipeline = createListenerPipeline(100)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 802,849      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 785,244      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges with 10 side effects matching',
      () => {
        // Change field_0 which triggers sync to sync_target_0
        const changes: Change[] = [
          { path: 'field_0', value: 'sync-10', meta: {} },
        ]
        sync10Pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 787,178      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 754,004      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges with 100 side effects matching',
      () => {
        const changes: Change[] = [
          { path: 'field_0', value: 'sync-100', meta: {} },
        ]
        sync100Pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 799,217      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 750,701      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges with 10 listeners triggered',
      () => {
        const changes: Change[] = [
          { path: 'field_0', value: 'listen-10', meta: {} },
        ]
        listener10Pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 792,481      | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 757,977      | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'processChanges with 100 listeners triggered',
      () => {
        const changes: Change[] = [
          { path: 'field_0', value: 'listen-100', meta: {} },
        ]
        listener100Pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })
})
