/**
 * Benchmarks: WASM Pipeline — Effect Types & Combinations
 *
 * Restructured from Legacy JS vs WASM comparison to WASM-only benchmarks
 * measuring each effect type and their combinations in isolation.
 *
 * The Legacy JS pipeline requires full valtio store setup which is too heavy
 * for standalone benchmarks. This suite focuses on WASM pipeline performance
 * across all effect combinations: sync, flip, listeners, BoolLogic, and their
 * combinations.
 *
 * Uses createWasmPipeline() directly (no React, no store overhead).
 * Test data builders create state/pipelines once, only measure processChanges().
 *
 * NOTE: When implementing bench() calls, add a `@perf-history` comment block
 * above each one. See CLAUDE.md § Benchmarks for the required table format.
 */

import { bench, describe } from 'vitest'

import { type Change, createWasmPipeline } from '../../src/wasm/bridge'
import { loadWasm } from '../../src/wasm/lifecycle'
import {
  BENCH_OPTIONS,
  createBarePipeline,
  createBoolLogicPipeline,
  createCombinedPipeline,
  createFlipPipeline,
  createListenerPipeline,
  createSyncPipeline,
} from './helpers'

// Load WASM before benchmarks run
await loadWasm()

// ---------------------------------------------------------------------------
// File-specific helpers (not shared — used only here)
// ---------------------------------------------------------------------------

/** Create E-commerce pipeline with 15 orders. */
const createEcommercePipeline = () => {
  const state: Record<string, unknown> = {}
  // 15 orders: currency (synced), confirmed (boolean), invoice_pending (flip), audit (listener output)
  for (let i = 0; i < 15; i++) {
    state[`order_${i}_currency`] = 'USD'
    state[`order_${i}_confirmed`] = false
    state[`order_${i}_invoice_pending`] = true
    state[`order_${i}_audit`] = ''
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(state)

  // Sync pairs: all orders to order_0 currency
  const syncPairs: [string, string][] = []
  for (let i = 1; i < 15; i++) {
    syncPairs.push([`order_0_currency`, `order_${i}_currency`])
  }

  // Flip pairs: confirmed ↔ invoice_pending
  const flipPairs: [string, string][] = []
  for (let i = 0; i < 15; i++) {
    flipPairs.push([`order_${i}_confirmed`, `order_${i}_invoice_pending`])
  }

  // Listeners: audit trail on each order
  const listeners = []
  for (let i = 0; i < 15; i++) {
    listeners.push({
      subscriber_id: i,
      topic_path: `order_${i}_confirmed`,
      scope_path: '',
    })
  }

  pipeline.registerSideEffects({
    registration_id: 'bench',
    sync_pairs: syncPairs,
    flip_pairs: flipPairs,
    listeners,
  })
  return pipeline
}

// ---------------------------------------------------------------------------
// Bare pipeline — no side effects registered
// ---------------------------------------------------------------------------

describe('WASM Pipeline: Effect Types & Combinations', () => {
  describe('Bare pipeline (no effects)', () => {
    const pipeline = createBarePipeline(10)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      'single field change (baseline reference)',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'updated' }]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Sync paths — bidirectional value propagation
  // ---------------------------------------------------------------------------

  describe('Sync paths', () => {
    const syncPipeline10 = createSyncPipeline(10)
    const syncPipeline50 = createSyncPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 10 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'synced' }]
        syncPipeline10.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 50 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'synced' }]
        syncPipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Flip paths — inverse boolean propagation
  // ---------------------------------------------------------------------------

  describe('Flip paths', () => {
    const flipPipeline10 = createFlipPipeline(10)
    const flipPipeline50 = createFlipPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 10 flip pairs',
      () => {
        const changes: Change[] = [{ path: 'bool_0', value: true }]
        flipPipeline10.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 50 flip pairs',
      () => {
        const changes: Change[] = [{ path: 'bool_0', value: true }]
        flipPipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Listeners — dispatch overhead only
  // ---------------------------------------------------------------------------

  describe('Listeners (dispatch overhead only)', () => {
    const listenerPipeline15 = createListenerPipeline(15)
    const listenerPipeline50 = createListenerPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change dispatched to 15 listeners',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'listened' }]
        listenerPipeline15.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change dispatched to 50 listeners',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'listened' }]
        listenerPipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // BoolLogic concerns — declarative evaluation
  // ---------------------------------------------------------------------------

  describe('BoolLogic concerns', () => {
    const boolLogicPipeline20 = createBoolLogicPipeline(20)
    const boolLogicPipeline50 = createBoolLogicPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 20 BoolLogic evaluations',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'trigger' }]
        boolLogicPipeline20.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change triggers 50 BoolLogic evaluations',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'trigger' }]
        boolLogicPipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Combined effects — sync + flip + listeners
  // ---------------------------------------------------------------------------

  describe('Combined effects', () => {
    const combinedPipeline2x10 = createCombinedPipeline(10, 10, 10)
    const combinedPipeline4x = createCombinedPipeline(10, 10, 10)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '2 changes through 10 sync + 10 flip + 10 listeners',
      () => {
        const changes: Change[] = [
          { path: 'field_0', value: 'synced' },
          { path: 'bool_0', value: true },
        ]
        combinedPipeline2x10.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '4 changes through sync + flip + listeners + BoolLogic',
      () => {
        const changes: Change[] = [
          { path: 'field_0', value: 'change1' },
          { path: 'field_1', value: 'change2' },
          { path: 'bool_0', value: true },
          { path: 'bool_1', value: false },
        ]
        combinedPipeline4x.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Batch scaling — how latency grows with change count
  // ---------------------------------------------------------------------------

  describe('Batch scaling', () => {
    const pipeline10 = createSyncPipeline(20)
    const pipeline50 = createSyncPipeline(20)
    const pipeline100 = createSyncPipeline(20)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '10 changes with 20 sync pairs',
      () => {
        const changes: Change[] = []
        for (let i = 0; i < 10; i++) {
          changes.push({ path: `field_${i % 20}`, value: `batch${i}` })
        }
        pipeline10.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '50 changes with 20 sync pairs',
      () => {
        const changes: Change[] = []
        for (let i = 0; i < 50; i++) {
          changes.push({ path: `field_${i % 20}`, value: `batch${i}` })
        }
        pipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '100 changes with 20 sync pairs',
      () => {
        const changes: Change[] = []
        for (let i = 0; i < 100; i++) {
          changes.push({ path: `field_${i % 20}`, value: `batch${i}` })
        }
        pipeline100.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // Effect count scaling — how latency grows with registered effects
  // ---------------------------------------------------------------------------

  describe('Effect count scaling', () => {
    const syncPipeline5 = createSyncPipeline(5)
    const syncPipeline25 = createSyncPipeline(25)
    const syncPipeline50 = createSyncPipeline(50)
    const syncPipeline100 = createSyncPipeline(100)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change with 5 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'scaled' }]
        syncPipeline5.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change with 25 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'scaled' }]
        syncPipeline25.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change with 50 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'scaled' }]
        syncPipeline50.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      '1 change with 100 sync pairs',
      () => {
        const changes: Change[] = [{ path: 'field_0', value: 'scaled' }]
        syncPipeline100.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  // ---------------------------------------------------------------------------
  // E-commerce — realistic order management workflow
  // ---------------------------------------------------------------------------

  describe('E-commerce workflow', () => {
    const ecommercePipeline = createEcommercePipeline()

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     */
    bench(
      'currency change cascades through 15 orders',
      () => {
        // Trigger: change order_0 currency + confirm order_0
        const changes: Change[] = [
          { path: 'order_0_currency', value: 'EUR' },
          { path: 'order_0_confirmed', value: true },
        ]
        ecommercePipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })
})
