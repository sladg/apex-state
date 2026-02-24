/**
 * Benchmarks: Optimization Validation (ARCHITECTURE DECISIONS)
 *
 * Validates the performance gains from key architectural optimizations:
 * - Boundary crossing overhead (serialization, deserialization)
 * - Path interning (string → ID mapping efficiency)
 * - Pre-computed sync/flip graphs (vs runtime evaluation)
 * - Topic router pre-computation (vs dynamic listener matching)
 *
 * These benchmarks measure the cost/benefit of design choices.
 *
 * NOTE: When implementing bench() calls, add a `@perf-history` comment block
 * above each one. See CLAUDE.md § Benchmarks for the required table format.
 */

import { bench, describe } from 'vitest'

import { type Change, createWasmPipeline } from '~/wasm/bridge'
import { loadWasm } from '~/wasm/lifecycle'

import {
  BENCH_OPTIONS,
  buildState,
  createBarePipeline,
  createMultiPathListenerPipeline,
  createSyncPipeline,
} from './helpers'

// Load WASM before benchmarks run
await loadWasm()

describe('WASM Pipeline: Optimization Validation', () => {
  describe('Boundary crossing overhead (JS ↔ WASM)', () => {
    // Setup all pipelines outside benchmarks (once per test run)
    const minimalPipeline = createBarePipeline(10)

    const listenerPipeline = (() => {
      const p = createBarePipeline(10)
      p.registerSideEffects({
        registration_id: 'bench',
        listeners: [
          { subscriber_id: 0, topic_path: 'field_0', scope_path: '' },
        ],
      })
      return p
    })()

    const effectsPipeline = (() => {
      const state = buildState(10)
      state['sync_target'] = 'initial'
      state['flip_target'] = true
      const p = createWasmPipeline()
      p.shadowInit(state)
      p.registerSideEffects({
        registration_id: 'bench',
        sync_pairs: [['field_0', 'sync_target']],
        flip_pairs: [['bool_0', 'flip_target']],
        listeners: [
          { subscriber_id: 0, topic_path: 'field_0', scope_path: '' },
        ],
      })
      return p
    })()

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 806,850      | 4de0ee8 | baseline — measure JS→WASM serialization overhead |
     */
    bench(
      'processChanges call overhead (JS → WASM serialization)',
      () => {
        // Measure: Single minimal change, no effects registered
        // Includes: JSON serialization of changes, WASM boundary crossing
        // Baseline for: boundary crossing cost only
        const changes: Change[] = [{ path: 'field_0', value: 'updated' }]
        minimalPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 793,618      | 4de0ee8 | baseline — WASM→JS result serialization + listener dispatch |
     */
    bench(
      'returning results (WASM → JS + listener dispatch)',
      () => {
        // Measure: Change that produces listener output
        // Includes: Serialization of execution plan, listener dispatch
        // Cost of: returning results from WASM + JS execution plan
        const changes: Change[] = [{ path: 'field_0', value: 'listened' }]
        listenerPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 793,159      | 4de0ee8 | baseline — full round-trip with sync + flip + listeners |
     */
    bench(
      'round-trip latency (full JS ↔ WASM ↔ JS)',
      () => {
        // Measure: Complete round-trip with multiple effect types
        // Includes: Sync graph evaluation, flip graph evaluation, listener dispatch
        // Cost of: full pipeline orchestration including multiple boundary crossings
        const changes: Change[] = [{ path: 'field_0', value: 'round-trip' }]
        effectsPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })

  describe('Optimization validation', () => {
    // Setup: Many field accesses (1000 fields creates significant interning table)
    const manyFieldsPipeline = createBarePipeline(1000)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 794,448      | 4de0ee8 | baseline — path interning table lookups remain O(1) |
     */
    bench(
      'path interning efficiency (string → ID lookup)',
      () => {
        // Measure: Access different paths in large state
        // Validates: String intern lookups are O(1) cached (not O(n) searches)
        // Setup: 1000-field state means 1000 potential paths
        // Cost of: accessing path IDs is constant, not linear in field count
        const changes: Change[] = [{ path: 'field_500', value: 'accessed' }]
        manyFieldsPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    // Setup: 50 sync pairs for pre-computation validation
    const syncPipeline = createSyncPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 793,710      | 4de0ee8 | baseline — pre-computed graph lookup vs runtime evaluation |
     */
    bench(
      'pre-computed sync/flip graphs (lookup vs evaluation)',
      () => {
        // Measure: Sync graph traversal on pre-computed graph
        // Validates: Pre-computed graphs enable O(1) connected component lookup
        // Setup: 50 sync pairs means 50 possible sync relations
        // Cost of: graph lookup is fast (pre-built), not re-evaluated at runtime
        const changes: Change[] = [{ path: 'field_0', value: 'synced' }]
        syncPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    // Setup: 50 listeners on different paths for router pre-computation
    const routerPipeline = createMultiPathListenerPipeline(50)

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 793,120      | 4de0ee8 | baseline — pre-computed topic router avoids O(n) listener scanning |
     */
    bench(
      'topic router pre-computation (listener routing)',
      () => {
        // Measure: Topic router lookup for matching listeners
        // Validates: Pre-computed router enables O(1) listener matching
        // Setup: 50 listeners on different paths
        // Cost of: router lookup is fast (pre-built), not dynamic matching
        const changes: Change[] = [{ path: 'field_25', value: 'routed' }]
        routerPipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })
})
