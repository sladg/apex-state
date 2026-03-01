/**
 * Benchmarks: End-to-End Real-World Scenarios (USER EXPERIENCE)
 *
 * Measures complete pipeline performance in realistic workflows:
 * - Form field changes with validation
 * - E-commerce checkout workflows (syncs + flips + listeners)
 * - Dashboard metric updates with cascading aggregations
 *
 * These benchmarks simulate what users actually experience.
 * Focus: should feel instant (< 5ms for complex workflows).
 *
 * NOTE: When implementing bench() calls, add a `@perf-history` comment block
 * above each one. See CLAUDE.md § Benchmarks for the required table format.
 */

import { bench, describe } from 'vitest'

import { type Change, createWasmPipeline } from '~/wasm/bridge'
import { loadWasm } from '~/wasm/lifecycle'

import { BENCH_OPTIONS } from './helpers'

// Load WASM before benchmarks run
await loadWasm()

describe('WASM Pipeline: Real-World Scenarios', () => {
  describe('End-to-end pipeline scenarios', () => {
    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 37,236       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 32,218       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'typical form field change (validation + listener)',
      () => {
        // Setup: Realistic form state
        const formState: Record<string, unknown> = {
          email: 'user@example.com',
          name: 'John Doe',
          age: 30,
        }
        const pipeline = createWasmPipeline()
        pipeline.shadowInit(formState)

        // Register BoolLogic concern for disabledWhen
        pipeline.registerConcerns({
          registration_id: 'form-bench',
          bool_logics: [
            {
              output_path: 'email.disabledWhen',
              tree_json: JSON.stringify({ IS_EQUAL: ['email', ''] }),
            },
          ],
        })

        // Register listener for email field changes
        pipeline.registerSideEffects({
          registration_id: 'form-bench',
          listeners: [
            {
              subscriber_id: 0,
              topic_paths: ['email'],
              scope_path: '',
            },
          ],
        })

        // Measure: processChanges for typical user input
        const changes: Change[] = [
          { path: 'email', value: 'newemail@example.com', meta: {} },
        ]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 26,366       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 27,495       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'checkout workflow (syncs + flips + listeners)',
      () => {
        // Setup: E-commerce checkout with product/shipping/billing
        const checkoutState: Record<string, unknown> = {
          productPrice: 99.99,
          quantity: 1,
          shippingCost: 10.0,
          billingAddress: '123 Main St',
          sameAsShipping: true,
          shippingAddress: '123 Main St',
          isEligibleForFreeShip: false,
          orderTotal: 109.99,
        }
        const pipeline = createWasmPipeline()
        pipeline.shadowInit(checkoutState)

        // Register sync pairs (shipping address syncs to billing if sameAsShipping)
        // and aggregation for total (price * qty + shipping)
        pipeline.registerSideEffects({
          registration_id: 'checkout-bench',
          sync_pairs: [['shippingAddress', 'billingAddress']],
          flip_pairs: [['sameAsShipping', 'isEligibleForFreeShip']],
          listeners: [
            {
              subscriber_id: 0,
              topic_paths: ['productPrice'],
              scope_path: '',
            },
          ],
        })

        // Measure: Total pipeline time for field change that triggers sync, flip, listener
        const changes: Change[] = [
          { path: 'productPrice', value: 149.99, meta: {} },
        ]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )

    /**
     * @perf-history
     * Hardware: Apple M4 Pro
     * | Date       | Hz (ops/sec) | Commit  | Note                          |
     * |------------|--------------|---------|-------------------------------|
     * | 2026-02-22 | 18,841       | 4de0ee8 | baseline — initial measurement |
     * | 2026-02-25 | 18,808       | aa7e7da | simplified compute_sync_initial_changes; delegate no-op filter to diff_changes |
     */
    bench(
      'dashboard metric update (aggregates + cascades)',
      () => {
        // Setup: Dashboard state with source metrics and aggregation targets
        const dashboardState: Record<string, unknown> = {
          metric_views: 1000,
          metric_clicks: 150,
          metric_conversions: 25,
          total_value: 1175,
          avg_value: 0,
        }
        const pipeline = createWasmPipeline()
        pipeline.shadowInit(dashboardState)

        // Register aggregation pairs: compute totals from sources
        pipeline.registerSideEffects({
          registration_id: 'dashboard-bench',
          aggregation_pairs: [
            ['total_value', 'metric_views'],
            ['total_value', 'metric_clicks'],
            ['total_value', 'metric_conversions'],
          ],
        })

        // Measure: Time for pipeline to update aggregated metrics
        const changes: Change[] = [
          { path: 'metric_views', value: 2000, meta: {} },
        ]
        pipeline.processChanges(changes)
      },
      BENCH_OPTIONS,
    )
  })
})
