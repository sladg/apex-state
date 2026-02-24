/**
 * TEST: Computation SUM/AVG Operations
 *
 * Validates that computation pairs correctly compute numeric reduction operations
 * (SUM, AVG) in a unidirectional source → target flow.
 *
 * Key behaviors:
 * - SUM: sum of all active source values (0 if all excluded)
 * - AVG: average of all active source values (undefined if all excluded)
 * - Writes to computation target are silently filtered (no-op)
 * - excludeWhen condition skips sources from computation
 * - Source changes trigger recomputation
 * - Initial registration computes correct values
 * - Works alongside aggregations in same useSideEffects call
 */

import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import {
  type Change,
  createWasmPipeline,
  type WasmPipeline,
} from '~/wasm/bridge'

/** Helper: find change by path */
const findChange = (changes: Change[], path: string) =>
  changes.find((c) => c.path === path)

describe('Computation SUM', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: price1=10, price2=20, price3=30, total=0
  // SUM target="total", sources=["price1", "price2", "price3"]
  // Expected initial: total = 60
  it('should compute initial SUM from sources', () => {
    // Initialize shadow state with source values
    pipeline.shadowInit({
      total: 0,
      price1: 10,
      price2: 20,
      price3: 30,
    })

    // Register computation with SUM operation
    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        ['SUM', 'total', 'price2'],
        ['SUM', 'total', 'price3'],
      ],
    })

    // Initial computation_changes: total = 10 + 20 + 30 = 60
    const change = findChange(result.computation_changes, 'total')
    expect(change).toBeDefined()
    expect(change?.value).toBe(60)
  })

  // After registration, changing a source should trigger recomputation
  it('should recompute SUM when a source changes', () => {
    // Initialize with sources
    pipeline.shadowInit({
      total: 0,
      price1: 10,
      price2: 20,
      price3: 30,
    })

    // Register computations
    pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        ['SUM', 'total', 'price2'],
        ['SUM', 'total', 'price3'],
      ],
    })

    // Change price2 from 20 to 50
    const result = pipeline.processChanges([{ path: 'price2', value: 50 }])

    // Should see both price2 change and total recomputation
    const totalChange = findChange(result.state_changes, 'total')
    expect(totalChange).toBeDefined()
    expect(totalChange?.value).toBe(90) // 10 + 50 + 30
  })
})

describe('Computation AVG', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: score1=10, score2=20, score3=30, average=0
  // AVG target="average", sources=["score1", "score2", "score3"]
  // Expected initial: average = 20
  it('should compute initial AVG from sources', () => {
    pipeline.shadowInit({
      average: 0,
      score1: 10,
      score2: 20,
      score3: 30,
    })

    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['AVG', 'average', 'score1'],
        ['AVG', 'average', 'score2'],
        ['AVG', 'average', 'score3'],
      ],
    })

    const change = findChange(result.computation_changes, 'average')
    expect(change).toBeDefined()
    expect(change?.value).toBe(20)
  })

  // After registration, changing a source should trigger recomputation
  it('should recompute AVG when a source changes', () => {
    pipeline.shadowInit({
      average: 0,
      score1: 10,
      score2: 20,
      score3: 30,
    })

    pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['AVG', 'average', 'score1'],
        ['AVG', 'average', 'score2'],
        ['AVG', 'average', 'score3'],
      ],
    })

    // Change score2 from 20 to 50
    const result = pipeline.processChanges([{ path: 'score2', value: 50 }])

    const avgChange = findChange(result.state_changes, 'average')
    expect(avgChange).toBeDefined()
    expect(avgChange?.value).toBe(30) // (10 + 50 + 30) / 3 = 30
  })
})

describe('Computation: write to target is no-op', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  it('should silently filter writes to computation target', () => {
    pipeline.shadowInit({
      total: 0,
      price1: 10,
      price2: 20,
    })

    pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        ['SUM', 'total', 'price2'],
      ],
    })

    // Try to write directly to the computation target
    const result = pipeline.processChanges([{ path: 'total', value: 999 }])

    // The write to 'total' should be filtered out (no-op)
    const totalChange = findChange(result.state_changes, 'total')
    expect(totalChange).toBeUndefined()
  })
})

describe('Computation: excludeWhen conditions', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // When excludeWhen is true for a source, that source is skipped
  it('should exclude source when condition is true', () => {
    pipeline.shadowInit({
      total: 0,
      price1: 10,
      price2: 20,
      price2_disabled: true,
    })

    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        [
          'SUM',
          'total',
          'price2',
          JSON.stringify({ IS_EQUAL: ['price2_disabled', true] }),
        ],
      ],
    })

    // price2 excluded → total = 10 only
    const change = findChange(result.computation_changes, 'total')
    expect(change).toBeDefined()
    expect(change?.value).toBe(10)
  })

  // Changing the condition path should trigger recomputation
  it('should recompute when condition path changes', () => {
    pipeline.shadowInit({
      total: 0,
      price1: 10,
      price2: 20,
      price2_disabled: true,
    })

    pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        [
          'SUM',
          'total',
          'price2',
          JSON.stringify({ IS_EQUAL: ['price2_disabled', true] }),
        ],
      ],
    })

    // Enable price2 by changing condition
    const result = pipeline.processChanges([
      { path: 'price2_disabled', value: false },
    ])

    const totalChange = findChange(result.state_changes, 'total')
    expect(totalChange).toBeDefined()
    expect(totalChange?.value).toBe(30) // 10 + 20
  })
})

describe('Computation: edge cases', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // SUM: all sources excluded → 0
  it('SUM with all sources excluded should be 0', () => {
    pipeline.shadowInit({
      total: 50,
      price1: 10,
      disabled: true,
    })

    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        [
          'SUM',
          'total',
          'price1',
          JSON.stringify({ IS_EQUAL: ['disabled', true] }),
        ],
      ],
    })

    const change = findChange(result.computation_changes, 'total')
    expect(change).toBeDefined()
    expect(change?.value).toBe(0)
  })

  // AVG: all sources excluded → undefined
  it('AVG with all sources excluded should be undefined', () => {
    pipeline.shadowInit({
      average: 50,
      score1: 10,
      disabled: true,
    })

    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      computation_pairs: [
        [
          'AVG',
          'average',
          'score1',
          JSON.stringify({ IS_EQUAL: ['disabled', true] }),
        ],
      ],
    })

    const change = findChange(result.computation_changes, 'average')
    expect(change).toBeDefined()
    expect(change?.value).toBeUndefined()
  })
})

describe('Computation: mixed with aggregations', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Both aggregations and computations in same registration
  it('should work alongside aggregations in same useSideEffects call', () => {
    pipeline.shadowInit({
      allChecked: null,
      item1: true,
      item2: true,
      total: 0,
      price1: 10,
      price2: 20,
    })

    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allChecked', 'item1'],
        ['allChecked', 'item2'],
      ],
      computation_pairs: [
        ['SUM', 'total', 'price1'],
        ['SUM', 'total', 'price2'],
      ],
    })

    // Aggregation: allChecked = true (both items are true)
    const aggChange = findChange(result.aggregation_changes, 'allChecked')
    expect(aggChange).toBeDefined()
    expect(aggChange?.value).toBe(true)

    // Computation: total = 30
    const compChange = findChange(result.computation_changes, 'total')
    expect(compChange).toBeDefined()
    expect(compChange?.value).toBe(30)
  })
})
