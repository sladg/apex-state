/**
 * TEST: Aggregation excludeWhen Conditions
 *
 * Validates that aggregation pairs with an optional BoolLogic condition
 * correctly exclude sources from both read (source→target) and write
 * (target→source) directions.
 *
 * When excludeWhen evaluates to true, the source is excluded:
 * - Read: excluded source doesn't count toward the all-equal target computation
 * - Write: excluded source doesn't receive distributed writes from the target
 * - Condition change: changing a path used in excludeWhen triggers re-aggregation
 *
 * NOTE: registerSideEffects returns aggregation_changes but does NOT apply them
 * to shadow state. In a real store, JS applies them to valtio. In pipeline tests,
 * we check the returned aggregation_changes for initial values, then use
 * processChanges for runtime behavior.
 */

import { afterEach, beforeEach, describe, expect, it } from 'vitest'

import {
  type Change,
  createWasmPipeline,
  type WasmPipeline,
} from '../../src/wasm/bridge'

/** Helper: find change by path */
const findChange = (changes: Change[], path: string) =>
  changes.find((c) => c.path === path)

/** Helper: get all paths from changes */
const getPaths = (changes: Change[]) => changes.map((c) => c.path)

describe('Aggregation excludeWhen: Read direction (source → target)', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: item1=true, item2=false, item3=true, item2_disabled=true
  // Aggregation: target="allChecked", sources=["item1", "item2" (excluded when item2_disabled=true), "item3"]
  //
  // Expected: item2 excluded → only item1 and item3 considered → both true → allChecked = true
  it('should exclude a source from initial read when condition is true', () => {
    pipeline.shadowInit({
      allChecked: null,
      item1: true,
      item2: false,
      item3: true,
      item2_disabled: true,
    })
    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allChecked', 'item1'],
        [
          'allChecked',
          'item2',
          JSON.stringify({ IS_EQUAL: ['item2_disabled', true] }),
        ],
        ['allChecked', 'item3'],
      ],
    })

    // Initial aggregation_changes: item2 excluded → item1 & item3 both true → allChecked = true
    const change = findChange(result.aggregation_changes, 'allChecked')
    expect(change).toBeDefined()
    expect(change?.value).toBe(true)
  })

  // Setup: item1=true, item2=false, item3=true, item2_disabled=false
  // All sources active, values differ → allChecked = undefined
  //
  // Action: set item2_disabled = true (now item2 excluded)
  // Expected: re-aggregation → item1 and item3 both true → allChecked = true
  it('should re-aggregate when condition path changes', () => {
    pipeline.shadowInit({
      allChecked: null,
      item1: true,
      item2: false,
      item3: true,
      item2_disabled: false,
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allChecked', 'item1'],
        [
          'allChecked',
          'item2',
          JSON.stringify({ IS_EQUAL: ['item2_disabled', true] }),
        ],
        ['allChecked', 'item3'],
      ],
    })

    // Initial: all active, values differ → allChecked = undefined (sentinel)
    const initChange = findChange(regResult.aggregation_changes, 'allChecked')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBeUndefined()

    // Now disable item2 by changing the condition path
    // Note: shadow still has allChecked=null (initial registration changes
    // are returned, not applied to shadow — JS store applies them to valtio)
    const result = pipeline.processChanges([
      { path: 'item2_disabled', value: true },
    ])

    // allChecked should be recomputed: item2 now excluded → item1, item3 both true → true
    const allCheckedChange = findChange(result.state_changes, 'allChecked')
    expect(allCheckedChange).toBeDefined()
    expect(allCheckedChange?.value).toBe(true)
  })

  // Setup: item1=true, item2=true, item1_disabled=false, item2_disabled=false
  // All sources active, all same → allChecked = true
  //
  // Action: disable both sources
  // Expected: all excluded → allChecked = undefined (sentinel)
  it('should set target to undefined when all sources are excluded', () => {
    pipeline.shadowInit({
      allChecked: null,
      item1: true,
      item2: true,
      item1_disabled: false,
      item2_disabled: false,
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        [
          'allChecked',
          'item1',
          JSON.stringify({ IS_EQUAL: ['item1_disabled', true] }),
        ],
        [
          'allChecked',
          'item2',
          JSON.stringify({ IS_EQUAL: ['item2_disabled', true] }),
        ],
      ],
    })

    // Initial: both active, both true → allChecked = true
    const initChange = findChange(regResult.aggregation_changes, 'allChecked')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBe(true)

    // Disable both sources
    const result = pipeline.processChanges([
      { path: 'item1_disabled', value: true },
      { path: 'item2_disabled', value: true },
    ])

    // All excluded → target = undefined
    const allCheckedChange = findChange(result.state_changes, 'allChecked')
    expect(allCheckedChange).toBeDefined()
    expect(allCheckedChange?.value).toBeUndefined()
  })

  // Setup: item1=10, item2=20, item2_disabled=true
  // item2 excluded → only item1 → allValues = 10
  //
  // Action: re-enable item2 (item2_disabled = false)
  // Expected: item2 now active, values differ → allValues = undefined
  it('should re-include source when condition becomes false', () => {
    pipeline.shadowInit({
      allValues: null,
      item1: 10,
      item2: 20,
      item2_disabled: true,
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allValues', 'item1'],
        [
          'allValues',
          'item2',
          JSON.stringify({ IS_EQUAL: ['item2_disabled', true] }),
        ],
      ],
    })

    // Initial: item2 excluded → only item1 (10) → allValues = 10
    const initChange = findChange(regResult.aggregation_changes, 'allValues')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBe(10)

    // Re-enable item2
    const result = pipeline.processChanges([
      { path: 'item2_disabled', value: false },
    ])

    // item2 now active, values differ (10 vs 20) → allValues = undefined
    const change = findChange(result.state_changes, 'allValues')
    expect(change).toBeDefined()
    expect(change?.value).toBeUndefined()
  })

  // Backward compatibility: pairs without condition work the same as before
  it('should work normally for pairs without excludeWhen condition', () => {
    pipeline.shadowInit({
      allChecked: null,
      item1: true,
      item2: true,
      item3: true,
    })
    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allChecked', 'item1'],
        ['allChecked', 'item2'],
        ['allChecked', 'item3'],
      ],
    })

    // All same → allChecked = true
    const change = findChange(result.aggregation_changes, 'allChecked')
    expect(change).toBeDefined()
    expect(change?.value).toBe(true)
  })

  // Mix: some pairs with condition, some without
  it('should handle mixed pairs (with and without conditions)', () => {
    pipeline.shadowInit({
      total: null,
      price1: 50,
      price2: 50,
      price3: 99,
      price3_excluded: true,
    })
    const result = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['total', 'price1'],
        ['total', 'price2'],
        [
          'total',
          'price3',
          JSON.stringify({ IS_EQUAL: ['price3_excluded', true] }),
        ],
      ],
    })

    // price3 excluded → price1 and price2 both 50 → total = 50
    const change = findChange(result.aggregation_changes, 'total')
    expect(change).toBeDefined()
    expect(change?.value).toBe(50)
  })
})

describe('Aggregation excludeWhen: Write direction (target → sources)', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // Setup: user1_disabled=true
  // Aggregation: target="allUsers", sources=["user1" (excluded), "user2"]
  //
  // Action: setValue("allUsers", "alice")
  // Expected: user1 excluded → only user2 receives "alice"
  it('should not distribute writes to excluded sources', () => {
    pipeline.shadowInit({
      allUsers: null,
      user1: null,
      user2: null,
      user1_disabled: true,
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        [
          'allUsers',
          'user1',
          JSON.stringify({ IS_EQUAL: ['user1_disabled', true] }),
        ],
        ['allUsers', 'user2'],
      ],
    })

    const result = pipeline.processChanges([
      { path: 'allUsers', value: 'alice' },
    ])

    const paths = getPaths(result.state_changes)

    // user2 should receive the distributed write
    expect(paths).toContain('user2')
    const user2Change = findChange(result.state_changes, 'user2')
    expect(user2Change?.value).toBe('alice')

    // user1 should NOT receive the write (excluded)
    expect(paths).not.toContain('user1')
  })

  // All sources active → all receive writes (normal behavior)
  it('should distribute writes to all sources when no conditions exclude', () => {
    pipeline.shadowInit({
      allUsers: null,
      user1: null,
      user2: null,
      user1_disabled: false,
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        [
          'allUsers',
          'user1',
          JSON.stringify({ IS_EQUAL: ['user1_disabled', true] }),
        ],
        ['allUsers', 'user2'],
      ],
    })

    const result = pipeline.processChanges([{ path: 'allUsers', value: 'bob' }])

    const paths = getPaths(result.state_changes)

    // Both should receive writes (user1_disabled is false)
    expect(paths).toContain('user1')
    expect(paths).toContain('user2')
  })

  // Write to child path of aggregation target with excluded source
  it('should not distribute child path writes to excluded sources', () => {
    pipeline.shadowInit({
      allUsers: { name: null },
      user1: { name: null },
      user2: { name: null },
      user1_disabled: true,
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        [
          'allUsers',
          'user1',
          JSON.stringify({ IS_EQUAL: ['user1_disabled', true] }),
        ],
        ['allUsers', 'user2'],
      ],
    })

    const result = pipeline.processChanges([
      { path: 'allUsers.name', value: 'alice' },
    ])

    const paths = getPaths(result.state_changes)

    // user2.name should receive the write
    expect(paths).toContain('user2.name')
    // user1.name should NOT (excluded)
    expect(paths).not.toContain('user1.name')
  })
})

describe('Aggregation excludeWhen: Complex conditions', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // AND condition: exclude only when BOTH flags are true
  it('should support AND conditions', () => {
    pipeline.shadowInit({
      total: null,
      price1: 100,
      price2: 100,
      hidden: true,
      archived: false,
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['total', 'price1'],
        [
          'total',
          'price2',
          JSON.stringify({
            AND: [
              { IS_EQUAL: ['hidden', true] },
              { IS_EQUAL: ['archived', true] },
            ],
          }),
        ],
      ],
    })

    // Only hidden=true, archived=false → AND is false → price2 NOT excluded
    // Both active, both 100 → total = 100
    const initChange = findChange(regResult.aggregation_changes, 'total')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBe(100)

    // Now set archived=true → AND is true → price2 excluded
    // total is still 100 because only price1 remains (also 100) — same value
    const result = pipeline.processChanges([{ path: 'archived', value: true }])

    // Re-aggregation fires (condition changed), result is still 100
    // Shadow had null → 100 is a genuine change
    const totalChange = findChange(result.state_changes, 'total')
    expect(totalChange).toBeDefined()
    expect(totalChange?.value).toBe(100)
  })

  // NOT condition: exclude when NOT exists
  it('should support NOT conditions', () => {
    pipeline.shadowInit({
      result: null,
      val1: 42,
      val2: 42,
      val2_active: true,
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['result', 'val1'],
        ['result', 'val2', JSON.stringify({ NOT: { EXISTS: 'val2_active' } })],
      ],
    })

    // val2_active exists → NOT EXISTS = false → val2 NOT excluded
    // Both 42 → result = 42
    const initChange = findChange(regResult.aggregation_changes, 'result')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBe(42)

    // Remove val2_active (set to null) → NOT EXISTS = true → val2 excluded
    // Only val1 (42) active → result = 42
    const result = pipeline.processChanges([
      { path: 'val2_active', value: null },
    ])

    // Re-aggregation fires (condition path changed), result is 42
    // Shadow had null → 42 is a genuine change (initial wasn't applied to shadow)
    const resultChange = findChange(result.state_changes, 'result')
    expect(resultChange).toBeDefined()
    expect(resultChange?.value).toBe(42)
  })
})

describe('Aggregation excludeWhen: Realistic product scenarios', () => {
  let pipeline: WasmPipeline
  beforeEach(() => {
    pipeline = createWasmPipeline()
  })
  afterEach(() => {
    pipeline.destroy()
  })

  // 5 products, 2 excluded. The 3 active ones share the same price.
  // Aggregation target should show the common value of the 3 active products.
  //
  // Setup:
  //   product1.price=25, product2.price=25, product3.price=25 (active)
  //   product4.price=99, product5.price=50 (excluded via disabled=true)
  //   allPrice target = null initially
  //
  // Expected after registration: allPrice = 25 (common value of 3 active products)
  it('should show common value when excluded products have different values', () => {
    pipeline.shadowInit({
      allPrice: null,
      product1: { price: 25 },
      product2: { price: 25 },
      product3: { price: 25 },
      product4: { price: 99, disabled: true },
      product5: { price: 50, disabled: true },
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allPrice', 'product1.price'],
        ['allPrice', 'product2.price'],
        ['allPrice', 'product3.price'],
        [
          'allPrice',
          'product4.price',
          JSON.stringify({ IS_EQUAL: ['product4.disabled', true] }),
        ],
        [
          'allPrice',
          'product5.price',
          JSON.stringify({ IS_EQUAL: ['product5.disabled', true] }),
        ],
      ],
    })

    // 3 active products all have price=25 → allPrice = 25
    const change = findChange(regResult.aggregation_changes, 'allPrice')
    expect(change).toBeDefined()
    expect(change?.value).toBe(25)
  })

  // Same 5-product setup as above.
  // When 1 of the 2 excluded products gets re-enabled, aggregation should
  // re-evaluate. Since the re-enabled product has a different price (99),
  // sources now disagree → allPrice = undefined.
  //
  // Action: product4.disabled = false (product4.price=99 now active)
  // Expected: 4 active products [25, 25, 25, 99] → disagree → allPrice = undefined
  it('should re-evaluate when a previously excluded product becomes active', () => {
    pipeline.shadowInit({
      allPrice: null,
      product1: { price: 25 },
      product2: { price: 25 },
      product3: { price: 25 },
      product4: { price: 99, disabled: true },
      product5: { price: 50, disabled: true },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allPrice', 'product1.price'],
        ['allPrice', 'product2.price'],
        ['allPrice', 'product3.price'],
        [
          'allPrice',
          'product4.price',
          JSON.stringify({ IS_EQUAL: ['product4.disabled', true] }),
        ],
        [
          'allPrice',
          'product5.price',
          JSON.stringify({ IS_EQUAL: ['product5.disabled', true] }),
        ],
      ],
    })

    // Re-enable product4 (price=99, now active)
    const result = pipeline.processChanges([
      { path: 'product4.disabled', value: false },
    ])

    // 4 active: [25, 25, 25, 99] → disagree → allPrice = undefined
    const change = findChange(result.state_changes, 'allPrice')
    expect(change).toBeDefined()
    expect(change?.value).toBeUndefined()
  })

  // Same 5-product setup, but re-enabled product matches the common value.
  // When product4 is re-enabled AND its price matches the others,
  // aggregation should still show the common value.
  //
  // Action: first set product4.price=25, then re-enable it
  // Expected: 4 active products [25, 25, 25, 25] → agree → allPrice = 25
  it('should keep common value when re-enabled product matches', () => {
    pipeline.shadowInit({
      allPrice: null,
      product1: { price: 25 },
      product2: { price: 25 },
      product3: { price: 25 },
      product4: { price: 99, disabled: true },
      product5: { price: 50, disabled: true },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allPrice', 'product1.price'],
        ['allPrice', 'product2.price'],
        ['allPrice', 'product3.price'],
        [
          'allPrice',
          'product4.price',
          JSON.stringify({ IS_EQUAL: ['product4.disabled', true] }),
        ],
        [
          'allPrice',
          'product5.price',
          JSON.stringify({ IS_EQUAL: ['product5.disabled', true] }),
        ],
      ],
    })

    // Update product4's price to match while still excluded, then re-enable
    // Note: changing product4.price triggers re-aggregation for the 3 active sources,
    // which sets allPrice=25 in shadow (product4 is still excluded at this point)
    pipeline.processChanges([{ path: 'product4.price', value: 25 }])
    const result = pipeline.processChanges([
      { path: 'product4.disabled', value: false },
    ])

    // 4 active: [25, 25, 25, 25] → agree → allPrice = 25
    // Shadow already has allPrice=25 from the first processChanges, so this is a no-op
    // (no redundant change emitted — correct optimization)
    const change = findChange(result.state_changes, 'allPrice')
    expect(change).toBeUndefined()

    // Verify the shadow state is correct
    const dump = pipeline.shadowDump() as Record<string, unknown>
    expect(dump['allPrice']).toBe(25)
  })

  // 1 product is deviating from the rest. Aggregation target is blank (undefined)
  // because sources disagree. When the deviant product is excluded, the remaining
  // products all agree → target becomes the common value.
  //
  // Setup:
  //   product1.price=25, product2.price=25, product3.price=25 (agree)
  //   product4.price=99 (deviating, initially active, disabled=false)
  //
  // Initial: [25, 25, 25, 99] → disagree → allPrice = undefined
  // Action: product4.disabled = true → product4 excluded
  // Expected: [25, 25, 25] → agree → allPrice = 25
  it('should resolve to common value when deviating product is excluded', () => {
    pipeline.shadowInit({
      allPrice: null,
      product1: { price: 25 },
      product2: { price: 25 },
      product3: { price: 25 },
      product4: { price: 99, disabled: false },
    })
    const regResult = pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allPrice', 'product1.price'],
        ['allPrice', 'product2.price'],
        ['allPrice', 'product3.price'],
        [
          'allPrice',
          'product4.price',
          JSON.stringify({ IS_EQUAL: ['product4.disabled', true] }),
        ],
      ],
    })

    // Initial: all active, [25, 25, 25, 99] → disagree → allPrice = undefined
    const initChange = findChange(regResult.aggregation_changes, 'allPrice')
    expect(initChange).toBeDefined()
    expect(initChange?.value).toBeUndefined()

    // Exclude the deviating product
    const result = pipeline.processChanges([
      { path: 'product4.disabled', value: true },
    ])

    // product4 now excluded → [25, 25, 25] → agree → allPrice = 25
    const change = findChange(result.state_changes, 'allPrice')
    expect(change).toBeDefined()
    expect(change?.value).toBe(25)
  })

  // Reverse of above: aggregation shows common value, then deviating product
  // gets re-included → aggregation becomes blank.
  //
  // Setup:
  //   product1.price=25, product2.price=25, product3.price=25 (agree)
  //   product4.price=99 (deviating, excluded via disabled=true)
  //
  // Initial: product4 excluded → [25, 25, 25] → allPrice = 25
  // Action: product4.disabled = false → product4 active
  // Expected: [25, 25, 25, 99] → disagree → allPrice = undefined
  it('should become blank when deviating product is re-included', () => {
    pipeline.shadowInit({
      allPrice: null,
      product1: { price: 25 },
      product2: { price: 25 },
      product3: { price: 25 },
      product4: { price: 99, disabled: true },
    })
    pipeline.registerSideEffects({
      registration_id: 'test',
      aggregation_pairs: [
        ['allPrice', 'product1.price'],
        ['allPrice', 'product2.price'],
        ['allPrice', 'product3.price'],
        [
          'allPrice',
          'product4.price',
          JSON.stringify({ IS_EQUAL: ['product4.disabled', true] }),
        ],
      ],
    })

    // Re-include the deviating product
    const result = pipeline.processChanges([
      { path: 'product4.disabled', value: false },
    ])

    // product4 now active → [25, 25, 25, 99] → disagree → allPrice = undefined
    const change = findChange(result.state_changes, 'allPrice')
    expect(change).toBeDefined()
    expect(change?.value).toBeUndefined()
  })
})
