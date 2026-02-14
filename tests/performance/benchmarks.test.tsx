/**
 * Performance Regression Tests — Calibrated Ratios
 *
 * Uses a machine-speed calibration unit (JSON parse/stringify cycles) so the
 * same ratio threshold passes on fast laptops and slow CI machines.
 *
 * Each test measures: elapsed / calibration < threshold
 *   - Fast laptop: calibration ~5ms  → absolute threshold ~0.1ms
 *   - Slow CI:     calibration ~50ms → absolute threshold ~1ms
 *   - Same ratio passes everywhere
 */

import { describe, expect, it } from 'vitest'
import { z } from 'zod'

import { createGenericStore } from '~'
import type { BoolLogic } from '~/types/boolLogic'
import { evaluateBoolLogic } from '~/utils/boolLogic'
import { dot } from '~/utils/dot'
import { _ } from '~/utils/hashKey'
import { interpolateTemplate } from '~/utils/interpolation'

import { typeHelpers } from '../mocks'
import { flush, renderWithStore } from '../utils/react'
import { calibrate, measure } from './calibrate'

// ---------------------------------------------------------------------------
// Shared state shape — deeply nested to stress-test path operations
// ---------------------------------------------------------------------------

interface BenchState {
  catalog: {
    departments: Record<
      string,
      {
        name: string
        products: Record<
          string,
          {
            status: string
            variants: Record<
              string,
              {
                price: number
                quantity: number
                metrics: { revenue: number; margin: number; turnover: number }
                shipping: {
                  weight: number
                  dimensions: {
                    length: number
                    reviews: Record<
                      string,
                      { rating: number; sentiment: number }
                    >
                  }
                }
              }
            >
          }
        >
      }
    >
  }
  isStocked: boolean
  needsReorder: boolean
  totalRevenue: number
  _errors: Record<string, string[]>
}

const makeBenchState = (): BenchState => ({
  catalog: {
    departments: {
      d1: {
        name: 'Electronics',
        products: {
          p1: {
            status: 'draft',
            variants: {
              v1: {
                price: 1.1,
                quantity: 1_000_000,
                metrics: { revenue: 0.55, margin: 0.03, turnover: 0.12 },
                shipping: {
                  weight: 1.1,
                  dimensions: {
                    length: 0.1,
                    reviews: {
                      r1: { rating: 1.08, sentiment: 0.12 },
                      r2: { rating: 1.1, sentiment: 0.1 },
                      r3: { rating: 1.15, sentiment: 0.11 },
                    },
                  },
                },
              },
              v2: {
                price: 1.1,
                quantity: 1_000_000,
                metrics: { revenue: -0.45, margin: 0.02, turnover: 0.11 },
                shipping: {
                  weight: 1.1,
                  dimensions: {
                    length: 0.1,
                    reviews: {
                      r1: { rating: 1.08, sentiment: 0.12 },
                      r2: { rating: 1.1, sentiment: 0.1 },
                      r3: { rating: 1.15, sentiment: 0.11 },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
  isStocked: true,
  needsReorder: false,
  totalRevenue: 0.1,
  _errors: {},
})

const DEEP_PATH =
  'catalog.departments.d1.products.p1.variants.v1.shipping.dimensions.reviews.r1.sentiment' as const
const MEDIUM_PATH =
  `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}.price` as const
const SHALLOW_PATH = 'isStocked' as const

// ---------------------------------------------------------------------------
// 1. deepGet — the most-called function in the library
// ---------------------------------------------------------------------------

describe('Benchmark: deepGet throughput', () => {
  it('shallow path (1 level) — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(() => dot.get__unsafe(state, SHALLOW_PATH), 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('medium path (7 levels) — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(() => dot.get__unsafe(state, MEDIUM_PATH), 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('deep path (12 levels) — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(() => dot.get__unsafe(state, DEEP_PATH), 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })
})

// ---------------------------------------------------------------------------
// 2. deepSet
// ---------------------------------------------------------------------------

describe('Benchmark: deepSet throughput', () => {
  it('shallow path — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    let i = 0
    const elapsed = measure(() => {
      dot.set__unsafe(state, SHALLOW_PATH, i++ % 2 === 0)
    }, 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('medium path — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    let i = 0
    const elapsed = measure(() => {
      dot.set__unsafe(state, MEDIUM_PATH, 1.1 + (i++ % 100) * 0.01)
    }, 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('deep path — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    let i = 0
    const elapsed = measure(() => {
      dot.set__unsafe(state, DEEP_PATH, 0.12 + (i++ % 100) * 0.001)
    }, 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })
})

// ---------------------------------------------------------------------------
// 3. evaluateBoolLogic — runs on every concern with conditions
// ---------------------------------------------------------------------------

describe('Benchmark: evaluateBoolLogic throughput', () => {
  it('simple IS_EQUAL — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(
      () => evaluateBoolLogic({ IS_EQUAL: ['isStocked', true] }, state),
      10_000,
    )
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('deep path IS_EQUAL — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(
      () => evaluateBoolLogic({ IS_EQUAL: [MEDIUM_PATH, 1.1] }, state),
      10_000,
    )
    expect(elapsed / cal).toBeLessThan(3.0)
  })

  it('complex AND/OR/NOT — 10k iterations', () => {
    const state = makeBenchState()
    const condition: BoolLogic<BenchState> = {
      AND: [
        {
          NOT: {
            IS_EQUAL: [
              `catalog.departments.${_('d1')}.products.${_('p1')}.status`,
              'draft',
            ],
          },
        },
        {
          OR: [
            {
              GT: [
                `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}.price`,
                2,
              ],
            },
            { IS_EQUAL: ['isStocked', false] },
            { EXISTS: 'totalRevenue' },
          ],
        },
        {
          LTE: [
            `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}.quantity`,
            5_000_000,
          ],
        },
      ],
    }

    const cal = calibrate()
    const elapsed = measure(() => evaluateBoolLogic(condition, state), 10_000)
    expect(elapsed / cal).toBeLessThan(3.0)
  })
})

// ---------------------------------------------------------------------------
// 4. interpolateTemplate — runs for dynamic labels/tooltips/placeholders
// ---------------------------------------------------------------------------

describe('Benchmark: interpolateTemplate throughput', () => {
  it('single placeholder — 10k iterations', () => {
    const state = makeBenchState()
    const cal = calibrate()
    const elapsed = measure(
      () =>
        interpolateTemplate(
          'Price: {{catalog.departments.d1.products.p1.variants.v1.price}}',
          state,
        ),
      10_000,
    )
    expect(elapsed / cal).toBeLessThan(5.0)
  })

  it('multiple placeholders — 10k iterations', () => {
    const state = makeBenchState()
    const template =
      'Price: {{catalog.departments.d1.products.p1.variants.v1.price}} | ' +
      'Weight: {{catalog.departments.d1.products.p1.variants.v1.shipping.weight}} | ' +
      'Revenue: {{catalog.departments.d1.products.p1.variants.v1.metrics.revenue}} | ' +
      'Length: {{catalog.departments.d1.products.p1.variants.v1.shipping.dimensions.length}}'

    const cal = calibrate()
    const elapsed = measure(() => interpolateTemplate(template, state), 10_000)
    expect(elapsed / cal).toBeLessThan(20.0)
  })
})

// ---------------------------------------------------------------------------
// 5. processChanges pipeline — full pipeline with/without side effects
// ---------------------------------------------------------------------------

describe('Benchmark: processChanges pipeline', () => {
  it('single change, no side effects — measures snapshot() cost', async () => {
    const store = createGenericStore<BenchState>()

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState())
    await flush()

    const cal = calibrate()
    const iterations = 100
    let total = 0

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
        'v1'
      ]!.price = 1.1 + i * 0.01
      await flush()
      total += performance.now() - start
    }

    expect(total / cal).toBeLessThan(5.0)
  })

  it('single change with sync + flip side effects', async () => {
    const store = createGenericStore<BenchState>()

    const SyncFlipBench = () => {
      store.useSideEffects('bench-effects', {
        syncPaths: [
          typeHelpers.syncPair<BenchState>(
            'catalog.departments.d1.products.p1.variants.v1.price',
            'catalog.departments.d1.products.p1.variants.v2.price',
          ),
        ],
        flipPaths: [
          typeHelpers.flipPair<BenchState>('isStocked', 'needsReorder'),
        ],
      })
      return null
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(
      <SyncFlipBench />,
      store,
      makeBenchState(),
    )
    await flush()

    const cal = calibrate()
    const iterations = 100
    let total = 0

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
        'v1'
      ]!.price = 1.1 + i * 0.01
      await flush()
      total += performance.now() - start
    }

    expect(total / cal).toBeLessThan(5.0)
  })

  it('single change with 10 concerns registered', async () => {
    const store = createGenericStore<BenchState>()

    const V1 =
      `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}` as const
    const V2 =
      `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v2')}` as const
    const disabledCondition: BoolLogic<BenchState> = {
      IS_EQUAL: [
        `catalog.departments.${_('d1')}.products.${_('p1')}.status`,
        'approved',
      ],
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState(), {
      concerns: {
        [`${V1}.price`]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: { condition: disabledCondition },
          dynamicTooltip: {
            template: `Price: {{${V1}.price}} | Weight: {{${V1}.shipping.weight}}`,
          },
        },
        [`${V1}.quantity`]: {
          validationState: { schema: z.number().positive() },
          disabledWhen: { condition: disabledCondition },
        },
        [`${V2}.price`]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: { condition: disabledCondition },
          dynamicTooltip: {
            template: `Price: {{${V2}.price}} | Weight: {{${V2}.shipping.weight}}`,
          },
        },
        [`${V2}.quantity`]: {
          validationState: { schema: z.number().positive() },
          disabledWhen: { condition: disabledCondition },
        },
      },
    })
    await flush()

    const cal = calibrate()
    const iterations = 100
    let total = 0

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
        'v1'
      ]!.price = 1.1 + i * 0.01
      await flush()
      total += performance.now() - start
    }

    expect(total / cal).toBeLessThan(100.0)
  })

  it('batch of 20 rapid mutations with sync side effects', async () => {
    const store = createGenericStore<BenchState>()

    const BatchSyncBench = () => {
      store.useSideEffects('bench-batch', {
        syncPaths: [
          typeHelpers.syncPair<BenchState>(
            'catalog.departments.d1.products.p1.variants.v1.price',
            'catalog.departments.d1.products.p1.variants.v2.price',
          ),
        ],
      })
      return null
    }

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(
      <BatchSyncBench />,
      store,
      makeBenchState(),
    )
    await flush()

    const cal = calibrate()
    const iterations = 50
    let total = 0

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      for (let j = 0; j < 20; j++) {
        storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
          'v1'
        ]!.metrics.revenue = 0.5 + j * 0.01 + i * 0.001
      }
      await flush()
      total += performance.now() - start
    }

    expect(total / cal).toBeLessThan(5.0)
  })
})

// ---------------------------------------------------------------------------
// 6. snapshot() cost — measures valtio snapshot overhead
// ---------------------------------------------------------------------------

describe('Benchmark: valtio snapshot() cost', () => {
  it('snapshot on deeply nested state — 1k iterations', async () => {
    const { snapshot } = await import('valtio')
    const { proxy } = await import('valtio/vanilla')

    const state = proxy(makeBenchState())

    const cal = calibrate()
    const elapsed = measure(() => {
      snapshot(state)
    }, 1_000)

    expect(elapsed / cal).toBeLessThan(5.0)
  })
})

// ---------------------------------------------------------------------------
// 7. Concern evaluation end-to-end — full round-trip
// ---------------------------------------------------------------------------

describe('Benchmark: concern evaluation round-trip', () => {
  it('change → concern re-evaluated — measures full reactive chain', async () => {
    const store = createGenericStore<BenchState>()

    const PRICE_PATH =
      `catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}.price` as const

    // eslint-disable-next-line no-restricted-syntax
    const { storeInstance } = renderWithStore(store, makeBenchState(), {
      concerns: {
        [PRICE_PATH]: {
          validationState: { schema: z.number().positive().max(10) },
          disabledWhen: {
            condition: {
              AND: [
                {
                  IS_EQUAL: [
                    `catalog.departments.${_('d1')}.products.${_('p1')}.status`,
                    'approved',
                  ],
                },
                { GT: [PRICE_PATH, 5] },
              ],
            },
          },
          dynamicTooltip: {
            template: `Price {{${PRICE_PATH}}} vs weight {{catalog.departments.${_('d1')}.products.${_('p1')}.variants.${_('v1')}.shipping.weight}}`,
          },
        },
      },
    })
    await flush()

    // Warm up
    storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
      'v1'
    ]!.price = 1.15
    await flush()

    const cal = calibrate()
    const iterations = 50
    let total = 0

    for (let i = 0; i < iterations; i++) {
      const start = performance.now()
      storeInstance.state.catalog.departments['d1']!.products['p1']!.variants[
        'v1'
      ]!.price = 1.1 + i * 0.01
      await flush()
      total += performance.now() - start

      // Verify concern updated
      const tooltip = storeInstance._concerns[PRICE_PATH]?.['dynamicTooltip']
      expect(tooltip).toContain('Price')
    }

    expect(total / cal).toBeLessThan(25.0)
  })
})
