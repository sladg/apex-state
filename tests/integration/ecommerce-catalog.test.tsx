/**
 * Integration Tests: E-commerce Product Catalog Form
 *
 * Scenario: Deeply nested (15 levels) e-commerce catalog management form
 * with custom concerns, all built-in concerns, side effects (sync, flip,
 * listeners, aggregations), and complex BoolLogic conditions.
 *
 * Models a realistic e-commerce operations center where:
 * - Multiple product variants each contain analytics, pricing data, and promotion config
 * - Validation depends on product type, market conditions, and cross-variant relationships
 * - Fields are conditionally visible/disabled/readonly based on deep state
 * - Listeners compute derived values (revenue, inventory adjustments) from variant changes
 * - Custom concerns handle domain-specific logic (budget check, spending limits)
 */

import { useLayoutEffect } from 'react'

import { screen } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import {
  createGenericStore,
  defaultConcerns,
  registerListener,
} from '../../src'
import type { ConcernType } from '../../src/concerns/types'
import { useStoreContext } from '../../src/core/context'
import { dot } from '../../src/utils/dot'
import { _ } from '../../src/utils/hashKey'
import { typeHelpers } from '../mocks'
import { fireEvent, flushEffects, renderWithStore } from '../utils/react'

// ---------------------------------------------------------------------------
// 15-level deep type hierarchy
// ---------------------------------------------------------------------------

interface BarrierScheduleEntry {
  date: string
  level: number
  observed: boolean
}

interface BarrierConfig {
  type: 'knockout' | 'knockin' | 'none'
  direction: 'up' | 'down'
  level: number
  schedule: Record<string, BarrierScheduleEntry>
}

interface GreeksSnapshot {
  delta: number
  gamma: number
  vega: number
  theta: number
  rho: number
}

interface VolSurface {
  atmVol: number
  skew: number
  kurtosis: number
  smile: Record<
    string,
    {
      strike: number
      vol: number
      source: 'market' | 'model'
    }
  >
}

interface MarketData {
  spot: number
  forward: number
  domesticRate: number
  foreignRate: number
  volSurface: VolSurface
}

interface LegData {
  direction: 'buy' | 'sell'
  notional: number
  strike: number
  expiry: string
  optionType: 'call' | 'put'
  exerciseStyle: 'european' | 'american'
  premium: number
  premiumCurrency: 'USD' | 'EUR' | 'GBP' | 'JPY'
  barrier: BarrierConfig
  greeks: GreeksSnapshot
  marketData: MarketData
  isLocked: boolean
}

interface LegGroup {
  strategy: 'straddle' | 'strangle' | 'riskReversal' | 'butterfly' | 'vanilla'
  legs: Record<string, LegData>
  groupDelta: number
  groupNotional: number
}

interface Product {
  ccyPair: string
  valueDate: string
  tradeDate: string
  status: 'draft' | 'pending' | 'approved' | 'rejected' | 'executed'
  trader: string
  counterparty: string
  legGroups: Record<string, LegGroup>
}

// Top-level: portfolio → books → products → legGroups → legs → barrier → schedule → entries
// That gives us: portfolio.books.{id}.products.{id}.legGroups.{id}.legs.{id}.barrier.schedule.{id}.level
// Count: portfolio(1).books(2).{id}(3).products(4).{id}(5).legGroups(6).{id}(7).legs(8).{id}(9).barrier(10).schedule(11).{id}(12).level(13)
// Plus: ...legs.{id}.marketData(10).volSurface(11).smile(12).{id}(13).vol(14) → deeper still
// And: ...greeks(10).delta(11), etc.
// Total reachable depth: 15 via portfolio.books.b1.products.p1.legGroups.g1.legs.l1.marketData.volSurface.smile.s25.source
interface EcommerceCatalog {
  portfolio: {
    books: Record<
      string,
      {
        name: string
        desk: string
        riskLimit: number
        products: Record<string, Product>
      }
    >
  }
  globalMarket: {
    regime: 'normal' | 'stressed' | 'crisis'
    lastUpdate: number
  }
  aggregatedDelta: number
  aggregatedGamma: number
  isHedged: boolean
  needsRebalance: boolean
  _errors: Record<string, string[]>
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const defaultBarrierSchedule: Record<string, BarrierScheduleEntry> = {
  'obs-1': { date: '2025-03-15', level: 1.12, observed: false },
  'obs-2': { date: '2025-06-15', level: 1.13, observed: false },
}

const defaultGreeks: GreeksSnapshot = {
  delta: 0.55,
  gamma: 0.03,
  vega: 0.12,
  theta: -0.04,
  rho: 0.01,
}

const defaultVolSurface: VolSurface = {
  atmVol: 0.1,
  skew: -0.02,
  kurtosis: 0.005,
  smile: {
    s25p: { strike: 1.08, vol: 0.12, source: 'market' },
    s25c: { strike: 1.15, vol: 0.11, source: 'market' },
    atm: { strike: 1.1, vol: 0.1, source: 'model' },
  },
}

const defaultMarketData: MarketData = {
  spot: 1.1,
  forward: 1.105,
  domesticRate: 0.05,
  foreignRate: 0.03,
  volSurface: defaultVolSurface,
}

const makeLeg = (overrides: Partial<LegData> = {}): LegData => ({
  direction: 'buy',
  notional: 1_000_000,
  strike: 1.1,
  expiry: '2025-12-31',
  optionType: 'call',
  exerciseStyle: 'european',
  premium: 25000,
  premiumCurrency: 'USD',
  barrier: {
    type: 'none',
    direction: 'up',
    level: 0,
    schedule: {},
  },
  greeks: { ...defaultGreeks },
  marketData: {
    ...defaultMarketData,
    volSurface: { ...defaultVolSurface, smile: { ...defaultVolSurface.smile } },
  },
  isLocked: false,
  ...overrides,
})

const ecommerceFixtures = {
  empty: {
    portfolio: {
      books: {
        b1: {
          name: 'Premium Electronics',
          desk: 'NYC',
          riskLimit: 50_000_000,
          products: {
            p1: {
              ccyPair: 'EURUSD',
              valueDate: '2025-12-31',
              tradeDate: '2025-01-15',
              status: 'draft',
              trader: 'jsmith',
              counterparty: 'ACME Corp',
              legGroups: {
                g1: {
                  strategy: 'straddle',
                  legs: {
                    l1: makeLeg({
                      direction: 'buy',
                      optionType: 'call',
                      strike: 1.1,
                    }),
                    l2: makeLeg({
                      direction: 'buy',
                      optionType: 'put',
                      strike: 1.1,
                      greeks: { ...defaultGreeks, delta: -0.45 },
                    }),
                  },
                  groupDelta: 0.1,
                  groupNotional: 2_000_000,
                },
              },
            },
            p2: {
              ccyPair: 'USDJPY',
              valueDate: '2025-09-30',
              tradeDate: '2025-01-15',
              status: 'pending',
              trader: 'jsmith',
              counterparty: 'Beta Fund',
              legGroups: {
                g1: {
                  strategy: 'vanilla',
                  legs: {
                    l1: makeLeg({
                      direction: 'sell',
                      optionType: 'put',
                      strike: 148.5,
                      notional: 5_000_000,
                      premium: 120000,
                      barrier: {
                        type: 'knockout',
                        direction: 'down',
                        level: 140,
                        schedule: { ...defaultBarrierSchedule },
                      },
                      greeks: {
                        delta: -0.35,
                        gamma: 0.02,
                        vega: 0.15,
                        theta: -0.06,
                        rho: 0.008,
                      },
                      marketData: {
                        spot: 150.2,
                        forward: 149.8,
                        domesticRate: 0.05,
                        foreignRate: 0.002,
                        volSurface: {
                          atmVol: 0.08,
                          skew: -0.01,
                          kurtosis: 0.003,
                          smile: {
                            s25p: { strike: 146, vol: 0.09, source: 'market' },
                            atm: { strike: 150, vol: 0.08, source: 'model' },
                          },
                        },
                      },
                    }),
                  },
                  groupDelta: -0.35,
                  groupNotional: 5_000_000,
                },
              },
            },
          },
        },
      },
    },
    globalMarket: {
      regime: 'normal',
      lastUpdate: Date.now(),
    },
    aggregatedDelta: 0,
    aggregatedGamma: 0,
    isHedged: true,
    needsRebalance: false,
    _errors: {},
  } satisfies EcommerceCatalog,
}

// ---------------------------------------------------------------------------
// Custom Concerns
// ---------------------------------------------------------------------------

/**
 * Custom concern: margin sufficiency check
 * Returns { sufficient: boolean, required: number, available: number }
 */
const marginCheck: ConcernType<
  { riskLimitPath: string; notionalPath: string },
  { sufficient: boolean; required: number; available: number }
> = {
  name: 'marginCheck',
  description: 'Checks if notional is within risk limit',
  evaluate: (props) => {
    const state = props.state

    const riskLimit = Number(dot.get__unsafe(state, props.riskLimitPath)) || 0
    const notional = Number(dot.get__unsafe(state, props.notionalPath)) || 0

    return {
      sufficient: notional <= riskLimit,
      required: notional,
      available: riskLimit,
    }
  },
}

/**
 * Custom concern: barrier proximity warning
 * Returns { isNearBarrier: boolean, distance: number, distancePct: number }
 */
const barrierProximity: ConcernType<
  { spotPath: string; barrierLevelPath: string; thresholdPct: number },
  { isNearBarrier: boolean; distance: number; distancePct: number }
> = {
  name: 'barrierProximity',
  description: 'Warns when spot is near barrier level',
  evaluate: (props) => {
    const state = props.state

    const spot = Number(dot.get__unsafe(state, props.spotPath)) || 0
    const barrierLevel =
      Number(dot.get__unsafe(state, props.barrierLevelPath)) || 0

    if (barrierLevel === 0 || spot === 0) {
      return { isNearBarrier: false, distance: 0, distancePct: 0 }
    }

    const distance = Math.abs(spot - barrierLevel)
    const distancePct = (distance / spot) * 100

    return {
      isNearBarrier: distancePct <= props.thresholdPct,
      distance,
      distancePct,
    }
  },
}

// ---------------------------------------------------------------------------
// Path Constants (with hash key notation)
// ---------------------------------------------------------------------------

// Book b1, Product p1, LegGroup g1, Leg l1 paths
const STRIKE_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.strike` as const
const NOTIONAL_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.notional` as const
const STATUS_P1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.status` as const
const MARKET_DATA_SPOT_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.spot` as const
const MARKET_DATA_FORWARD_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.forward` as const
const IS_LOCKED_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.isLocked` as const
const CCY_PAIR_P1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.ccyPair` as const
const STRATEGY_G1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.strategy` as const
const VOL_SURFACE_ATM_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.volSurface.atmVol` as const
const SMILE_VOL_S25P =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.volSurface.smile.${_('s25p')}.vol` as const
const SMILE_SOURCE_S25P =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.volSurface.smile.${_('s25p')}.source` as const
const GROUP_NOTIONAL_G1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.groupNotional` as const
const RISK_LIMIT_B1 = `portfolio.books.${_('b1')}.riskLimit` as const

// Book b1, Product p1, LegGroup g1, Leg l2 paths
const STRIKE_L2 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l2')}.strike` as const
const GREEKS_DELTA_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.greeks.delta` as const
const GREEKS_DELTA_L2 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l2')}.greeks.delta` as const
const GREEKS_L1 =
  `portfolio.books.${_('b1')}.products.${_('p1')}.legGroups.${_('g1')}.legs.${_('l1')}.greeks` as const

// Book b1, Product p2, LegGroup g1, Leg l1 paths
const BARRIER_TYPE_P2_L1 =
  `portfolio.books.${_('b1')}.products.${_('p2')}.legGroups.${_('g1')}.legs.${_('l1')}.barrier.type` as const
const BARRIER_LEVEL_P2_L1 =
  `portfolio.books.${_('b1')}.products.${_('p2')}.legGroups.${_('g1')}.legs.${_('l1')}.barrier.level` as const
const BARRIER_SCHEDULE_OBS1 =
  `portfolio.books.${_('b1')}.products.${_('p2')}.legGroups.${_('g1')}.legs.${_('l1')}.barrier.schedule.${_('obs-1')}.level` as const
const MARKET_DATA_SPOT_P2_L1 =
  `portfolio.books.${_('b1')}.products.${_('p2')}.legGroups.${_('g1')}.legs.${_('l1')}.marketData.spot` as const

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Deep clone fixture to prevent valtio proxy from mutating the original */
const freshFixture = (): EcommerceCatalog =>
  JSON.parse(JSON.stringify(ecommerceFixtures.empty))

// ---------------------------------------------------------------------------
// Store factory
// ---------------------------------------------------------------------------

const createCatalogStore = () => createGenericStore<EcommerceCatalog>()

// ---------------------------------------------------------------------------
// Test component
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('Integration: E-commerce Catalog – Deep Nesting & Full Feature Coverage', () => {
  let store: ReturnType<typeof createCatalogStore>

  beforeEach(() => {
    store = createCatalogStore()
  })

  // =========================================================================
  // 1. VALIDATION (validationState) on deeply nested paths
  // =========================================================================

  describe('Validation on deeply nested paths', () => {
    it('validates strike price with Zod schema at depth 9', async () => {
      function StrikeValidator() {
        store.useConcerns('strike-validation', {
          [STRIKE_L1]: {
            validationState: {
              schema: z
                .number()
                .positive('Strike must be positive')
                .max(10, 'Strike too high for EURUSD'),
            },
          },
        })

        const strikeField = store
          .withConcerns({ validationState: true })
          .useFieldStore(STRIKE_L1)

        return (
          <div>
            <input
              data-testid="strike-input"
              type="number"
              value={strikeField.value}
              onChange={(e) => strikeField.setValue(parseFloat(e.target.value))}
            />
            {strikeField.validationState?.isError && (
              <span data-testid="strike-error">
                {strikeField.validationState.errors[0]?.message}
              </span>
            )}
            <span data-testid="strike-valid">
              {String(!strikeField.validationState?.isError)}
            </span>
          </div>
        )
      }

      renderWithStore(<StrikeValidator />, store, freshFixture())

      // Initial value 1.1 should be valid
      await flushEffects()
      expect(screen.getByTestId('strike-valid')).toHaveTextContent('true')

      // Set strike to -5 → invalid
      fireEvent.change(screen.getByTestId('strike-input'), {
        target: { value: '-5' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-error')).toHaveTextContent(
        'Strike must be positive',
      )

      // Set strike to 15 → too high
      fireEvent.change(screen.getByTestId('strike-input'), {
        target: { value: '15' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-error')).toHaveTextContent(
        'Strike too high for EURUSD',
      )

      // Set strike to 1.25 → valid again
      fireEvent.change(screen.getByTestId('strike-input'), {
        target: { value: '1.25' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-valid')).toHaveTextContent('true')
    })

    it('validates barrier schedule entry at depth 13', async () => {
      function BarrierScheduleValidator() {
        store.useConcerns('barrier-schedule-val', {
          [BARRIER_SCHEDULE_OBS1]: {
            validationState: {
              schema: z.number().positive('Barrier level must be positive'),
            },
          },
        })

        const levelField = store
          .withConcerns({ validationState: true })
          .useFieldStore(BARRIER_SCHEDULE_OBS1)

        return (
          <div>
            <input
              data-testid="barrier-level"
              type="number"
              value={levelField.value}
              onChange={(e) => levelField.setValue(parseFloat(e.target.value))}
            />
            {levelField.validationState?.isError && (
              <span data-testid="barrier-level-error">
                {levelField.validationState.errors[0]?.message}
              </span>
            )}
          </div>
        )
      }

      renderWithStore(<BarrierScheduleValidator />, store, freshFixture())
      await flushEffects()

      // Initial 1.12 is valid
      expect(
        screen.queryByTestId('barrier-level-error'),
      ).not.toBeInTheDocument()

      // Set to -1 → invalid
      fireEvent.change(screen.getByTestId('barrier-level'), {
        target: { value: '-1' },
      })
      await flushEffects()
      expect(screen.getByTestId('barrier-level-error')).toHaveTextContent(
        'Barrier level must be positive',
      )
    })

    it('validates vol surface smile point at depth 15', async () => {
      function SmileValidator() {
        store.useConcerns('smile-val', {
          [SMILE_VOL_S25P]: {
            validationState: {
              schema: z
                .number()
                .min(0, 'Vol cannot be negative')
                .max(2, 'Vol unreasonably high'),
            },
          },
        })

        const volField = store
          .withConcerns({ validationState: true })
          .useFieldStore(SMILE_VOL_S25P)

        return (
          <div>
            <input
              data-testid="smile-vol"
              type="number"
              step="0.01"
              value={volField.value}
              onChange={(e) => volField.setValue(parseFloat(e.target.value))}
            />
            {volField.validationState?.isError && (
              <span data-testid="smile-vol-error">
                {volField.validationState.errors[0]?.message}
              </span>
            )}
          </div>
        )
      }

      renderWithStore(<SmileValidator />, store, freshFixture())
      await flushEffects()

      expect(screen.queryByTestId('smile-vol-error')).not.toBeInTheDocument()

      fireEvent.change(screen.getByTestId('smile-vol'), {
        target: { value: '-0.05' },
      })
      await flushEffects()
      expect(screen.getByTestId('smile-vol-error')).toHaveTextContent(
        'Vol cannot be negative',
      )
    })
  })

  // =========================================================================
  // 2. CONDITIONAL UI (disabledWhen, visibleWhen, readonlyWhen) with BoolLogic
  // =========================================================================

  describe('Conditional UI with BoolLogic', () => {
    it('disables strike editing when product status is approved (deep BoolLogic)', async () => {
      function StrikeDisable() {
        const statusField = store.useFieldStore(STATUS_P1)

        store.useConcerns('strike-disabled', {
          [STRIKE_L1]: {
            disabledWhen: {
              condition: {
                OR: [
                  {
                    IS_EQUAL: [STATUS_P1, 'approved'],
                  },
                  {
                    IS_EQUAL: [STATUS_P1, 'executed'],
                  },
                  {
                    IS_EQUAL: [IS_LOCKED_L1, true],
                  },
                ],
              },
            },
          },
        })

        const strikeField = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore(STRIKE_L1)
        const isDisabled = strikeField.disabledWhen === true

        return (
          <div>
            <select
              data-testid="status-select"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'pending'
                    | 'approved'
                    | 'rejected'
                    | 'executed',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="pending">Pending</option>
              <option value="approved">Approved</option>
              <option value="executed">Executed</option>
            </select>
            <input
              data-testid="strike-input"
              type="number"
              value={strikeField.value}
              disabled={isDisabled}
              onChange={(e) => strikeField.setValue(parseFloat(e.target.value))}
            />
            <span data-testid="disabled-state">{String(isDisabled)}</span>
          </div>
        )
      }

      renderWithStore(<StrikeDisable />, store, freshFixture())
      await flushEffects()

      // Draft → not disabled
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('false')

      // Approved → disabled
      fireEvent.change(screen.getByTestId('status-select'), {
        target: { value: 'approved' },
      })
      await flushEffects()
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('true')
      expect(screen.getByTestId('strike-input')).toBeDisabled()

      // Back to draft → enabled
      fireEvent.change(screen.getByTestId('status-select'), {
        target: { value: 'draft' },
      })
      await flushEffects()
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('false')
    })

    it('shows barrier config only when barrier type is not none (visibleWhen)', async () => {
      function BarrierVisibility() {
        const barrierTypeField = store.useFieldStore(BARRIER_TYPE_P2_L1)

        store.useConcerns('barrier-vis', {
          [BARRIER_LEVEL_P2_L1]: {
            visibleWhen: {
              condition: {
                NOT: { IS_EQUAL: [BARRIER_TYPE_P2_L1, 'none'] },
              },
            },
          },
        })

        const barrierLevelField = store
          .withConcerns({ visibleWhen: true })
          .useFieldStore(BARRIER_LEVEL_P2_L1)
        const showBarrierLevel = barrierLevelField.visibleWhen !== false

        return (
          <div>
            <select
              data-testid="barrier-type"
              value={barrierTypeField.value}
              onChange={(e) =>
                barrierTypeField.setValue(
                  e.target.value as 'knockout' | 'knockin' | 'none',
                )
              }
            >
              <option value="none">None</option>
              <option value="knockout">Knockout</option>
              <option value="knockin">Knockin</option>
            </select>
            {showBarrierLevel && (
              <input
                data-testid="barrier-level-input"
                type="number"
                value={barrierLevelField.value}
                onChange={(e) =>
                  barrierLevelField.setValue(parseFloat(e.target.value))
                }
              />
            )}
          </div>
        )
      }

      renderWithStore(<BarrierVisibility />, store, freshFixture())
      await flushEffects()

      // P2 leg1 has barrier type = knockout → visible
      expect(screen.getByTestId('barrier-level-input')).toBeInTheDocument()

      // Set to none → hidden
      fireEvent.change(screen.getByTestId('barrier-type'), {
        target: { value: 'none' },
      })
      await flushEffects()
      expect(
        screen.queryByTestId('barrier-level-input'),
      ).not.toBeInTheDocument()

      // Set to knockin → visible again
      fireEvent.change(screen.getByTestId('barrier-type'), {
        target: { value: 'knockin' },
      })
      await flushEffects()
      expect(screen.getByTestId('barrier-level-input')).toBeInTheDocument()
    })

    it('makes fields readonly in crisis market regime (readonlyWhen)', async () => {
      function CrisisReadonly() {
        const regimeField = store.useFieldStore('globalMarket.regime' as const)

        store.useConcerns('crisis-readonly', {
          [STRIKE_L1]: {
            readonlyWhen: {
              condition: {
                IS_EQUAL: ['globalMarket.regime' as const, 'crisis'],
              },
            },
          },
        })

        const strikeField = store
          .withConcerns({ readonlyWhen: true })
          .useFieldStore(STRIKE_L1)
        const isReadonly = strikeField.readonlyWhen === true

        return (
          <div>
            <select
              data-testid="regime-select"
              value={regimeField.value}
              onChange={(e) =>
                regimeField.setValue(
                  e.target.value as 'normal' | 'stressed' | 'crisis',
                )
              }
            >
              <option value="normal">Normal</option>
              <option value="stressed">Stressed</option>
              <option value="crisis">Crisis</option>
            </select>
            <input
              data-testid="strike-input"
              type="number"
              value={strikeField.value}
              readOnly={isReadonly}
              onChange={(e) => strikeField.setValue(parseFloat(e.target.value))}
            />
          </div>
        )
      }

      renderWithStore(<CrisisReadonly />, store, freshFixture())
      await flushEffects()

      expect(screen.getByTestId('strike-input')).not.toHaveAttribute('readonly')

      fireEvent.change(screen.getByTestId('regime-select'), {
        target: { value: 'crisis' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-input')).toHaveAttribute('readonly')
    })

    it('uses complex AND/OR/NOT BoolLogic for multi-condition disable', async () => {
      function ComplexConditions() {
        const statusField = store.useFieldStore(STATUS_P1)
        const regimeField = store.useFieldStore('globalMarket.regime' as const)

        // Disable notional when: (status != draft) AND (regime = stressed OR regime = crisis)
        store.useConcerns('complex-conditions', {
          [NOTIONAL_L1]: {
            disabledWhen: {
              condition: {
                AND: [
                  {
                    NOT: {
                      IS_EQUAL: [STATUS_P1, 'draft'],
                    },
                  },
                  {
                    OR: [
                      {
                        IS_EQUAL: ['globalMarket.regime' as const, 'stressed'],
                      },
                      { IS_EQUAL: ['globalMarket.regime' as const, 'crisis'] },
                    ],
                  },
                ],
              },
            },
          },
        })

        const notionalField = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore(NOTIONAL_L1)
        const isDisabled = notionalField.disabledWhen === true

        return (
          <div>
            <select
              data-testid="status"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'pending'
                    | 'approved'
                    | 'rejected'
                    | 'executed',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="pending">Pending</option>
              <option value="approved">Approved</option>
            </select>
            <select
              data-testid="regime"
              value={regimeField.value}
              onChange={(e) =>
                regimeField.setValue(
                  e.target.value as 'normal' | 'stressed' | 'crisis',
                )
              }
            >
              <option value="normal">Normal</option>
              <option value="stressed">Stressed</option>
              <option value="crisis">Crisis</option>
            </select>
            <input
              data-testid="notional"
              type="number"
              value={notionalField.value}
              disabled={isDisabled}
              onChange={(e) =>
                notionalField.setValue(parseFloat(e.target.value))
              }
            />
          </div>
        )
      }

      renderWithStore(<ComplexConditions />, store, freshFixture())
      await flushEffects()

      // draft + normal → enabled
      expect(screen.getByTestId('notional')).not.toBeDisabled()

      // draft + stressed → enabled (because status IS draft, AND fails)
      fireEvent.change(screen.getByTestId('regime'), {
        target: { value: 'stressed' },
      })
      await flushEffects()
      expect(screen.getByTestId('notional')).not.toBeDisabled()

      // pending + stressed → disabled (status != draft AND regime = stressed)
      fireEvent.change(screen.getByTestId('status'), {
        target: { value: 'pending' },
      })
      await flushEffects()
      expect(screen.getByTestId('notional')).toBeDisabled()

      // pending + normal → enabled (regime condition fails)
      fireEvent.change(screen.getByTestId('regime'), {
        target: { value: 'normal' },
      })
      await flushEffects()
      expect(screen.getByTestId('notional')).not.toBeDisabled()
    })
  })

  // =========================================================================
  // 3. DYNAMIC TEXT (dynamicLabel, dynamicTooltip, dynamicPlaceholder)
  // =========================================================================

  describe('Dynamic text interpolation', () => {
    it('interpolates dynamic tooltip from deeply nested market data', async () => {
      function DynamicTooltipComponent() {
        store.useConcerns('tooltips', {
          [STRIKE_L1]: {
            dynamicTooltip: {
              template: `Spot: {{${MARKET_DATA_SPOT_L1}}} | Fwd: {{${MARKET_DATA_FORWARD_L1}}}`,
            },
          },
        })

        const strikeField = store
          .withConcerns({ dynamicTooltip: true })
          .useFieldStore(STRIKE_L1)
        const tooltip = String(strikeField.dynamicTooltip ?? '')

        return (
          <div>
            <div data-testid="strike-tooltip" title={tooltip}>
              <input
                data-testid="strike"
                type="number"
                value={strikeField.value}
                onChange={(e) =>
                  strikeField.setValue(parseFloat(e.target.value))
                }
              />
            </div>
            <span data-testid="tooltip-text">{tooltip}</span>
          </div>
        )
      }

      renderWithStore(<DynamicTooltipComponent />, store, freshFixture())
      await flushEffects()

      expect(screen.getByTestId('tooltip-text')).toHaveTextContent(
        'Spot: 1.1 | Fwd: 1.105',
      )
    })

    it('interpolates dynamic label with ccyPair and strategy', async () => {
      function DynamicLabelComponent() {
        store.useConcerns('labels', {
          [NOTIONAL_L1]: {
            dynamicLabel: {
              template: `Notional ({{${CCY_PAIR_P1}}} {{${STRATEGY_G1}}})`,
            },
          },
        })

        const notionalField = store
          .withConcerns({ dynamicLabel: true })
          .useFieldStore(NOTIONAL_L1)
        const label = String(notionalField.dynamicLabel ?? '')

        return <span data-testid="notional-label">{label}</span>
      }

      renderWithStore(<DynamicLabelComponent />, store, freshFixture())
      await flushEffects()

      expect(screen.getByTestId('notional-label')).toHaveTextContent(
        'Notional (EURUSD straddle)',
      )
    })

    it('interpolates dynamic placeholder with vol surface data at depth 14', async () => {
      function DynamicPlaceholderComponent() {
        store.useConcerns('placeholders', {
          [STRIKE_L1]: {
            dynamicPlaceholder: {
              template: `ATM Vol: {{${VOL_SURFACE_ATM_L1}}}`,
            },
          },
        })

        const strikeField = store
          .withConcerns({ dynamicPlaceholder: true })
          .useFieldStore(STRIKE_L1)
        const placeholder = String(strikeField.dynamicPlaceholder ?? '')

        return (
          <input
            data-testid="strike"
            placeholder={placeholder}
            value=""
            readOnly
          />
        )
      }

      renderWithStore(<DynamicPlaceholderComponent />, store, freshFixture())
      await flushEffects()

      expect(screen.getByTestId('strike')).toHaveAttribute(
        'placeholder',
        'ATM Vol: 0.1',
      )
    })
  })

  // =========================================================================
  // 4. CUSTOM CONCERNS
  // =========================================================================

  describe('Custom concerns', () => {
    it('marginCheck concern validates notional against risk limit', async () => {
      function MarginCheckComponent() {
        store.useConcerns(
          'margin-check',
          {
            [GROUP_NOTIONAL_G1]: {
              marginCheck: {
                riskLimitPath: RISK_LIMIT_B1,
                notionalPath: GROUP_NOTIONAL_G1,
              },
            },
          },
          [...defaultConcerns, marginCheck],
        )

        const notionalField = store
          .withConcerns({ marginCheck: true })
          .useFieldStore(GROUP_NOTIONAL_G1)
        const margin = notionalField.marginCheck as
          | { sufficient: boolean; required: number; available: number }
          | undefined

        return (
          <div>
            <input
              data-testid="group-notional"
              type="number"
              value={notionalField.value}
              onChange={(e) =>
                notionalField.setValue(parseFloat(e.target.value))
              }
            />
            <span data-testid="margin-sufficient">
              {String(margin?.sufficient ?? 'unknown')}
            </span>
            <span data-testid="margin-required">
              {String(margin?.required ?? 0)}
            </span>
          </div>
        )
      }

      renderWithStore(<MarginCheckComponent />, store, freshFixture())
      await flushEffects()

      // 2M notional vs 50M limit → sufficient
      expect(screen.getByTestId('margin-sufficient')).toHaveTextContent('true')

      // Increase to 60M → breached
      fireEvent.change(screen.getByTestId('group-notional'), {
        target: { value: '60000000' },
      })
      await flushEffects()
      expect(screen.getByTestId('margin-sufficient')).toHaveTextContent('false')
      expect(screen.getByTestId('margin-required')).toHaveTextContent(
        '60000000',
      )
    })

    it('barrierProximity concern warns when spot is near barrier', async () => {
      function BarrierProximityComponent() {
        store.useConcerns(
          'barrier-proximity',
          {
            [MARKET_DATA_SPOT_P2_L1]: {
              barrierProximity: {
                spotPath: MARKET_DATA_SPOT_P2_L1,
                barrierLevelPath: BARRIER_LEVEL_P2_L1,
                thresholdPct: 5,
              },
            },
          },
          [barrierProximity],
        )

        const spotField = store
          .withConcerns({ barrierProximity: true })
          .useFieldStore(MARKET_DATA_SPOT_P2_L1)
        const proximity = spotField.barrierProximity as
          | { isNearBarrier: boolean; distance: number; distancePct: number }
          | undefined

        return (
          <div>
            <input
              data-testid="spot"
              type="number"
              step="0.1"
              value={spotField.value}
              onChange={(e) => spotField.setValue(parseFloat(e.target.value))}
            />
            <span data-testid="near-barrier">
              {String(proximity?.isNearBarrier ?? false)}
            </span>
            <span data-testid="barrier-distance-pct">
              {proximity?.distancePct?.toFixed(2) ?? '0'}
            </span>
          </div>
        )
      }

      renderWithStore(<BarrierProximityComponent />, store, freshFixture())
      await flushEffects()

      // Spot 150.2, barrier 140 → distance ~6.8% → not near
      expect(screen.getByTestId('near-barrier')).toHaveTextContent('false')

      // Move spot to 142 → distance ~1.4% → near barrier
      fireEvent.change(screen.getByTestId('spot'), { target: { value: '142' } })
      await flushEffects()
      expect(screen.getByTestId('near-barrier')).toHaveTextContent('true')
    })

    it('custom concern combined with built-in concerns on same path', async () => {
      function CombinedConcerns() {
        // Register built-in validationState + custom marginCheck on the SAME path
        store.useConcerns(
          'combined',
          {
            [NOTIONAL_L1]: {
              validationState: {
                schema: z.number().positive('Notional must be positive'),
              },
              disabledWhen: {
                condition: {
                  IS_EQUAL: [STATUS_P1, 'executed'],
                },
              },
              marginCheck: {
                riskLimitPath: RISK_LIMIT_B1,
                notionalPath: NOTIONAL_L1,
              },
            },
          },
          [...defaultConcerns, marginCheck],
        )

        const notionalField = store
          .withConcerns({
            validationState: true,
            disabledWhen: true,
            marginCheck: true,
          })
          .useFieldStore(NOTIONAL_L1)
        const validation = notionalField.validationState
        const disabled = notionalField.disabledWhen
        const margin = notionalField.marginCheck as
          | { sufficient: boolean; required: number; available: number }
          | undefined

        return (
          <div>
            <input
              data-testid="notional"
              type="number"
              value={notionalField.value}
              disabled={disabled === true}
              onChange={(e) =>
                notionalField.setValue(parseFloat(e.target.value))
              }
            />
            <span data-testid="valid">{String(!validation?.isError)}</span>
            <span data-testid="disabled">{String(disabled)}</span>
            <span data-testid="margin-ok">{String(margin?.sufficient)}</span>
          </div>
        )
      }

      renderWithStore(<CombinedConcerns />, store, freshFixture())
      await flushEffects()

      // All should be healthy initially
      expect(screen.getByTestId('valid')).toHaveTextContent('true')
      expect(screen.getByTestId('disabled')).toHaveTextContent('false')
      expect(screen.getByTestId('margin-ok')).toHaveTextContent('true')

      // Set negative notional → validation fails, margin still computes
      fireEvent.change(screen.getByTestId('notional'), {
        target: { value: '-100' },
      })
      await flushEffects()
      expect(screen.getByTestId('valid')).toHaveTextContent('false')
    })
  })

  // =========================================================================
  // 5. SIDE EFFECTS (syncPaths, flipPaths, listeners)
  // =========================================================================

  describe('Side effects on deep state', () => {
    it('syncs strike across legs within a straddle', async () => {
      function StrikeSync() {
        const [strike1, setStrike1] = store.useStore(STRIKE_L1)
        const [strike2] = store.useStore(STRIKE_L2)

        store.useSideEffects('straddle-sync', {
          syncPaths: [
            typeHelpers.syncPair<EcommerceCatalog>(STRIKE_L1, STRIKE_L2),
          ],
        })

        return (
          <div>
            <input
              data-testid="strike-l1"
              type="number"
              value={strike1}
              onChange={(e) => setStrike1(parseFloat(e.target.value))}
            />
            <span data-testid="strike-l2">{String(strike2)}</span>
          </div>
        )
      }

      renderWithStore(<StrikeSync />, store, freshFixture())
      await flushEffects()

      // Read initial synced value (both legs start at 1.1)
      const initialStrike = Number(screen.getByTestId('strike-l2').textContent)

      // Change leg1 strike to a different value → leg2 should sync
      const newStrike = initialStrike + 0.25
      fireEvent.change(screen.getByTestId('strike-l1'), {
        target: { value: String(newStrike) },
      })
      await flushEffects()
      expect(Number(screen.getByTestId('strike-l2').textContent)).toBe(
        newStrike,
      )
    })

    it('flips isHedged/needsRebalance at top level', async () => {
      function HedgeFlip() {
        const [isHedged, setHedged] = store.useStore('isHedged')
        const [needsRebalance] = store.useStore('needsRebalance')

        store.useSideEffects('hedge-flip', {
          flipPaths: [
            typeHelpers.flipPair<EcommerceCatalog>(
              'isHedged',
              'needsRebalance',
            ),
          ],
        })

        return (
          <div>
            <button data-testid="toggle-hedge" onClick={() => setHedged(false)}>
              Unhedge
            </button>
            <span data-testid="hedged">{String(isHedged)}</span>
            <span data-testid="rebalance">{String(needsRebalance)}</span>
          </div>
        )
      }

      renderWithStore(<HedgeFlip />, store, freshFixture())
      await flushEffects()

      // Initially hedged
      expect(screen.getByTestId('hedged')).toHaveTextContent('true')
      expect(screen.getByTestId('rebalance')).toHaveTextContent('false')

      // Toggle → flip
      fireEvent.click(screen.getByTestId('toggle-hedge'))
      await flushEffects()
      expect(screen.getByTestId('hedged')).toHaveTextContent('false')
      expect(screen.getByTestId('rebalance')).toHaveTextContent('true')
    })

    it('listener computes aggregated delta from leg changes', async () => {
      function DeltaListener() {
        const storeInstance = useStoreContext<EcommerceCatalog>()
        const [leg1Delta, setLeg1Delta] = store.useStore(GREEKS_DELTA_L1)
        const [leg2Delta] = store.useStore(GREEKS_DELTA_L2)
        const [aggDelta] = store.useStore('aggregatedDelta' as const)

        useLayoutEffect(() => {
          const cleanup = registerListener(storeInstance, {
            path: GREEKS_L1,
            scope: null,
            fn: (changes, state) => {
              // Changes have paths relative to watched path: ['delta', 0.7, {}]
              // State is full pre-change snapshot (scope: null)
              const legs =
                state?.portfolio?.books?.[_('b1')]?.products?.[_('p1')]
                  ?.legGroups?.[_('g1')]?.legs
              const l2d = legs?.[_('l2')]?.greeks?.delta ?? -0.45
              let l1d = legs?.[_('l1')]?.greeks?.delta ?? 0.55
              for (const change of changes) {
                if (change[0] === 'delta') l1d = Number(change[1])
              }
              return typeHelpers.changes<EcommerceCatalog>([
                ['aggregatedDelta' as const, l1d + l2d],
              ])
            },
          })
          return cleanup
        }, [storeInstance])

        return (
          <div>
            <input
              data-testid="l1-delta"
              type="number"
              step="0.01"
              value={leg1Delta}
              onChange={(e) => setLeg1Delta(parseFloat(e.target.value))}
            />
            <span data-testid="l2-delta">{String(leg2Delta)}</span>
            <span data-testid="agg-delta">{String(aggDelta)}</span>
          </div>
        )
      }

      renderWithStore(<DeltaListener />, store, freshFixture())
      await flushEffects()

      // Change leg1 delta to 0.7
      fireEvent.change(screen.getByTestId('l1-delta'), {
        target: { value: '0.7' },
      })
      await flushEffects()

      // Aggregated delta should be 0.7 + (-0.45) = 0.25
      const aggDelta = parseFloat(
        screen.getByTestId('agg-delta').textContent || '0',
      )
      expect(aggDelta).toBeCloseTo(0.25, 1)
    })
  })

  // =========================================================================
  // 6. JIT STORE – Batch updates on deeply nested state
  // =========================================================================

  describe('JitStore batch updates', () => {
    it('applies batch changes across multiple deeply nested paths', async () => {
      function BatchUpdater() {
        const { setChanges } = store.useJitStore()
        const [strike] = store.useStore(STRIKE_L1)
        const [notional] = store.useStore(NOTIONAL_L1)
        const [status] = store.useStore(STATUS_P1)

        const handleBatchUpdate = () => {
          setChanges(
            typeHelpers.changes<EcommerceCatalog>([
              [STRIKE_L1, 1.35],
              [NOTIONAL_L1, 5_000_000],
              [STATUS_P1, 'pending'],
            ]),
          )
        }

        return (
          <div>
            <button data-testid="batch-update" onClick={handleBatchUpdate}>
              Batch
            </button>
            <span data-testid="strike">{String(strike)}</span>
            <span data-testid="notional">{String(notional)}</span>
            <span data-testid="status">{String(status)}</span>
          </div>
        )
      }

      renderWithStore(<BatchUpdater />, store, freshFixture())

      fireEvent.click(screen.getByTestId('batch-update'))
      await flushEffects()

      expect(screen.getByTestId('strike')).toHaveTextContent('1.35')
      expect(screen.getByTestId('notional')).toHaveTextContent('5000000')
      expect(screen.getByTestId('status')).toHaveTextContent('pending')
    })
  })

  // =========================================================================
  // 7. COMBINED: concerns + side effects + deep nesting
  // =========================================================================

  describe('Combined concerns and side effects', () => {
    it('validation + sync + conditional UI work together on straddle', async () => {
      function StraddleForm() {
        const [strike1, setStrike1] = store.useStore(STRIKE_L1)
        const [strike2] = store.useStore(STRIKE_L2)
        const statusField = store.useFieldStore(STATUS_P1)

        // Sync strikes
        store.useSideEffects('straddle-effects', {
          syncPaths: [
            typeHelpers.syncPair<EcommerceCatalog>(STRIKE_L1, STRIKE_L2),
          ],
        })

        // Validate + disable
        store.useConcerns('straddle-concerns', {
          [STRIKE_L1]: {
            validationState: {
              schema: z
                .number()
                .positive('Strike must be positive')
                .max(5, 'Strike unreasonable'),
            },
            disabledWhen: {
              condition: {
                OR: [
                  {
                    IS_EQUAL: [STATUS_P1, 'approved'],
                  },
                  {
                    IS_EQUAL: [STATUS_P1, 'executed'],
                  },
                ],
              },
            },
            dynamicTooltip: {
              template: `Strike for {{${CCY_PAIR_P1}}} (spot: {{${MARKET_DATA_SPOT_L1}}})`,
            },
          },
        })

        const strikeField = store
          .withConcerns({
            validationState: true,
            disabledWhen: true,
            dynamicTooltip: true,
          })
          .useFieldStore(STRIKE_L1)
        const validation = strikeField.validationState
        const disabled = strikeField.disabledWhen === true
        const tooltip = strikeField.dynamicTooltip ?? ''

        return (
          <div>
            <select
              data-testid="status"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'pending'
                    | 'approved'
                    | 'rejected'
                    | 'executed',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="approved">Approved</option>
              <option value="executed">Executed</option>
            </select>
            <input
              data-testid="strike-l1"
              type="number"
              value={strike1}
              disabled={disabled}
              onChange={(e) => setStrike1(parseFloat(e.target.value))}
            />
            <span data-testid="strike-l2">{String(strike2)}</span>
            <span data-testid="tooltip">{tooltip}</span>
            {validation?.isError && (
              <span data-testid="validation-error">
                {validation.errors[0]?.message}
              </span>
            )}
          </div>
        )
      }

      renderWithStore(<StraddleForm />, store, freshFixture())
      await flushEffects()

      // Initial: valid, enabled, synced, tooltip populated
      expect(screen.queryByTestId('validation-error')).not.toBeInTheDocument()
      expect(screen.getByTestId('strike-l1')).not.toBeDisabled()
      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Strike for EURUSD (spot: 1.1)',
      )

      // Change strike → syncs to leg2, validates
      fireEvent.change(screen.getByTestId('strike-l1'), {
        target: { value: '1.25' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-l2')).toHaveTextContent('1.25')
      expect(screen.queryByTestId('validation-error')).not.toBeInTheDocument()

      // Set invalid strike → validation error, but still syncs
      fireEvent.change(screen.getByTestId('strike-l1'), {
        target: { value: '-1' },
      })
      await flushEffects()
      expect(screen.getByTestId('validation-error')).toHaveTextContent(
        'Strike must be positive',
      )
      expect(screen.getByTestId('strike-l2')).toHaveTextContent('-1')

      // Approve product → strike disabled
      fireEvent.change(screen.getByTestId('status'), {
        target: { value: 'approved' },
      })
      await flushEffects()
      expect(screen.getByTestId('strike-l1')).toBeDisabled()
    })

    it('re-evaluates all concerns when deep state changes propagate', async () => {
      function PropagationTest() {
        const spotField = store.useFieldStore(MARKET_DATA_SPOT_L1)

        store.useConcerns('propagation', {
          [STRIKE_L1]: {
            dynamicTooltip: {
              template: `Moneyness vs spot {{${MARKET_DATA_SPOT_L1}}}`,
            },
            disabledWhen: {
              condition: {
                GT: [MARKET_DATA_SPOT_L1, 2],
              },
            },
          },
        })

        const strikeField = store
          .withConcerns({
            dynamicTooltip: true,
            disabledWhen: true,
          })
          .useFieldStore(STRIKE_L1)
        const tooltip = strikeField.dynamicTooltip ?? ''
        const disabled = strikeField.disabledWhen === true

        return (
          <div>
            <input
              data-testid="spot"
              type="number"
              step="0.01"
              value={spotField.value}
              onChange={(e) => spotField.setValue(parseFloat(e.target.value))}
            />
            <span data-testid="tooltip">{tooltip}</span>
            <span data-testid="disabled">{String(disabled)}</span>
          </div>
        )
      }

      renderWithStore(<PropagationTest />, store, freshFixture())
      await flushEffects()

      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Moneyness vs spot 1.1',
      )
      expect(screen.getByTestId('disabled')).toHaveTextContent('false')

      // Move spot above threshold
      fireEvent.change(screen.getByTestId('spot'), { target: { value: '2.5' } })
      await flushEffects()

      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Moneyness vs spot 2.5',
      )
      expect(screen.getByTestId('disabled')).toHaveTextContent('true')
    })
  })

  // =========================================================================
  // 8. DIRECT STORE INSTANCE ASSERTIONS
  // =========================================================================

  describe('Direct store instance assertions', () => {
    it('concern results accessible via _concerns proxy at deep paths', async () => {
      function DirectConcernTest() {
        const statusField = store.useFieldStore(STATUS_P1)

        store.useConcerns('direct-test', {
          [STATUS_P1]: {
            validationState: {
              schema: z.string().min(1, 'Status required'),
            },
            disabledWhen: {
              condition: {
                IS_EQUAL: [STATUS_P1, 'approved'],
              },
            },
          },
        })

        return (
          <div>
            <span data-testid="status">{statusField.value}</span>
          </div>
        )
      }

      const result = renderWithStore(
        <DirectConcernTest />,
        store,
        freshFixture(),
      )
      await flushEffects()

      // Access concern results directly from store instance
      const statusConcerns = result.storeInstance._concerns[STATUS_P1]
      expect(statusConcerns).toBeDefined()
      expect((statusConcerns?.['validationState'] as any)?.isError).toBe(false)
      expect(statusConcerns?.['disabledWhen']).toBe(false)

      // Mutate state directly and re-check
      result.storeInstance.state.portfolio.books[_('b1')]!.products[
        _('p1')
      ]!.status = 'approved'
      await flushEffects()

      const updatedConcerns = result.storeInstance._concerns[STATUS_P1]
      expect(updatedConcerns?.['disabledWhen']).toBe(true)
    })

    it('state mutations at depth 15 are tracked by valtio', async () => {
      function DeepMutationTracker() {
        const sourceField = store.useFieldStore(SMILE_SOURCE_S25P)

        return (
          <div>
            <span data-testid="source">{sourceField.value}</span>
            <button
              data-testid="change-source"
              onClick={() =>
                sourceField.setValue('model' as 'market' | 'model')
              }
            >
              Switch to Model
            </button>
          </div>
        )
      }

      const result = renderWithStore(
        <DeepMutationTracker />,
        store,
        freshFixture(),
      )

      expect(screen.getByTestId('source')).toHaveTextContent('market')

      fireEvent.click(screen.getByTestId('change-source'))
      await flushEffects()

      expect(screen.getByTestId('source')).toHaveTextContent('model')

      // Verify state proxy reflects the change
      const source =
        result.storeInstance.state.portfolio.books[_('b1')]?.products?.[_('p1')]
          ?.legGroups?.[_('g1')]?.legs?.[_('l1')]?.marketData?.volSurface
          ?.smile?.[_('s25p')]?.source
      expect(source).toBe('model')
    })
  })
})
