/**
 * Integration Tests: E-commerce Product Catalog Form
 *
 * Scenario: Deeply nested (12+ levels) e-commerce catalog management form
 * with custom concerns, all built-in concerns, side effects (sync, flip,
 * listeners, aggregations), and complex BoolLogic conditions.
 *
 * Models a realistic e-commerce operations center where:
 * - Departments contain categories, products, and variants with pricing/inventory/shipping
 * - Validation depends on product status, store mode, and cross-variant relationships
 * - Fields are conditionally visible/disabled/readonly based on deep state
 * - Listeners compute derived values (revenue, inventory) from variant changes
 * - Custom concerns handle domain-specific logic (budget check, stock alerts)
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
import { fireEvent, flush, renderWithStore } from '../utils/react'

// ---------------------------------------------------------------------------
// Type hierarchy (depth 12+ via departments.{}.categories.{}.products.{}.variants.{}.reviews.{}.metadata.helpfulVotes)
// ---------------------------------------------------------------------------

interface ReviewMetadata {
  helpfulVotes: number
  reportCount: number
  editedAt: string | null
}

interface Review {
  rating: number
  verified: boolean
  sentiment: 'positive' | 'neutral' | 'negative'
  metadata: ReviewMetadata
}

interface Dimensions {
  length: number
  width: number
  height: number
}

interface ShippingConfig {
  weight: number
  dimensions: Dimensions
  method: 'standard' | 'express' | 'freight'
}

interface InventoryData {
  stock: number
  reserved: number
  warehouse: string
}

interface PricingData {
  base: number
  sale: number
  currency: 'USD' | 'EUR' | 'GBP' | 'JPY'
  margin: number
}

interface Variant {
  sku: string
  color: string
  size: string
  isActive: boolean
  pricing: PricingData
  inventory: InventoryData
  shipping: ShippingConfig
  reviews: Record<string, Review>
}

interface Product {
  name: string
  status: 'draft' | 'active' | 'archived' | 'discontinued'
  brand: string
  variants: Record<string, Variant>
}

interface Category {
  name: string
  slug: string
  products: Record<string, Product>
}

interface Department {
  name: string
  manager: string
  budget: number
  categories: Record<string, Category>
}

interface EcommerceCatalog {
  catalog: {
    departments: Record<string, Department>
  }
  storeConfig: {
    mode: 'normal' | 'sale' | 'clearance'
    lastSync: number
  }
  totalRevenue: number
  totalInventory: number
  isStocked: boolean
  needsReorder: boolean
  _errors: Record<string, string[]>
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const makeVariant = (overrides: Partial<Variant> = {}): Variant => ({
  sku: 'SKU-001',
  color: 'black',
  size: 'M',
  isActive: true,
  pricing: {
    base: 999,
    sale: 799,
    currency: 'USD',
    margin: 0.2,
  },
  inventory: {
    stock: 100,
    reserved: 5,
    warehouse: 'WH-EAST',
  },
  shipping: {
    weight: 0.5,
    dimensions: { length: 15, width: 7.5, height: 1 },
    method: 'standard',
  },
  reviews: {
    r1: {
      rating: 5,
      verified: true,
      sentiment: 'positive',
      metadata: { helpfulVotes: 42, reportCount: 0, editedAt: null },
    },
  },
  ...overrides,
})

const makeCatalogFixture = (): EcommerceCatalog => ({
  catalog: {
    departments: {
      electronics: {
        name: 'Electronics',
        manager: 'Jane Smith',
        budget: 500_000,
        categories: {
          phones: {
            name: 'Smartphones',
            slug: 'phones',
            products: {
              p1: {
                name: 'ProPhone X',
                status: 'active',
                brand: 'TechCo',
                variants: {
                  v1: makeVariant({
                    sku: 'PP-X-BLK-M',
                    color: 'black',
                    size: 'M',
                  }),
                  v2: makeVariant({
                    sku: 'PP-X-WHT-L',
                    color: 'white',
                    size: 'L',
                    pricing: {
                      base: 1099,
                      sale: 899,
                      currency: 'USD',
                      margin: 0.18,
                    },
                    inventory: {
                      stock: 50,
                      reserved: 3,
                      warehouse: 'WH-WEST',
                    },
                  }),
                },
              },
              p2: {
                name: 'BudgetTab',
                status: 'draft',
                brand: 'GadgetInc',
                variants: {
                  v1: makeVariant({
                    sku: 'BT-SLV-S',
                    color: 'silver',
                    size: 'S',
                    pricing: {
                      base: 299,
                      sale: 249,
                      currency: 'USD',
                      margin: 0.15,
                    },
                    inventory: {
                      stock: 8,
                      reserved: 2,
                      warehouse: 'WH-EAST',
                    },
                    shipping: {
                      weight: 0.3,
                      dimensions: { length: 10, width: 6, height: 0.8 },
                      method: 'express',
                    },
                  }),
                },
              },
            },
          },
        },
      },
    },
  },
  storeConfig: {
    mode: 'normal',
    lastSync: Date.now(),
  },
  totalRevenue: 0,
  totalInventory: 0,
  isStocked: true,
  needsReorder: false,
  _errors: {},
})

// ---------------------------------------------------------------------------
// Custom Concerns
// ---------------------------------------------------------------------------

/**
 * Custom concern: budget sufficiency check
 * Checks if variant pricing x quantity stays within department budget
 */
const budgetCheck: ConcernType<
  { budgetPath: string; pricePath: string; quantityPath: string },
  { sufficient: boolean; cost: number; budget: number }
> = {
  name: 'budgetCheck',
  description: 'Checks if pricing x quantity stays within department budget',
  evaluate: (props) => {
    const state = props.state
    const budget = Number(dot.get__unsafe(state, props.budgetPath)) || 0
    const price = Number(dot.get__unsafe(state, props.pricePath)) || 0
    const quantity = Number(dot.get__unsafe(state, props.quantityPath)) || 0
    const cost = price * quantity

    return {
      sufficient: cost <= budget,
      cost,
      budget,
    }
  },
}

/**
 * Custom concern: stock alert
 * Warns when stock minus reserved is below a threshold
 */
const stockAlert: ConcernType<
  { stockPath: string; reservedPath: string; threshold: number },
  { isLowStock: boolean; available: number; threshold: number }
> = {
  name: 'stockAlert',
  description: 'Warns when available stock is below threshold',
  evaluate: (props) => {
    const state = props.state
    const stock = Number(dot.get__unsafe(state, props.stockPath)) || 0
    const reserved = Number(dot.get__unsafe(state, props.reservedPath)) || 0
    const available = stock - reserved

    return {
      isLowStock: available < props.threshold,
      available,
      threshold: props.threshold,
    }
  },
}

// ---------------------------------------------------------------------------
// Path Constants (with hash key notation)
// ---------------------------------------------------------------------------

// Department electronics, category phones, product p1, variant v1
const PRICE_BASE_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.pricing.base` as const
const PRICE_SALE_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.pricing.sale` as const
const STOCK_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.inventory.stock` as const
const RESERVED_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.inventory.reserved` as const
const STATUS_P1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.status` as const
const BRAND_P1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.brand` as const
const IS_ACTIVE_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.isActive` as const
const SHIPPING_WEIGHT_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.shipping.weight` as const
const SHIPPING_DIM_LENGTH_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.shipping.dimensions.length` as const

// Variant v2 of product p1
const PRICE_BASE_V2 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v2')}.pricing.base` as const
const PRICING_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.pricing` as const

// Review paths (deep nesting)
const REVIEW_HELPFUL_VOTES =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.reviews.${_('r1')}.metadata.helpfulVotes` as const
const REVIEW_EDITED_AT =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p1')}.variants.${_('v1')}.reviews.${_('r1')}.metadata.editedAt` as const

// Product p2, variant v1 — low stock scenario
const STOCK_P2_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p2')}.variants.${_('v1')}.inventory.stock` as const
const RESERVED_P2_V1 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p2')}.variants.${_('v1')}.inventory.reserved` as const
const STATUS_P2 =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.products.${_('p2')}.status` as const

// Department-level
const BUDGET_ELECTRONICS =
  `catalog.departments.${_('electronics')}.budget` as const
const CATEGORY_NAME =
  `catalog.departments.${_('electronics')}.categories.${_('phones')}.name` as const

// ---------------------------------------------------------------------------
// Store factory
// ---------------------------------------------------------------------------

const createCatalogStore = () => createGenericStore<EcommerceCatalog>()

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('Integration: E-commerce Catalog -- Deep Nesting & Full Feature Coverage', () => {
  let store: ReturnType<typeof createCatalogStore>

  beforeEach(() => {
    store = createCatalogStore()
  })

  // =========================================================================
  // 1. VALIDATION (headless)
  // =========================================================================

  describe('Validation on deeply nested paths (headless)', () => {
    it('validates variant base price', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [PRICE_BASE_V1]: {
            validationState: {
              schema: z
                .number()
                .positive('Price must be positive')
                .max(10000, 'Price too high'),
            },
          },
        },
      })
      await flush()

      // Initial value 999 should be valid
      const concerns = rendered.storeInstance._concerns[PRICE_BASE_V1]
      expect(
        (concerns?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(false)

      // Set invalid value
      dot.set__unsafe(rendered.storeInstance.state, PRICE_BASE_V1, -5)
      await flush()

      const updated = rendered.storeInstance._concerns[PRICE_BASE_V1]
      expect(
        (updated?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(true)
    })

    it('validates review helpfulVotes at deep nesting', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [REVIEW_HELPFUL_VOTES]: {
            validationState: {
              schema: z
                .number()
                .int('Must be integer')
                .nonnegative('Votes cannot be negative'),
            },
          },
        },
      })
      await flush()

      // Initial 42 is valid
      const concerns = rendered.storeInstance._concerns[REVIEW_HELPFUL_VOTES]
      expect(
        (concerns?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(false)

      // Set to -1 -> invalid
      dot.set__unsafe(rendered.storeInstance.state, REVIEW_HELPFUL_VOTES, -1)
      await flush()

      const updated = rendered.storeInstance._concerns[REVIEW_HELPFUL_VOTES]
      expect(
        (updated?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(true)
    })

    it('validates review metadata editedAt', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [REVIEW_EDITED_AT]: {
            validationState: {
              schema: z
                .string()
                .datetime('Must be valid ISO datetime')
                .nullable(),
            },
          },
        },
      })
      await flush()

      // Initial null is valid (nullable)
      const concerns = rendered.storeInstance._concerns[REVIEW_EDITED_AT]
      expect(
        (concerns?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(false)

      // Set to invalid date string
      dot.set__unsafe(
        rendered.storeInstance.state,
        REVIEW_EDITED_AT,
        'not-a-date',
      )
      await flush()

      const updated = rendered.storeInstance._concerns[REVIEW_EDITED_AT]
      expect(
        (updated?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(true)
    })
  })

  // =========================================================================
  // 2. DYNAMIC TEXT (headless)
  // =========================================================================

  describe('Dynamic text interpolation (headless)', () => {
    it('interpolates dynamic tooltip from deeply nested pricing data', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [PRICE_BASE_V1]: {
            dynamicTooltip: {
              template: `Base: {{${PRICE_BASE_V1}}} | Sale: {{${PRICE_SALE_V1}}}`,
            },
          },
        },
      })
      await flush()

      const concerns = rendered.storeInstance._concerns[PRICE_BASE_V1]
      const tooltip = String(concerns?.['dynamicTooltip'] ?? '')
      expect(tooltip).toBe('Base: 999 | Sale: 799')
    })

    it('interpolates dynamic label with brand and category', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [STOCK_V1]: {
            dynamicLabel: {
              template: `Stock ({{${BRAND_P1}}} {{${CATEGORY_NAME}}})`,
            },
          },
        },
      })
      await flush()

      const concerns = rendered.storeInstance._concerns[STOCK_V1]
      const label = String(concerns?.['dynamicLabel'] ?? '')
      expect(label).toBe('Stock (TechCo Smartphones)')
    })

    it('interpolates dynamic placeholder with shipping data', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [PRICE_BASE_V1]: {
            dynamicPlaceholder: {
              template: `Weight: {{${SHIPPING_WEIGHT_V1}}}kg`,
            },
          },
        },
      })
      await flush()

      const concerns = rendered.storeInstance._concerns[PRICE_BASE_V1]
      const placeholder = String(concerns?.['dynamicPlaceholder'] ?? '')
      expect(placeholder).toBe('Weight: 0.5kg')
    })
  })

  // =========================================================================
  // 3. CUSTOM CONCERNS (headless)
  // =========================================================================

  describe('Custom concerns (headless)', () => {
    it('budgetCheck concern validates pricing against department budget', async () => {
      function BudgetCheckWrapper() {
        store.useConcerns(
          'budget-check',
          {
            [PRICE_BASE_V1]: {
              budgetCheck: {
                budgetPath: BUDGET_ELECTRONICS,
                pricePath: PRICE_BASE_V1,
                quantityPath: STOCK_V1,
              },
            },
          },
          [...defaultConcerns, budgetCheck],
        )
        return <div data-testid="wrapper">budget</div>
      }

      const rendered = renderWithStore(
        <BudgetCheckWrapper />,
        store,
        makeCatalogFixture(),
      )
      await flush()

      // 999 * 100 = 99,900 vs 500,000 budget -> sufficient
      const concerns = rendered.storeInstance._concerns[PRICE_BASE_V1]
      const result = concerns?.['budgetCheck'] as {
        sufficient: boolean
        cost: number
        budget: number
      }
      expect(result.sufficient).toBe(true)
      expect(result.cost).toBe(99_900)

      // Increase price to 6000 -> 6000 * 100 = 600,000 > 500,000 -> insufficient
      dot.set__unsafe(rendered.storeInstance.state, PRICE_BASE_V1, 6000)
      await flush()

      const updated = rendered.storeInstance._concerns[PRICE_BASE_V1]
      const updatedResult = updated?.['budgetCheck'] as {
        sufficient: boolean
        cost: number
        budget: number
      }
      expect(updatedResult.sufficient).toBe(false)
      expect(updatedResult.cost).toBe(600_000)
    })

    it('stockAlert concern warns when available stock is low', async () => {
      function StockAlertWrapper() {
        store.useConcerns(
          'stock-alert',
          {
            [STOCK_P2_V1]: {
              stockAlert: {
                stockPath: STOCK_P2_V1,
                reservedPath: RESERVED_P2_V1,
                threshold: 10,
              },
            },
          },
          [stockAlert],
        )
        return <div data-testid="wrapper">stock</div>
      }

      const rendered = renderWithStore(
        <StockAlertWrapper />,
        store,
        makeCatalogFixture(),
      )
      await flush()

      // P2 v1: stock 8, reserved 2 -> available 6 < threshold 10 -> low stock
      const concerns = rendered.storeInstance._concerns[STOCK_P2_V1]
      const result = concerns?.['stockAlert'] as {
        isLowStock: boolean
        available: number
      }
      expect(result.isLowStock).toBe(true)
      expect(result.available).toBe(6)

      // Increase stock to 50 -> available 48 > 10 -> not low
      dot.set__unsafe(rendered.storeInstance.state, STOCK_P2_V1, 50)
      await flush()

      const updated = rendered.storeInstance._concerns[STOCK_P2_V1]
      const updatedResult = updated?.['stockAlert'] as {
        isLowStock: boolean
        available: number
      }
      expect(updatedResult.isLowStock).toBe(false)
    })

    it('custom concern combined with built-in concerns on same path', async () => {
      function CombinedWrapper() {
        store.useConcerns(
          'combined',
          {
            [STOCK_V1]: {
              validationState: {
                schema: z.number().nonnegative('Stock cannot be negative'),
              },
              disabledWhen: {
                condition: {
                  IS_EQUAL: [STATUS_P1, 'discontinued'],
                },
              },
              stockAlert: {
                stockPath: STOCK_V1,
                reservedPath: RESERVED_V1,
                threshold: 10,
              },
            },
          },
          [...defaultConcerns, stockAlert],
        )
        return <div data-testid="wrapper">combined</div>
      }

      const rendered = renderWithStore(
        <CombinedWrapper />,
        store,
        makeCatalogFixture(),
      )
      await flush()

      // All healthy initially: stock=100, reserved=5, available=95, status=active
      const concerns = rendered.storeInstance._concerns[STOCK_V1]
      expect(
        (concerns?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(false)
      expect(concerns?.['disabledWhen']).toBe(false)
      const alert = concerns?.['stockAlert'] as { isLowStock: boolean }
      expect(alert.isLowStock).toBe(false)

      // Set negative stock -> validation fails, stock alert still computes
      dot.set__unsafe(rendered.storeInstance.state, STOCK_V1, -5)
      await flush()

      const updated = rendered.storeInstance._concerns[STOCK_V1]
      expect(
        (updated?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(true)
    })
  })

  // =========================================================================
  // 4. DIRECT STORE INSTANCE (headless)
  // =========================================================================

  describe('Direct store instance assertions (headless)', () => {
    it('concern results accessible via _concerns proxy at deep paths', async () => {
      const rendered = renderWithStore(store, makeCatalogFixture(), {
        concerns: {
          [STATUS_P1]: {
            validationState: {
              schema: z.string().min(1, 'Status required'),
            },
            disabledWhen: {
              condition: {
                IS_EQUAL: [STATUS_P1, 'archived'],
              },
            },
          },
        },
      })
      await flush()

      // Access concern results directly
      const statusConcerns = rendered.storeInstance._concerns[STATUS_P1]
      expect(statusConcerns).toBeDefined()
      expect(
        (statusConcerns?.['validationState'] as { isError: boolean })?.isError,
      ).toBe(false)
      expect(statusConcerns?.['disabledWhen']).toBe(false)

      // Mutate state directly and re-check
      rendered.storeInstance.state.catalog.departments[
        _('electronics')
      ]!.categories[_('phones')]!.products[_('p1')]!.status = 'archived'
      await flush()

      const updatedConcerns = rendered.storeInstance._concerns[STATUS_P1]
      expect(updatedConcerns?.['disabledWhen']).toBe(true)
    })
  })

  // =========================================================================
  // 5. CONDITIONAL UI (React rendering)
  // =========================================================================

  describe('Conditional UI with BoolLogic', () => {
    it('disables price editing when product status is archived', async () => {
      function PriceDisable() {
        const statusField = store.useFieldStore(STATUS_P1)

        store.useConcerns('price-disabled', {
          [PRICE_BASE_V1]: {
            disabledWhen: {
              condition: {
                OR: [
                  { IS_EQUAL: [STATUS_P1, 'archived'] },
                  { IS_EQUAL: [STATUS_P1, 'discontinued'] },
                  { IS_EQUAL: [IS_ACTIVE_V1, false] },
                ],
              },
            },
          },
        })

        const priceField = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore(PRICE_BASE_V1)
        const isDisabled = priceField.disabledWhen === true

        return (
          <div>
            <select
              data-testid="status-select"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'active'
                    | 'archived'
                    | 'discontinued',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="active">Active</option>
              <option value="archived">Archived</option>
              <option value="discontinued">Discontinued</option>
            </select>
            <input
              data-testid="price-input"
              type="number"
              value={priceField.value}
              disabled={isDisabled}
              onChange={(e) => priceField.setValue(parseFloat(e.target.value))}
            />
            <span data-testid="disabled-state">{String(isDisabled)}</span>
          </div>
        )
      }

      renderWithStore(<PriceDisable />, store, makeCatalogFixture())
      await flush()

      // Active -> not disabled
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('false')

      // Archived -> disabled
      fireEvent.change(screen.getByTestId('status-select'), {
        target: { value: 'archived' },
      })
      await flush()
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('true')
      expect(screen.getByTestId('price-input')).toBeDisabled()

      // Back to active -> enabled
      fireEvent.change(screen.getByTestId('status-select'), {
        target: { value: 'active' },
      })
      await flush()
      expect(screen.getByTestId('disabled-state')).toHaveTextContent('false')
    })

    it('shows restock config only when alertType is not none (visibleWhen)', async () => {
      function RestockVisibility() {
        const statusField = store.useFieldStore(STATUS_P2)

        store.useConcerns('restock-vis', {
          [STOCK_P2_V1]: {
            visibleWhen: {
              condition: {
                NOT: { IS_EQUAL: [STATUS_P2, 'discontinued'] },
              },
            },
          },
        })

        const stockField = store
          .withConcerns({ visibleWhen: true })
          .useFieldStore(STOCK_P2_V1)
        const showStock = stockField.visibleWhen !== false

        return (
          <div>
            <select
              data-testid="p2-status"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'active'
                    | 'archived'
                    | 'discontinued',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="active">Active</option>
              <option value="discontinued">Discontinued</option>
            </select>
            {showStock && (
              <input
                data-testid="stock-input"
                type="number"
                value={stockField.value}
                onChange={(e) =>
                  stockField.setValue(parseFloat(e.target.value))
                }
              />
            )}
          </div>
        )
      }

      renderWithStore(<RestockVisibility />, store, makeCatalogFixture())
      await flush()

      // P2 status = draft -> visible
      expect(screen.getByTestId('stock-input')).toBeInTheDocument()

      // Set to discontinued -> hidden
      fireEvent.change(screen.getByTestId('p2-status'), {
        target: { value: 'discontinued' },
      })
      await flush()
      expect(screen.queryByTestId('stock-input')).not.toBeInTheDocument()

      // Set to active -> visible again
      fireEvent.change(screen.getByTestId('p2-status'), {
        target: { value: 'active' },
      })
      await flush()
      expect(screen.getByTestId('stock-input')).toBeInTheDocument()
    })

    it('makes fields readonly in clearance mode (readonlyWhen)', async () => {
      function ClearanceReadonly() {
        const modeField = store.useFieldStore('storeConfig.mode' as const)

        store.useConcerns('clearance-readonly', {
          [PRICE_BASE_V1]: {
            readonlyWhen: {
              condition: {
                IS_EQUAL: ['storeConfig.mode' as const, 'clearance'],
              },
            },
          },
        })

        const priceField = store
          .withConcerns({ readonlyWhen: true })
          .useFieldStore(PRICE_BASE_V1)
        const isReadonly = priceField.readonlyWhen === true

        return (
          <div>
            <select
              data-testid="mode-select"
              value={modeField.value}
              onChange={(e) =>
                modeField.setValue(
                  e.target.value as 'normal' | 'sale' | 'clearance',
                )
              }
            >
              <option value="normal">Normal</option>
              <option value="sale">Sale</option>
              <option value="clearance">Clearance</option>
            </select>
            <input
              data-testid="price-input"
              type="number"
              value={priceField.value}
              readOnly={isReadonly}
              onChange={(e) => priceField.setValue(parseFloat(e.target.value))}
            />
          </div>
        )
      }

      renderWithStore(<ClearanceReadonly />, store, makeCatalogFixture())
      await flush()

      expect(screen.getByTestId('price-input')).not.toHaveAttribute('readonly')

      fireEvent.change(screen.getByTestId('mode-select'), {
        target: { value: 'clearance' },
      })
      await flush()
      expect(screen.getByTestId('price-input')).toHaveAttribute('readonly')
    })

    it('uses complex AND/OR/NOT BoolLogic for multi-condition disable', async () => {
      function ComplexConditions() {
        const statusField = store.useFieldStore(STATUS_P1)
        const modeField = store.useFieldStore('storeConfig.mode' as const)

        // Disable stock when: (status != active) AND (mode = sale OR mode = clearance)
        store.useConcerns('complex-conditions', {
          [STOCK_V1]: {
            disabledWhen: {
              condition: {
                AND: [
                  {
                    NOT: { IS_EQUAL: [STATUS_P1, 'active'] },
                  },
                  {
                    OR: [
                      {
                        IS_EQUAL: ['storeConfig.mode' as const, 'sale'],
                      },
                      {
                        IS_EQUAL: ['storeConfig.mode' as const, 'clearance'],
                      },
                    ],
                  },
                ],
              },
            },
          },
        })

        const stockField = store
          .withConcerns({ disabledWhen: true })
          .useFieldStore(STOCK_V1)
        const isDisabled = stockField.disabledWhen === true

        return (
          <div>
            <select
              data-testid="status"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'active'
                    | 'archived'
                    | 'discontinued',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="active">Active</option>
              <option value="archived">Archived</option>
            </select>
            <select
              data-testid="mode"
              value={modeField.value}
              onChange={(e) =>
                modeField.setValue(
                  e.target.value as 'normal' | 'sale' | 'clearance',
                )
              }
            >
              <option value="normal">Normal</option>
              <option value="sale">Sale</option>
              <option value="clearance">Clearance</option>
            </select>
            <input
              data-testid="stock"
              type="number"
              value={stockField.value}
              disabled={isDisabled}
              onChange={(e) => stockField.setValue(parseFloat(e.target.value))}
            />
          </div>
        )
      }

      renderWithStore(<ComplexConditions />, store, makeCatalogFixture())
      await flush()

      // active + normal -> enabled
      expect(screen.getByTestId('stock')).not.toBeDisabled()

      // active + sale -> enabled (because status IS active, AND fails)
      fireEvent.change(screen.getByTestId('mode'), {
        target: { value: 'sale' },
      })
      await flush()
      expect(screen.getByTestId('stock')).not.toBeDisabled()

      // draft + sale -> disabled (status != active AND mode = sale)
      fireEvent.change(screen.getByTestId('status'), {
        target: { value: 'draft' },
      })
      await flush()
      expect(screen.getByTestId('stock')).toBeDisabled()

      // draft + normal -> enabled (mode condition fails)
      fireEvent.change(screen.getByTestId('mode'), {
        target: { value: 'normal' },
      })
      await flush()
      expect(screen.getByTestId('stock')).not.toBeDisabled()
    })
  })

  // =========================================================================
  // 6. SIDE EFFECTS (React rendering)
  // =========================================================================

  describe('Side effects on deep state', () => {
    it('syncs base price across variants', async () => {
      function PriceSync() {
        const [price1, setPrice1] = store.useStore(PRICE_BASE_V1)
        const [price2] = store.useStore(PRICE_BASE_V2)

        store.useSideEffects('variant-sync', {
          syncPaths: [
            typeHelpers.syncPair<EcommerceCatalog>(
              PRICE_BASE_V1,
              PRICE_BASE_V2,
            ),
          ],
        })

        return (
          <div>
            <input
              data-testid="price-v1"
              type="number"
              value={price1}
              onChange={(e) => setPrice1(parseFloat(e.target.value))}
            />
            <span data-testid="price-v2">{String(price2)}</span>
          </div>
        )
      }

      renderWithStore(<PriceSync />, store, makeCatalogFixture())
      await flush()

      const initialPrice = Number(screen.getByTestId('price-v2').textContent)

      // Change v1 price -> v2 should sync
      const newPrice = initialPrice + 100
      fireEvent.change(screen.getByTestId('price-v1'), {
        target: { value: String(newPrice) },
      })
      await flush()
      expect(Number(screen.getByTestId('price-v2').textContent)).toBe(newPrice)
    })

    it('flips isStocked/needsReorder at top level', async () => {
      function StockFlip() {
        const [isStocked, setStocked] = store.useStore('isStocked')
        const [needsReorder] = store.useStore('needsReorder')

        store.useSideEffects('stock-flip', {
          flipPaths: [
            typeHelpers.flipPair<EcommerceCatalog>('isStocked', 'needsReorder'),
          ],
        })

        return (
          <div>
            <button
              data-testid="toggle-stock"
              onClick={() => setStocked(false)}
            >
              Mark Unstocked
            </button>
            <span data-testid="stocked">{String(isStocked)}</span>
            <span data-testid="reorder">{String(needsReorder)}</span>
          </div>
        )
      }

      renderWithStore(<StockFlip />, store, makeCatalogFixture())
      await flush()

      // Initially stocked
      expect(screen.getByTestId('stocked')).toHaveTextContent('true')
      expect(screen.getByTestId('reorder')).toHaveTextContent('false')

      // Toggle -> flip
      fireEvent.click(screen.getByTestId('toggle-stock'))
      await flush()
      expect(screen.getByTestId('stocked')).toHaveTextContent('false')
      expect(screen.getByTestId('reorder')).toHaveTextContent('true')
    })

    it('listener computes total revenue from variant pricing changes', async () => {
      function RevenueListener() {
        const storeInstance = useStoreContext<EcommerceCatalog>()
        const [basePrice, setBasePrice] = store.useStore(PRICE_BASE_V1)
        const [totalRevenue] = store.useStore('totalRevenue' as const)

        useLayoutEffect(() => {
          const cleanup = registerListener(storeInstance, {
            path: PRICING_V1,
            scope: null,
            fn: (changes, state) => {
              // State is full pre-change snapshot (scope: null)
              const variants =
                state?.catalog?.departments?.[_('electronics')]?.categories?.[
                  _('phones')
                ]?.products?.[_('p1')]?.variants
              const v2Base = variants?.[_('v2')]?.pricing?.base ?? 1099
              let v1Base = variants?.[_('v1')]?.pricing?.base ?? 999
              for (const change of changes) {
                if (change[0] === 'base') v1Base = Number(change[1])
              }
              return typeHelpers.changes<EcommerceCatalog>([
                ['totalRevenue' as const, v1Base + v2Base],
              ])
            },
          })
          return cleanup
        }, [storeInstance])

        return (
          <div>
            <input
              data-testid="v1-price"
              type="number"
              value={basePrice}
              onChange={(e) => setBasePrice(parseFloat(e.target.value))}
            />
            <span data-testid="total-revenue">{String(totalRevenue)}</span>
          </div>
        )
      }

      renderWithStore(<RevenueListener />, store, makeCatalogFixture())
      await flush()

      // Change v1 base price to 1500
      fireEvent.change(screen.getByTestId('v1-price'), {
        target: { value: '1500' },
      })
      await flush()

      // Total revenue should be 1500 + 1099 = 2599
      const totalRevenue = parseFloat(
        screen.getByTestId('total-revenue').textContent || '0',
      )
      expect(totalRevenue).toBeCloseTo(2599, 0)
    })
  })

  // =========================================================================
  // 7. JIT STORE - Batch updates
  // =========================================================================

  describe('JitStore batch updates', () => {
    it('applies batch changes across multiple deeply nested paths', async () => {
      function BatchUpdater() {
        const { setChanges } = store.useJitStore()
        const [price] = store.useStore(PRICE_BASE_V1)
        const [stock] = store.useStore(STOCK_V1)
        const [status] = store.useStore(STATUS_P1)

        const handleBatchUpdate = () => {
          setChanges(
            typeHelpers.changes<EcommerceCatalog>([
              [PRICE_BASE_V1, 1299],
              [STOCK_V1, 200],
              [STATUS_P1, 'draft'],
            ]),
          )
        }

        return (
          <div>
            <button data-testid="batch-update" onClick={handleBatchUpdate}>
              Batch
            </button>
            <span data-testid="price">{String(price)}</span>
            <span data-testid="stock">{String(stock)}</span>
            <span data-testid="status">{String(status)}</span>
          </div>
        )
      }

      renderWithStore(<BatchUpdater />, store, makeCatalogFixture())

      fireEvent.click(screen.getByTestId('batch-update'))
      await flush()

      expect(screen.getByTestId('price')).toHaveTextContent('1299')
      expect(screen.getByTestId('stock')).toHaveTextContent('200')
      expect(screen.getByTestId('status')).toHaveTextContent('draft')
    })
  })

  // =========================================================================
  // 8. COMBINED: concerns + side effects + deep nesting
  // =========================================================================

  describe('Combined concerns and side effects', () => {
    it('validation + sync + conditional UI work together on variants', async () => {
      function VariantForm() {
        const [price1, setPrice1] = store.useStore(PRICE_BASE_V1)
        const [price2] = store.useStore(PRICE_BASE_V2)
        const statusField = store.useFieldStore(STATUS_P1)

        // Sync prices
        store.useSideEffects('variant-effects', {
          syncPaths: [
            typeHelpers.syncPair<EcommerceCatalog>(
              PRICE_BASE_V1,
              PRICE_BASE_V2,
            ),
          ],
        })

        // Validate + disable
        store.useConcerns('variant-concerns', {
          [PRICE_BASE_V1]: {
            validationState: {
              schema: z
                .number()
                .positive('Price must be positive')
                .max(5000, 'Price unreasonable'),
            },
            disabledWhen: {
              condition: {
                OR: [
                  { IS_EQUAL: [STATUS_P1, 'archived'] },
                  { IS_EQUAL: [STATUS_P1, 'discontinued'] },
                ],
              },
            },
            dynamicTooltip: {
              template: `Price for {{${BRAND_P1}}} (weight: {{${SHIPPING_WEIGHT_V1}}}kg)`,
            },
          },
        })

        const priceField = store
          .withConcerns({
            validationState: true,
            disabledWhen: true,
            dynamicTooltip: true,
          })
          .useFieldStore(PRICE_BASE_V1)
        const validation = priceField.validationState
        const disabled = priceField.disabledWhen === true
        const tooltip = priceField.dynamicTooltip ?? ''

        return (
          <div>
            <select
              data-testid="status"
              value={statusField.value}
              onChange={(e) =>
                statusField.setValue(
                  e.target.value as
                    | 'draft'
                    | 'active'
                    | 'archived'
                    | 'discontinued',
                )
              }
            >
              <option value="draft">Draft</option>
              <option value="active">Active</option>
              <option value="archived">Archived</option>
              <option value="discontinued">Discontinued</option>
            </select>
            <input
              data-testid="price-v1"
              type="number"
              value={price1}
              disabled={disabled}
              onChange={(e) => setPrice1(parseFloat(e.target.value))}
            />
            <span data-testid="price-v2">{String(price2)}</span>
            <span data-testid="tooltip">{tooltip}</span>
            {validation?.isError && (
              <span data-testid="validation-error">
                {validation.errors[0]?.message}
              </span>
            )}
          </div>
        )
      }

      renderWithStore(<VariantForm />, store, makeCatalogFixture())
      await flush()

      // Initial: valid, enabled, synced, tooltip populated
      expect(screen.queryByTestId('validation-error')).not.toBeInTheDocument()
      expect(screen.getByTestId('price-v1')).not.toBeDisabled()
      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Price for TechCo (weight: 0.5kg)',
      )

      // Change price -> syncs to v2, validates
      fireEvent.change(screen.getByTestId('price-v1'), {
        target: { value: '1250' },
      })
      await flush()
      expect(screen.getByTestId('price-v2')).toHaveTextContent('1250')
      expect(screen.queryByTestId('validation-error')).not.toBeInTheDocument()

      // Set invalid price -> validation error, but still syncs
      fireEvent.change(screen.getByTestId('price-v1'), {
        target: { value: '-1' },
      })
      await flush()
      expect(screen.getByTestId('validation-error')).toHaveTextContent(
        'Price must be positive',
      )
      expect(screen.getByTestId('price-v2')).toHaveTextContent('-1')

      // Archive product -> price disabled
      fireEvent.change(screen.getByTestId('status'), {
        target: { value: 'archived' },
      })
      await flush()
      expect(screen.getByTestId('price-v1')).toBeDisabled()
    })

    it('re-evaluates all concerns when deep state changes propagate', async () => {
      function PropagationTest() {
        const shippingField = store.useFieldStore(SHIPPING_DIM_LENGTH_V1)

        store.useConcerns('propagation', {
          [PRICE_BASE_V1]: {
            dynamicTooltip: {
              template: `Ship length: {{${SHIPPING_DIM_LENGTH_V1}}}cm`,
            },
            disabledWhen: {
              condition: {
                GT: [SHIPPING_DIM_LENGTH_V1, 50],
              },
            },
          },
        })

        const priceField = store
          .withConcerns({
            dynamicTooltip: true,
            disabledWhen: true,
          })
          .useFieldStore(PRICE_BASE_V1)
        const tooltip = priceField.dynamicTooltip ?? ''
        const disabled = priceField.disabledWhen === true

        return (
          <div>
            <input
              data-testid="ship-length"
              type="number"
              value={shippingField.value}
              onChange={(e) =>
                shippingField.setValue(parseFloat(e.target.value))
              }
            />
            <span data-testid="tooltip">{tooltip}</span>
            <span data-testid="disabled">{String(disabled)}</span>
          </div>
        )
      }

      renderWithStore(<PropagationTest />, store, makeCatalogFixture())
      await flush()

      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Ship length: 15cm',
      )
      expect(screen.getByTestId('disabled')).toHaveTextContent('false')

      // Move shipping length above threshold
      fireEvent.change(screen.getByTestId('ship-length'), {
        target: { value: '60' },
      })
      await flush()

      expect(screen.getByTestId('tooltip')).toHaveTextContent(
        'Ship length: 60cm',
      )
      expect(screen.getByTestId('disabled')).toHaveTextContent('true')
    })

    it('state mutations at deep level are tracked by valtio', async () => {
      function DeepMutationTracker() {
        const reviewField = store.useFieldStore(REVIEW_HELPFUL_VOTES)

        return (
          <div>
            <span data-testid="votes">{reviewField.value}</span>
            <button
              data-testid="add-vote"
              onClick={() => reviewField.setValue(43)}
            >
              Add Vote
            </button>
          </div>
        )
      }

      const result = renderWithStore(
        <DeepMutationTracker />,
        store,
        makeCatalogFixture(),
      )

      expect(screen.getByTestId('votes')).toHaveTextContent('42')

      fireEvent.click(screen.getByTestId('add-vote'))
      await flush()

      expect(screen.getByTestId('votes')).toHaveTextContent('43')

      // Verify state proxy reflects the change
      const votes =
        result.storeInstance.state.catalog.departments[_('electronics')]
          ?.categories?.[_('phones')]?.products?.[_('p1')]?.variants?.[_('v1')]
          ?.reviews?.[_('r1')]?.metadata?.helpfulVotes
      expect(votes).toBe(43)
    })
  })
})
