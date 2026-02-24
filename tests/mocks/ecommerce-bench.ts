/**
 * E-commerce Benchmark Fixture
 *
 * Deep hashmap structure: departments[dept_X].products[p_X].variants[v_X]
 * 3 departments × 5 products × 4 variants = 60 variants, each with rich data.
 * Plus 10 orders, settings, dashboard. ~800+ fields total.
 *
 * 3 dynamic Record layers on the path to each variant:
 *   catalog.departments.dept_0.products.p_0.variants.v_0.price.base
 *                       ^^^^^^           ^^^^            ^^^^
 *                       Record 1         Record 2        Record 3
 */

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Shared registration builders & scenarios (used by bench + parity test)
// ---------------------------------------------------------------------------

import type {
  ArrayOfChanges,
  ConcernRegistrationMap,
  GenericMeta,
} from '~/types'
import type { SideEffects } from '~/types/side-effects'

const DEPT_COUNT = 3
const PRODUCTS_PER_DEPT = 5
const VARIANTS_PER_PRODUCT = 4
const ORDER_COUNT = 10

// Total variants: 3 × 5 × 4 = 60

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface VariantMetadata {
  createdAt: string
  updatedAt: string
  tags: string[]
  origin: string
  season: string
}

interface VariantPrice {
  base: number
  sale: number
  wholesale: number
  currency: string
  taxRate: number
  margin: number
}

interface VariantInventory {
  inStock: number
  reserved: number
  warehouse: string
  reorderPoint: number
  isLowStock: boolean
}

interface VariantShipping {
  weight: number
  dimensions: { l: number; w: number; h: number }
  freeShipping: boolean
  estimatedDays: number
  carrier: string
}

interface Variant {
  sku: string
  color: string
  size: string
  isActive: boolean
  isDiscontinued: boolean
  metadata: VariantMetadata
  price: VariantPrice
  inventory: VariantInventory
  shipping: VariantShipping
}

interface Product {
  name: string
  sku: string
  status: 'active' | 'draft' | 'archived'
  brand: string
  description: string
  variants: Record<string, Variant>
}

export interface Department {
  name: string
  slug: string
  isActive: boolean
  isDiscontinued: boolean
  productCount: number
  products: Record<string, Product>
}

interface OrderItem {
  productId: string
  variantId: string
  quantity: number
  unitPrice: number
  total: number
}

export interface Order {
  status: 'pending' | 'confirmed' | 'shipped' | 'delivered'
  customerId: string
  total: number
  isPaid: boolean
  isShipped: boolean
  needsRefund: boolean
  items: Record<string, OrderItem>
}

export interface EcommerceBenchState {
  catalog: {
    departments: Record<string, Department>
  }
  orders: Record<string, Order>
  settings: {
    currency: string
    taxEnabled: boolean
    freeShippingThreshold: number
  }
  dashboard: {
    totalRevenue: number
    totalOrders: number
    avgOrderValue: number
    lowStockCount: number
  }
}

// ---------------------------------------------------------------------------
// Path helpers
// ---------------------------------------------------------------------------

/** dept/product/variant path prefix */
const vp = (d: number, p: number, v: number): string =>
  `catalog.departments.dept_${d}.products.p_${p}.variants.v_${v}`

/** dept/product path prefix */
const pp = (d: number, p: number): string =>
  `catalog.departments.dept_${d}.products.p_${p}`

/** dept path prefix */
const dp = (d: number): string => `catalog.departments.dept_${d}`

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

const COLORS = ['red', 'blue', 'green', 'black']
const SIZES = ['XS', 'S', 'M', 'L']
const CARRIERS = ['UPS', 'FedEx', 'DHL', 'USPS']
const WAREHOUSES = ['WH-EAST', 'WH-WEST', 'WH-CENTRAL', 'WH-SOUTH']
const SEASONS = ['spring', 'summer', 'fall', 'winter']

const buildVariant = (d: number, p: number, v: number): Variant => ({
  sku: `SKU-D${d}-P${p}-V${v}`,
  color: COLORS[v % COLORS.length]!,
  size: SIZES[v % SIZES.length]!,
  isActive: true,
  isDiscontinued: false,
  metadata: {
    createdAt: '2025-01-01T00:00:00Z',
    updatedAt: '2025-06-15T12:00:00Z',
    tags: ['bestseller', `dept-${d}`, `cat-${p % 3}`],
    origin: ['US', 'CN', 'DE', 'JP'][v % 4]!,
    season: SEASONS[v % SEASONS.length]!,
  },
  price: {
    // base: synced across variants in same product (v_0 → v_1/v_2/v_3), so must not vary by v
    base: 29.99 + d * 20 + p * 5,
    sale: 24.99 + d * 15 + p * 4 + v,
    wholesale: 19.99 + d * 10 + p * 3 + v,
    currency: 'USD',
    // taxRate: synced across depts (dept_0 → dept_1 → dept_2) for v_0, so must not vary by d
    taxRate: 0.08,
    margin: 0.3 + v * 0.05,
  },
  inventory: {
    inStock: 100 + d * 50 + p * 10 + v * 5,
    reserved: d + p + v,
    // warehouse: synced across depts (dept_0 → dept_2) for v_0, so must not vary by d
    warehouse: WAREHOUSES[(p + v) % WAREHOUSES.length]!,
    reorderPoint: 20 + v * 5,
    isLowStock: false,
  },
  shipping: {
    weight: 0.5 + d * 0.3 + p * 0.1 + v * 0.05,
    dimensions: { l: 10 + p, w: 8 + v, h: 4 + d },
    freeShipping: (d + p + v) % 3 === 0,
    estimatedDays: 2 + ((d + p) % 5),
    carrier: CARRIERS[(d + p + v) % CARRIERS.length]!,
  },
})

const buildProduct = (d: number, p: number): Product => {
  const variants: Record<string, Variant> = {}
  for (let v = 0; v < VARIANTS_PER_PRODUCT; v++) {
    variants[`v_${v}`] = buildVariant(d, p, v)
  }
  return {
    name: `Dept${d} Product ${p}`,
    sku: `SKU-D${d}-P${p}`,
    status: 'active',
    brand: `Brand ${(d * PRODUCTS_PER_DEPT + p) % 5}`,
    description: `Product ${p} in department ${d}`,
    variants,
  }
}

const buildDepartment = (d: number): Department => {
  const deptNames = ['Electronics', 'Clothing', 'Home & Garden']
  const products: Record<string, Product> = {}
  for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
    products[`p_${p}`] = buildProduct(d, p)
  }
  return {
    name: deptNames[d % deptNames.length]!,
    slug: deptNames[d % deptNames.length]!.toLowerCase().replace(/\s+/g, '-'),
    isActive: true,
    isDiscontinued: false,
    productCount: PRODUCTS_PER_DEPT,
    products,
  }
}

const buildOrders = (): Record<string, Order> => {
  const orders: Record<string, Order> = {}
  for (let i = 0; i < ORDER_COUNT; i++) {
    const items: Record<string, OrderItem> = {}
    for (let j = 0; j < 3; j++) {
      items[`item_${j}`] = {
        productId: `p_${j}`,
        variantId: `v_${j % VARIANTS_PER_PRODUCT}`,
        quantity: 1 + j,
        unitPrice: 49.99 + j * 10,
        total: (1 + j) * (49.99 + j * 10),
      }
    }
    orders[`o_${i}`] = {
      status: 'pending',
      customerId: `cust_${i}`,
      total: Object.values(items).reduce((s, it) => s + it.total, 0),
      isPaid: false,
      isShipped: false,
      needsRefund: false,
      items,
    }
  }
  return orders
}

// ---------------------------------------------------------------------------
// Static fixture builder
// ---------------------------------------------------------------------------

export const buildEcommerceState = (): EcommerceBenchState => {
  const departments: Record<string, Department> = {}
  for (let d = 0; d < DEPT_COUNT; d++) {
    departments[`dept_${d}`] = buildDepartment(d)
  }
  return {
    catalog: { departments },
    orders: buildOrders(),
    settings: {
      currency: 'USD',
      taxEnabled: true,
      freeShippingThreshold: 99.99,
    },
    dashboard: {
      totalRevenue: 125000,
      totalOrders: ORDER_COUNT,
      avgOrderValue: 12500,
      lowStockCount: 0,
    },
  }
}

// ---------------------------------------------------------------------------
// Registration definitions
// ---------------------------------------------------------------------------

/**
 * ~75 sync pairs across all 3 Record layers:
 * - 15 currency syncs: settings.currency → each product's v_0 price.currency
 * - 45 intra-product variant price syncs: v_0.price.base → v_1/v_2/v_3.price.base (per product)
 * - 15 cross-dept taxRate syncs: dept_0.p_X.v_0.price.taxRate → dept_1.p_X.v_0.price.taxRate
 */
const SYNC_PAIRS: [string, string][] = (() => {
  const pairs: [string, string][] = []

  // 15 currency syncs: settings → product v_0 (3 depts × 5 products)
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      pairs.push(['settings.currency', `${vp(d, p, 0)}.price.currency`])
    }
  }

  // 45 intra-product variant price syncs: v_0.price.base → v_1/v_2/v_3.price.base
  // Within each product, variant 0 is the "leader" — all others sync from it
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      for (let v = 1; v < VARIANTS_PER_PRODUCT; v++) {
        pairs.push([`${vp(d, p, 0)}.price.base`, `${vp(d, p, v)}.price.base`])
      }
    }
  }

  // 15 cross-dept taxRate syncs: dept_0 → dept_1 and dept_1 → dept_2 for matching products
  for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
    pairs.push([`${vp(0, p, 0)}.price.taxRate`, `${vp(1, p, 0)}.price.taxRate`])
    pairs.push([`${vp(1, p, 0)}.price.taxRate`, `${vp(2, p, 0)}.price.taxRate`])
    // Cross-dept warehouse sync for first variant
    pairs.push([
      `${vp(0, p, 0)}.inventory.warehouse`,
      `${vp(2, p, 0)}.inventory.warehouse`,
    ])
  }

  return pairs
})()

/**
 * ~40 flip pairs across all 3 Record layers:
 * - 5 order flips: isPaid ↔ needsRefund
 * - 15 variant flips: isActive ↔ isDiscontinued (3 depts × 5 products × v_0)
 * - 15 variant inventory flips: isLowStock ↔ freeShipping
 * - 3 department flips: isActive ↔ isDiscontinued
 */
const FLIP_PAIRS: [string, string][] = (() => {
  const pairs: [string, string][] = []

  // 5 order flips: isPaid ↔ needsRefund
  for (let i = 0; i < 5; i++) {
    pairs.push([`orders.o_${i}.isPaid`, `orders.o_${i}.needsRefund`])
  }

  // 15 variant flips: isActive ↔ isDiscontinued (v_0 of each product)
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      pairs.push([`${vp(d, p, 0)}.isActive`, `${vp(d, p, 0)}.isDiscontinued`])
    }
  }

  // 15 variant inventory flips: isLowStock ↔ freeShipping (v_1 of each product)
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      pairs.push([
        `${vp(d, p, 1)}.inventory.isLowStock`,
        `${vp(d, p, 1)}.shipping.freeShipping`,
      ])
    }
  }

  // 3 department flips: isActive ↔ isDiscontinued
  for (let d = 0; d < DEPT_COUNT; d++) {
    pairs.push([`${dp(d)}.isActive`, `${dp(d)}.isDiscontinued`])
  }

  return pairs
})()

/**
 * 100 BoolLogic conditions across deep hashmap paths.
 * - 60: variant price.base disabledWhen product status === 'archived' (all 60 variants)
 * - 30: variant visibleWhen product is active AND variant inventory exists
 * - 10: variant shipping disabledWhen variant isLowStock
 */
const BOOL_LOGIC_REGISTRATIONS = (() => {
  const regs: { outputPath: string; tree: unknown }[] = []

  // 60: all variants — disabledWhen product archived
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      for (let v = 0; v < VARIANTS_PER_PRODUCT; v++) {
        regs.push({
          outputPath: `_concerns.${vp(d, p, v)}.price.base.disabledWhen`,
          tree: {
            IS_EQUAL: [`${pp(d, p)}.status`, 'archived'],
          },
        })
      }
    }
  }

  // 30: v_0 and v_1 of each product — visibleWhen
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      for (let v = 0; v < 2; v++) {
        regs.push({
          outputPath: `_concerns.${vp(d, p, v)}.visibleWhen`,
          tree: {
            AND: [
              { IS_EQUAL: [`${pp(d, p)}.status`, 'active'] },
              { EXISTS: `${vp(d, p, v)}.inventory.inStock` },
            ],
          },
        })
      }
    }
  }

  // 10: v_2 shipping disabledWhen isLowStock (pad to 100)
  for (let d = 0; d < DEPT_COUNT && regs.length < 100; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT && regs.length < 100; p++) {
      regs.push({
        outputPath: `_concerns.${vp(d, p, 2)}.shipping.freeShipping.disabledWhen`,
        tree: {
          IS_EQUAL: [`${vp(d, p, 2)}.inventory.isLowStock`, true],
        },
      })
    }
  }

  return regs
})()

/**
 * 85 listener entries: 60 variant + 10 order + 5 dashboard + 10 catalog-root.
 * Variant listeners traverse all 3 Record layers.
 * Catalog-root listeners fire on every catalog change (14/16 scenarios).
 *
 * NOTE: True root listeners (path='') only see top-level changes in Legacy
 * (no dots in path), so we use 'catalog' as a high-level ancestor instead.
 * This ensures they fire in both Legacy and WASM for most scenarios.
 */
const CATALOG_ROOT_LISTENER_COUNT = 10

const LISTENER_ENTRIES = (() => {
  const entries: {
    subscriber_id: number
    topic_path: string
    scope_path: string
  }[] = []
  let id = 0

  // 60 variant listeners (one per variant, 3 Records deep)
  for (let d = 0; d < DEPT_COUNT; d++) {
    for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
      for (let v = 0; v < VARIANTS_PER_PRODUCT; v++) {
        entries.push({
          subscriber_id: id++,
          topic_path: vp(d, p, v),
          scope_path: vp(d, p, v),
        })
      }
    }
  }

  // 10 order listeners
  for (let i = 0; i < ORDER_COUNT; i++) {
    entries.push({
      subscriber_id: id++,
      topic_path: `orders.o_${i}`,
      scope_path: `orders.o_${i}`,
    })
  }

  // 5 dashboard aggregation listeners — each scoped to its own order to avoid
  // multiple listeners writing to the same path (order-dependent last-write-wins)
  for (let i = 0; i < 5; i++) {
    entries.push({
      subscriber_id: id++,
      topic_path: `orders.o_${i * 2}`,
      scope_path: `orders.o_${i * 2}`,
    })
  }

  // 10 catalog-root listeners: fire on every catalog.* change
  // Each produces changes that cascade into subsequent listeners' scope.
  // Tests overhead of broad listeners + cascading propagation.
  for (let i = 0; i < CATALOG_ROOT_LISTENER_COUNT; i++) {
    entries.push({
      subscriber_id: id++,
      topic_path: 'catalog',
      scope_path: 'catalog',
    })
  }

  return entries
})()

/**
 * Listener handler factories.
 * Variant listeners return price recalculation changes.
 * Order listeners return total + dashboard updates.
 */
const makeListenerHandler =
  (entry: (typeof LISTENER_ENTRIES)[number]) =>
  (
    _changes: [string, unknown, GenericMeta][],
  ): [string, unknown, Record<string, never>][] => {
    // Deterministic values derived from subscriber_id to ensure
    // Legacy and WASM produce identical results for parity testing
    const id = entry.subscriber_id

    // Variant listener: recalc sale price from base
    if (entry.topic_path.includes('.variants.')) {
      return [
        [`${entry.scope_path}.price.sale`, 10 + ((id * 7.3) % 50), {}],
        [`${entry.scope_path}.price.margin`, 0.1 + ((id * 0.037) % 0.3), {}],
      ]
    }
    // Order listener: recalculate total
    // Each listener writes to its own scoped path to avoid order-dependent last-write-wins
    if (entry.topic_path.startsWith('orders.')) {
      return [[`${entry.scope_path}.total`, (id * 73.7) % 1000, {}]]
    }
    // Catalog-root listener: produces cascading catalog changes.
    // Listener output paths are ABSOLUTE (both Legacy and WASM take them as-is).
    // Each writes to 2 unique catalog paths, creating 20 additional state
    // changes per trigger. Writes go back into catalog.* namespace so
    // subsequent catalog root listeners accumulate more work via propagation.
    // Paths are unique per listener (subscriber_id-based) to avoid conflicts.
    if (entry.topic_path === 'catalog') {
      const catalogIdx = id - 75 // catalog listeners start after 60 variant + 10 order + 5 dashboard
      const targetD = catalogIdx % DEPT_COUNT
      const targetP = catalogIdx % PRODUCTS_PER_DEPT
      return [
        [
          `catalog.departments.dept_${targetD}.products.p_${targetP}.variants.v_3.metadata.season`,
          `listener-${catalogIdx}`,
          {},
        ],
        [
          `catalog.departments.dept_${targetD}.productCount`,
          100 + catalogIdx,
          {},
        ],
      ]
    }
    return []
  }

type Changes = ArrayOfChanges<EcommerceBenchState, GenericMeta>

const c = (path: string, value: unknown): [string, unknown, GenericMeta] =>
  [path, value, {}] as [string, unknown, GenericMeta]

export const buildConcernRegistrations =
  (): ConcernRegistrationMap<EcommerceBenchState> => {
    const map: Record<string, Record<string, unknown>> = {}
    for (const reg of BOOL_LOGIC_REGISTRATIONS) {
      const withoutPrefix = reg.outputPath.replace('_concerns.', '')
      const parts = withoutPrefix.split('.')
      const concernName = parts.pop()!
      const basePath = parts.join('.')
      if (!map[basePath]) map[basePath] = {}
      map[basePath]![concernName] = { boolLogic: reg.tree }
    }
    return map as ConcernRegistrationMap<EcommerceBenchState>
  }

export const buildSideEffects = (): SideEffects<
  EcommerceBenchState,
  GenericMeta
> => ({
  syncPaths: SYNC_PAIRS as any,
  flipPaths: FLIP_PAIRS as any,
  listeners: LISTENER_ENTRIES.map((entry) => ({
    path: entry.topic_path as any,
    scope: entry.scope_path as any,
    fn: makeListenerHandler(entry) as any,
  })),
})

export const SCENARIOS: { name: string; trigger: () => Changes }[] = [
  {
    name: 'S01: Single variant price edit (3 Records deep)',
    trigger: () => [c(`${vp(0, 0, 0)}.price.base`, 59.99)] as Changes,
  },
  {
    name: 'S02: Batch variant update (3 fields, 3 Records)',
    trigger: () =>
      [
        c(`${vp(0, 2, 1)}.price.base`, 79.99),
        c(`${vp(0, 2, 1)}.price.sale`, 69.99),
        c(`${vp(0, 2, 1)}.inventory.inStock`, 200),
      ] as Changes,
  },
  {
    name: 'S03: Currency propagation (75 sync paths)',
    trigger: () => [c('settings.currency', 'EUR')] as Changes,
  },
  {
    name: 'S04: Order confirmation (flip + listeners)',
    trigger: () => [c('orders.o_0.isPaid', true)] as Changes,
  },
  {
    name: 'S05: Variant restock (60 variants, 3 Records)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < DEPT_COUNT; d++)
        for (let p = 0; p < PRODUCTS_PER_DEPT; p++)
          for (let v = 0; v < VARIANTS_PER_PRODUCT; v++)
            changes.push(c(`${vp(d, p, v)}.inventory.inStock`, 500))
      return changes as Changes
    },
  },
  {
    name: 'S06: Variant toggle (10 flips, 3 Records)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let i = 0; i < 10; i++) {
        const d = Math.floor(i / PRODUCTS_PER_DEPT) % DEPT_COUNT
        const p = i % PRODUCTS_PER_DEPT
        changes.push(c(`${vp(d, p, 0)}.isActive`, false))
      }
      return changes as Changes
    },
  },
  {
    name: 'S07: Full checkout workflow (5 changes)',
    trigger: () =>
      [
        c('orders.o_0.status', 'confirmed'),
        c('orders.o_0.isPaid', true),
        c('orders.o_0.isShipped', true),
        c('orders.o_0.items.item_0.quantity', 3),
        c('orders.o_0.total', 299.97),
      ] as Changes,
  },
  {
    name: 'S08: Dashboard aggregation (10 orders → listeners)',
    trigger: () =>
      Array.from({ length: ORDER_COUNT }, (_, i) =>
        c(`orders.o_${i}.total`, 100 + i * 50),
      ) as Changes,
  },
  {
    name: 'S09: Product archive (6 status → BoolLogic)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < DEPT_COUNT; d++)
        for (let p = 0; p < 2; p++)
          changes.push(c(`${pp(d, p)}.status`, 'archived'))
      return changes as Changes
    },
  },
  {
    name: 'S10: Deep variant metadata (7 nested writes)',
    trigger: () =>
      [
        c(`${vp(1, 2, 3)}.metadata.updatedAt`, '2026-02-16T18:00:00Z'),
        c(`${vp(1, 2, 3)}.metadata.origin`, 'UK'),
        c(`${vp(1, 2, 3)}.metadata.season`, 'winter'),
        c(`${vp(1, 2, 3)}.price.base`, 89.99),
        c(`${vp(1, 2, 3)}.price.sale`, 74.99),
        c(`${vp(1, 2, 3)}.inventory.inStock`, 300),
        c(`${vp(1, 2, 3)}.shipping.estimatedDays`, 2),
      ] as Changes,
  },
  {
    name: 'S11: Bulk variant prices (60 changes, 3 Records)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < DEPT_COUNT; d++)
        for (let p = 0; p < PRODUCTS_PER_DEPT; p++)
          for (let v = 0; v < VARIANTS_PER_PRODUCT; v++)
            changes.push(c(`${vp(d, p, v)}.price.base`, 99.99 + d + p + v))
      return changes as Changes
    },
  },
  {
    name: 'S12: Department toggle + flip (3 dept flips)',
    trigger: () =>
      Array.from({ length: DEPT_COUNT }, (_, d) =>
        c(`${dp(d)}.isActive`, false),
      ) as Changes,
  },
  {
    name: 'S13: Variant shipping recalc (15 changes)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < DEPT_COUNT; d++)
        for (let p = 0; p < PRODUCTS_PER_DEPT; p++)
          changes.push(c(`${vp(d, p, 0)}.shipping.weight`, 2.5 + d + p * 0.3))
      return changes as Changes
    },
  },
  {
    name: 'S14: Multi-order confirm (5 orders × 2 fields)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let i = 0; i < 5; i++) {
        changes.push(c(`orders.o_${i}.status`, 'confirmed'))
        changes.push(c(`orders.o_${i}.isPaid`, true))
      }
      return changes as Changes
    },
  },
  {
    name: 'S15: Cross-dept variant prices (40 changes)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < 2; d++)
        for (let p = 0; p < PRODUCTS_PER_DEPT; p++)
          for (let v = 0; v < VARIANTS_PER_PRODUCT; v++)
            changes.push(c(`${vp(d, p, v)}.price.sale`, 19.99 + d * 10 + p + v))
      return changes as Changes
    },
  },
  {
    name: 'S16: Full catalog refresh (135 changes, everything fires)',
    trigger: () => {
      const changes: [string, unknown, GenericMeta][] = []
      for (let d = 0; d < DEPT_COUNT; d++)
        for (let p = 0; p < PRODUCTS_PER_DEPT; p++) {
          changes.push(c(`${pp(d, p)}.status`, 'active'))
          for (let v = 0; v < VARIANTS_PER_PRODUCT; v++) {
            changes.push(
              c(`${vp(d, p, v)}.price.base`, 49.99 + d * 20 + p * 5 + v * 2),
            )
            changes.push(
              c(`${vp(d, p, v)}.inventory.inStock`, 200 + d * 50 + p * 10),
            )
          }
        }
      return changes as Changes
    },
  },
]
