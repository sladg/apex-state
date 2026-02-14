/**
 * Change Processing Pipeline Performance Benchmark
 *
 * Tests the performance of the change normalization and processing pipeline.
 * Key areas:
 * - normalizeChangesForGroups: path matching and change normalization
 * - processChanges: full pipeline with 4 sequential processors
 *
 * Scenarios:
 * - Single path change (baseline)
 * - Batch changes with sync/flip relationships
 * - Deep nested object paths
 * - Large pathGroups with many connected paths
 */

import { proxy } from 'valtio/vanilla'
import { bench, describe } from 'vitest'

import { normalizeChangesForGroups } from '~/_internal/pipeline/normalizeChanges'
import { processChanges } from '~/_internal/pipeline/processChanges'
import { createTiming } from '~/_internal/utils/timing'
import type { StoreInstance } from '~/core/types'
import type { GenericMeta } from '~/types'
import { addEdge, createGraph } from '~/utils/graph'

import { typeHelpers } from '../mocks/helpers'

/** Benchmark state type - represents dynamic nested paths used in benchmarks */
type BenchmarkState = Record<string, unknown>

// =============================================================================
// Mock Store Factory
// =============================================================================

const createMockStore = (
  syncPaths: [string, string][] = [],
  flipPaths: [string, string][] = [],
): StoreInstance<BenchmarkState, GenericMeta> => {
  const syncGraph = createGraph()
  const flipGraph = createGraph()

  for (const [path1, path2] of syncPaths) {
    addEdge(syncGraph, path1, path2)
  }

  for (const [path1, path2] of flipPaths) {
    addEdge(flipGraph, path1, path2)
  }

  return {
    state: proxy({ root: {} }),
    _concerns: proxy({}),
    _internal: {
      graphs: {
        sync: syncGraph,
        flip: flipGraph,
        topicRouter: {
          topics: [],
          topicMeta: new Map(),
          subscribers: new Map(),
          subscriberMeta: new Map(),
          routes: new Map(),
          handlers: new Map(),
        },
      },
      registrations: {
        concerns: new Map(),
        effectCleanups: new Set(),
        sideEffectCleanups: new Map(),
        aggregations: new Map(),
      },
      processing: {
        queue: [],
      },
      timing: createTiming({ timing: false, timingThreshold: 16 }),
      config: {
        errorStorePath: '_errors',
        maxIterations: 100,
        debug: {
          timing: false,
          timingThreshold: 16,
        },
      },
    },
  } as StoreInstance<BenchmarkState, GenericMeta>
}

// =============================================================================
// Test Fixtures
// =============================================================================

describe('Change Processing Pipeline Benchmarks', () => {
  // Generate a realistic e-commerce product structure with sync relationships
  const generateEcommercePaths = (productCount: number): [string, string][] => {
    const paths: [string, string][] = []

    for (let i = 0; i < productCount; i++) {
      const productId = `product_${i}`

      // Price sync: base price → discounted price
      paths.push([
        `products.${productId}.pricing.basePrice`,
        `products.${productId}.pricing.discountedPrice`,
      ])

      // Inventory sync: stock → availability
      paths.push([
        `products.${productId}.inventory.quantity`,
        `products.${productId}.inventory.available`,
      ])

      // Aggregation: product price → category price
      const categoryId = `category_${i % 5}`
      paths.push([
        `products.${productId}.pricing.basePrice`,
        `categories.${categoryId}.avgPrice`,
      ])

      // Stock aggregation: product stock → category stock
      paths.push([
        `products.${productId}.inventory.quantity`,
        `categories.${categoryId}.totalStock`,
      ])

      // Multi-level: product → category → company
      paths.push([
        `categories.${categoryId}.avgPrice`,
        `company.stats.avgProductPrice`,
      ])
      paths.push([
        `categories.${categoryId}.totalStock`,
        `company.stats.totalInventory`,
      ])
    }

    return paths
  }

  // Generate multi-level inventory aggregation paths
  const generateInventoryAggregationPaths = (): [string, string][] => {
    const paths: [string, string][] = []
    const categories = ['electronics', 'clothing', 'home', 'sports', 'books']
    const subcategories = [
      ['laptops', 'phones', 'tablets'],
      ['shirts', 'pants', 'shoes'],
      ['furniture', 'decor', 'kitchen'],
      ['equipment', 'apparel', 'accessories'],
      ['fiction', 'nonfiction', 'reference'],
    ]

    categories.forEach((cat, catIdx) => {
      subcategories[catIdx]?.forEach((subcat) => {
        // SKU-level inventory → subcategory stock
        for (let sku = 0; sku < 3; sku++) {
          paths.push([
            `inventory.${cat}.${subcat}.skus.sku_${sku}.quantity`,
            `inventory.${cat}.${subcat}.totalStock`,
          ])
        }

        // Subcategory stock → category total
        paths.push([
          `inventory.${cat}.${subcat}.totalStock`,
          `inventory.${cat}.totalStock`,
        ])

        // Category stock → warehouse totals
        paths.push([
          `inventory.${cat}.totalStock`,
          `inventory.warehouse.regional.${cat}.stock`,
        ])

        // Price aggregation: SKU price → subcategory avg
        for (let sku = 0; sku < 3; sku++) {
          paths.push([
            `pricing.${cat}.${subcat}.skus.sku_${sku}.price`,
            `pricing.${cat}.${subcat}.avgPrice`,
          ])
        }

        // Subcategory avg → category avg
        paths.push([
          `pricing.${cat}.${subcat}.avgPrice`,
          `pricing.${cat}.categoryAvg`,
        ])
      })
    })

    return paths
  }

  // ==========================================================================
  // normalizeChangesForGroups Benchmarks
  // ==========================================================================

  describe('normalizeChangesForGroups', () => {
    bench('single change, 10 registered paths', () => {
      const paths = generateEcommercePaths(10)
      const pathGroups = [
        paths.slice(0, 3).flatMap((p) => [p[0], p[1]]),
        paths.slice(3, 6).flatMap((p) => [p[0], p[1]]),
      ]

      const changes = typeHelpers.changes<BenchmarkState>([
        ['products.product_0.pricing.basePrice', 99.99],
      ])

      normalizeChangesForGroups({
        changes: changes,
        pathGroups,
      })
    })

    bench('single change, 100 registered paths with groups', () => {
      const paths = generateEcommercePaths(50)
      const pathGroups = [
        paths.slice(0, 15).flatMap((p) => [p[0], p[1]]),
        paths.slice(15, 30).flatMap((p) => [p[0], p[1]]),
        paths.slice(30, 50).flatMap((p) => [p[0], p[1]]),
      ]

      const changes = typeHelpers.changes<BenchmarkState>([
        ['products.product_5.pricing.basePrice', 99.99],
      ])

      normalizeChangesForGroups({
        changes: changes,
        pathGroups,
      })
    })

    bench('batch of 50 changes, 100 registered paths', () => {
      const paths = generateEcommercePaths(50)
      const pathGroups = [
        paths.slice(0, 15).flatMap((p) => [p[0], p[1]]),
        paths.slice(15, 30).flatMap((p) => [p[0], p[1]]),
        paths.slice(30, 50).flatMap((p) => [p[0], p[1]]),
      ]

      const changes = typeHelpers.changes<BenchmarkState>(
        Array.from({ length: 50 }, (_, i) => [
          `products.product_${i % 10}.pricing.basePrice`,
          Math.random() * 100,
        ]),
      )

      normalizeChangesForGroups({
        changes,
        pathGroups,
      })
    })

    bench('deep nested paths (inventory aggregation, 100 paths)', () => {
      const paths = generateInventoryAggregationPaths()
      const pathGroups = [
        paths.slice(0, 20).flatMap((p) => [p[0], p[1]]),
        paths.slice(20, 50).flatMap((p) => [p[0], p[1]]),
        paths
          .slice(50, Math.min(100, paths.length))
          .flatMap((p) => [p[0], p[1]]),
      ].filter((g) => g.length > 0)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['inventory.electronics.laptops.skus.sku_0.quantity', 50],
      ])

      normalizeChangesForGroups({
        changes,
        pathGroups,
      })
    })

    bench('parent change with nested extraction', () => {
      const paths = [
        ['a.b.c', 'a.b.d'],
        ['x.y.z', 'x.y.w'],
      ]
      const pathGroups = [paths[0] as string[], paths[1] as string[]]

      const changes = typeHelpers.changes<BenchmarkState>([
        [
          'a.b',
          {
            c: { deep: { nested: 'value' } },
            d: { other: 'data' },
          },
        ],
      ])

      normalizeChangesForGroups({
        changes,
        pathGroups,
      })
    })

    bench('child change propagation (deep updates)', () => {
      const paths = generateInventoryAggregationPaths().slice(0, 30)
      const pathGroups = [
        paths.slice(0, 10).flatMap((p) => [p[0], p[1]]),
        paths.slice(10, 20).flatMap((p) => [p[0], p[1]]),
      ]

      const changes = typeHelpers.changes<BenchmarkState>([
        ['inventory.electronics.laptops.skus.sku_0.quantity', 50],
        ['pricing.electronics.laptops.skus.sku_0.price', 999.99],
        ['inventory.clothing.shirts.totalStock', 250],
      ])

      normalizeChangesForGroups({
        changes,
        pathGroups,
      })
    })
  })

  // ==========================================================================
  // processChanges Full Pipeline Benchmarks
  // ==========================================================================

  describe('processChanges', () => {
    bench('simple state mutation, no relationships', () => {
      const store = createMockStore()
      const changes = typeHelpers.changes<BenchmarkState>([
        ['state.name', 'John'],
      ])

      processChanges(store, changes)
    })

    bench('state mutation with sync paths (10 relationships)', () => {
      const syncPaths = generateEcommercePaths(5)
      const store = createMockStore(syncPaths)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['products.product_0.pricing.basePrice', 99.99],
      ])

      processChanges(store, changes)
    })

    bench('batch mutation with sync paths (50 changes, 50 paths)', () => {
      const syncPaths = generateEcommercePaths(25)
      const store = createMockStore(syncPaths)

      const changes = typeHelpers.changes<BenchmarkState>(
        Array.from({ length: 50 }, (_, i) => [
          `products.product_${i % 10}.pricing.basePrice`,
          Math.random() * 100,
        ]),
      )

      processChanges(store, changes)
    })

    bench('deep nested inventory aggregation (100 relationships)', () => {
      const syncPaths = generateInventoryAggregationPaths()
      const store = createMockStore(syncPaths)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['inventory.electronics.laptops.skus.sku_0.quantity', 50],
      ])

      processChanges(store, changes)
    })

    bench('cascading updates (parent → child → grandchild)', () => {
      const syncPaths = [
        ['user.profile.settings.theme', 'app.ui.theme'],
        ['app.ui.theme', 'app.ui.components.button.theme'],
        ['app.ui.components.button.theme', 'app.ui.components.button.color'],
      ] as [string, string][]
      const store = createMockStore(syncPaths)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['user.profile.settings.theme', 'dark'],
      ])

      processChanges(store, changes)
    })

    bench('boolean flip paths (5 relationships)', () => {
      const flipPaths = [
        ['user.isAdmin', 'ui.isReadonly'],
        ['feature.enabled', 'feature.disabled'],
        ['payment.isProcessing', 'form.isDisabled'],
        ['modal.isOpen', 'backdrop.isVisible'],
        ['loading', 'content.isHidden'],
      ] as [string, string][]
      const store = createMockStore([], flipPaths)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['feature.enabled', true],
      ])

      processChanges(store, changes)
    })

    bench('mixed sync and flip operations', () => {
      const syncPaths = generateEcommercePaths(10)
      const flipPaths = [
        ['product.isAvailable', 'product.isDisabled'],
        ['store.isOpen', 'store.isClosed'],
      ] as [string, string][]
      const store = createMockStore(syncPaths, flipPaths)

      const changes = typeHelpers.changes<BenchmarkState>([
        ['products.product_0.pricing.basePrice', 99.99],
        ['products.product_1.inventory.quantity', 50],
        ['product.isAvailable', true],
        ['store.isOpen', false],
      ])

      processChanges(store, changes)
    })
  })
})
