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

import Graph from 'graphology'
import { proxy } from 'valtio/vanilla'
import { bench, describe } from 'vitest'

import type { StoreInstance } from '../../src/core/types'
import { normalizeChangesForGroups } from '../../src/pipeline/normalizeChanges'
import { processChanges } from '../../src/pipeline/processChanges'

// =============================================================================
// Mock Store Factory
// =============================================================================

const createMockStore = (
  syncPaths: [string, string][] = [],
  flipPaths: [string, string][] = [],
): StoreInstance<Record<string, any>, any> => {
  const syncGraph = new Graph({ type: 'undirected' })
  const flipGraph = new Graph({ type: 'undirected' })

  for (const [path1, path2] of syncPaths) {
    if (!syncGraph.hasNode(path1)) syncGraph.addNode(path1)
    if (!syncGraph.hasNode(path2)) syncGraph.addNode(path2)
    if (!syncGraph.hasEdge(path1, path2)) {
      syncGraph.addEdge(path1, path2)
    }
  }

  for (const [path1, path2] of flipPaths) {
    if (!flipGraph.hasNode(path1)) flipGraph.addNode(path1)
    if (!flipGraph.hasNode(path2)) flipGraph.addNode(path2)
    if (!flipGraph.hasEdge(path1, path2)) {
      flipGraph.addEdge(path1, path2)
    }
  }

  return {
    state: proxy({ root: {} }),
    _concerns: proxy({}),
    _internal: {
      graphs: {
        sync: syncGraph,
        flip: flipGraph,
        aggregation: new Graph({ type: 'directed' }),
        listeners: new Graph({ type: 'directed' }),
      },
      processing: {
        queue: [],
      },
    },
  } as any
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
      const pathGroups = [paths.slice(0, 3), paths.slice(3, 6)]

      const changes = [['products.product_0.pricing.basePrice', 99.99, {}]]

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: pathGroups as any[],
      })
    })

    bench('single change, 100 registered paths with groups', () => {
      const paths = generateEcommercePaths(50)
      const pathGroups = [
        paths.slice(0, 15),
        paths.slice(15, 30),
        paths.slice(30, 50),
      ]

      const changes = [['products.product_5.pricing.basePrice', 99.99, {}]]

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: pathGroups as any[],
      })
    })

    bench('batch of 50 changes, 100 registered paths', () => {
      const paths = generateEcommercePaths(50)
      const pathGroups = [
        paths.slice(0, 15),
        paths.slice(15, 30),
        paths.slice(30, 50),
      ]

      const changes = Array.from({ length: 50 }, (_, i) => [
        `products.product_${i % 10}.pricing.basePrice`,
        Math.random() * 100,
        {},
      ])

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: pathGroups as any[],
      })
    })

    bench('deep nested paths (inventory aggregation, 100 paths)', () => {
      const paths = generateInventoryAggregationPaths()
      const pathGroups = [
        paths.slice(0, 20),
        paths.slice(20, 50),
        paths.slice(50, Math.min(100, paths.length)),
      ].filter((g) => g.length > 0)

      const changes = [
        [
          'inventory.electronics.laptops.skus.sku_0.quantity',
          50,
          { isUserChange: true },
        ],
      ]

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: pathGroups as any[],
      })
    })

    bench('parent change with nested extraction', () => {
      const paths = [
        ['a.b.c', 'a.b.d'],
        ['x.y.z', 'x.y.w'],
      ]
      const pathGroups = [paths[0], paths[1]]

      const changes = [
        [
          'a.b',
          {
            c: { deep: { nested: 'value' } },
            d: { other: 'data' },
          },
          {},
        ],
      ]

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: [pathGroups] as any[],
      })
    })

    bench('child change propagation (deep updates)', () => {
      const paths = generateInventoryAggregationPaths().slice(0, 30)
      const pathGroups = [paths.slice(0, 10), paths.slice(10, 20)]

      const changes = [
        [
          'inventory.electronics.laptops.skus.sku_0.quantity',
          50,
          { isUpdate: true },
        ],
        [
          'pricing.electronics.laptops.skus.sku_0.price',
          999.99,
          { isUpdate: true },
        ],
        [
          'inventory.clothing.shirts.totalStock',
          250,
          { isInventorySync: true },
        ],
      ]

      normalizeChangesForGroups({
        changes: changes as any[],
        pathGroups: pathGroups as any[],
      })
    })
  })

  // ==========================================================================
  // processChanges Full Pipeline Benchmarks
  // ==========================================================================

  describe('processChanges', () => {
    bench('simple state mutation, no relationships', () => {
      const store = createMockStore()
      const changes = [['state.name', 'John', {}]]

      processChanges(store, changes as any[])
    })

    bench('state mutation with sync paths (10 relationships)', () => {
      const syncPaths = generateEcommercePaths(5)
      const store = createMockStore(syncPaths)

      const changes = [['products.product_0.pricing.basePrice', 99.99, {}]]

      processChanges(store, changes as any[])
    })

    bench('batch mutation with sync paths (50 changes, 50 paths)', () => {
      const syncPaths = generateEcommercePaths(25)
      const store = createMockStore(syncPaths)

      const changes = Array.from({ length: 50 }, (_, i) => [
        `products.product_${i % 10}.pricing.basePrice`,
        Math.random() * 100,
        {},
      ])

      processChanges(store, changes as any[])
    })

    bench('deep nested inventory aggregation (100 relationships)', () => {
      const syncPaths = generateInventoryAggregationPaths()
      const store = createMockStore(syncPaths)

      const changes = [
        [
          'inventory.electronics.laptops.skus.sku_0.quantity',
          50,
          { isUserChange: true },
        ],
      ]

      processChanges(store, changes as any[])
    })

    bench('cascading updates (parent → child → grandchild)', () => {
      const syncPaths = [
        ['user.profile.settings.theme', 'app.ui.theme'],
        ['app.ui.theme', 'app.ui.components.button.theme'],
        ['app.ui.components.button.theme', 'app.ui.components.button.color'],
      ] as [string, string][]
      const store = createMockStore(syncPaths)

      const changes = [['user.profile.settings.theme', 'dark', {}]]

      processChanges(store, changes as any[])
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

      const changes = [['feature.enabled', true, {}]]

      processChanges(store, changes as any[])
    })

    bench('mixed sync and flip operations', () => {
      const syncPaths = generateEcommercePaths(10)
      const flipPaths = [
        ['product.isAvailable', 'product.isDisabled'],
        ['store.isOpen', 'store.isClosed'],
      ] as [string, string][]
      const store = createMockStore(syncPaths, flipPaths)

      const changes = [
        ['products.product_0.pricing.basePrice', 99.99, {}],
        ['products.product_1.inventory.quantity', 50, {}],
        ['product.isAvailable', true, {}],
        ['store.isOpen', false, {}],
      ]

      processChanges(store, changes as any[])
    })
  })
})
