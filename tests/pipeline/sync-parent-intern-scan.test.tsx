/**
 * Sync parent expansion — intern table scan (Case 3)
 *
 * Regression: When a change targets a PARENT of a registered sync path,
 * Case 3 previously used `shadow.affected_paths()` to discover which leaves
 * to sync. That only finds leaves that ALREADY EXIST in shadow state.
 *
 * If the leaf was registered in the sync graph but not yet written to shadow
 * (e.g. first-time write of an object containing the leaf), the sync would
 * silently drop — the peer never received the value.
 *
 * Fix: Case 3 now scans the INTERN TABLE for children of the change path that
 * are registered in the sync graph, regardless of shadow existence.
 *
 * Concrete ecommerce scenario:
 *   syncPaths: [['catalog.us.products.sku001.pricing.salePrice',
 *                'cart.lineItems.sku001.pricing.salePrice']]
 *   change:     catalog.us.products.sku001.pricing = { salePrice: 19.99 }
 *               (leaf not yet in shadow — pricing object just created)
 *   Expected:   cart.lineItems.sku001.pricing.salePrice = 19.99  ← sync must fire
 */

import { describe, expect, it } from 'vitest'

import { registerSideEffects } from '~/sideEffects/registration.wasm-impl'
import type { ArrayOfChanges, GenericMeta } from '~/types'

import { createTestStore, expectShadowMatch } from '../utils/react'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Narrow helper so tests don't fight TypeScript unknown-key access. */
const at = (obj: unknown, ...keys: string[]): unknown => {
  let cur = obj as Record<string, unknown>
  for (const k of keys) {
    if (cur == null || typeof cur !== 'object') return undefined
    cur = (cur as Record<string, unknown>)[k] as Record<string, unknown>
  }
  return cur
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('[WASM] Sync parent expansion — intern table scan', () => {
  it('syncs deeply nested pricing leaf that was never written to shadow', () => {
    // Catalog pricing synced to cart line items.
    // The 'salePrice' leaf does not exist in shadow; pricing containers are empty objects.
    interface State {
      catalog: {
        us: { products: { sku001: { pricing: { salePrice?: number } } } }
      }
      cart: {
        lineItems: { sku001: { pricing: { salePrice?: number } } }
      }
    }

    const initialState: State = {
      catalog: { us: { products: { sku001: { pricing: {} } } } },
      cart: { lineItems: { sku001: { pricing: {} } } },
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      initialState,
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        [
          'catalog.us.products.sku001.pricing.salePrice',
          'cart.lineItems.sku001.pricing.salePrice',
        ],
      ],
    })

    // Change at pricing parent level — 'salePrice' leaf is absent from shadow
    processChanges(storeInstance, [
      [
        'catalog.us.products.sku001.pricing',
        { salePrice: 19.99 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    // Cart line item must receive the value from the incoming JSON fallback
    const cartPrice = at(
      storeInstance.state,
      'cart',
      'lineItems',
      'sku001',
      'pricing',
      'salePrice',
    )
    expect(cartPrice).toBe(19.99)

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('syncs multiple product attributes none of which existed in shadow', () => {
    // Warehouse inventory synced to storefront — all leaves are first-time writes.
    interface State {
      warehouse: {
        sku001: { stock?: number; reserved?: number; inTransit?: number }
      }
      storefront: {
        sku001: { stock?: number; reserved?: number; inTransit?: number }
      }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      { warehouse: { sku001: {} }, storefront: { sku001: {} } },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        ['warehouse.sku001.stock', 'storefront.sku001.stock'],
        ['warehouse.sku001.reserved', 'storefront.sku001.reserved'],
        ['warehouse.sku001.inTransit', 'storefront.sku001.inTransit'],
      ],
    })

    processChanges(storeInstance, [
      [
        'warehouse.sku001',
        { stock: 100, reserved: 20, inTransit: 5 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(at(storeInstance.state, 'storefront', 'sku001', 'stock')).toBe(100)
    expect(at(storeInstance.state, 'storefront', 'sku001', 'reserved')).toBe(20)
    expect(at(storeInstance.state, 'storefront', 'sku001', 'inTransit')).toBe(5)

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('syncs product attributes that existed in shadow AND those that did not', () => {
    // product.basePrice already in shadow (sync via shadow lookup).
    // product.discountRate is new (sync via incoming JSON).
    interface State {
      product: { basePrice: number; discountRate?: number }
      cart: { basePrice: number; discountRate?: number }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      { product: { basePrice: 49.99 }, cart: { basePrice: 49.99 } },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        ['product.basePrice', 'cart.basePrice'],
        ['product.discountRate', 'cart.discountRate'],
      ],
    })

    processChanges(storeInstance, [
      ['product', { basePrice: 39.99, discountRate: 0.2 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(at(storeInstance.state, 'cart', 'basePrice')).toBe(39.99)
    expect(at(storeInstance.state, 'cart', 'discountRate')).toBe(0.2)

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('does NOT sync to cart when the attribute is absent from the incoming product update', () => {
    // product.featured is registered in sync graph but not present in the new product object.
    // cart.featured must NOT appear.
    interface State {
      product: { name?: string; featured?: boolean }
      cart: { name?: string; featured?: boolean }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      { product: {}, cart: {} },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        ['product.name', 'cart.name'],
        ['product.featured', 'cart.featured'],
      ],
    })

    // Only 'name' in the incoming value — 'featured' is absent
    processChanges(storeInstance, [
      ['product', { name: 'Wireless Headphones' }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(at(storeInstance.state, 'cart', 'name')).toBe('Wireless Headphones')
    // 'featured' must NOT appear — absent from both shadow and incoming value
    expect(at(storeInstance.state, 'cart', 'featured')).toBeUndefined()

    cleanup()
  })

  it('does NOT produce spurious syncs for paths interned via prior changes but not sync-registered', () => {
    // Prior processChanges calls intern paths into the intern table.
    // An interned-but-unregistered path must NOT produce sync output.
    interface State {
      product: { price: number; internalCost: number }
      cart: { price: number }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      { product: { price: 100, internalCost: 60 }, cart: { price: 100 } },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [['product.price', 'cart.price']], // internalCost is NOT registered
    })

    // Intern product.internalCost via a direct change (it's now in the intern table)
    processChanges(storeInstance, [
      ['product.internalCost', 65, {} as GenericMeta],
    ] as ArrayOfChanges<State, GenericMeta>)

    // Set the parent — product.internalCost is interned but NOT sync-registered
    processChanges(storeInstance, [
      ['product', { price: 90, internalCost: 65 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    // Registered pair still syncs
    expect(at(storeInstance.state, 'cart', 'price')).toBe(90)
    // Unregistered attribute does NOT leak into cart
    expect(at(storeInstance.state, 'cart', 'internalCost')).toBeUndefined()

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('syncs correctly on a second parent-level write after leaf is in shadow', () => {
    // After the first write the leaf is in shadow. A second write should still
    // sync correctly (the shadow lookup path is taken, not the incoming JSON path).
    interface State {
      product: { price?: number }
      cart: { price?: number }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      { product: {}, cart: {} },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [['product.price', 'cart.price']],
    })

    // First write — leaf absent from shadow
    processChanges(storeInstance, [
      ['product', { price: 49.99 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(at(storeInstance.state, 'cart', 'price')).toBe(49.99)

    // Second write — leaf now in shadow, price changes again
    processChanges(storeInstance, [
      ['product', { price: 29.99 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(at(storeInstance.state, 'cart', 'price')).toBe(29.99)

    expectShadowMatch(storeInstance)
    cleanup()
  })
})

// ---------------------------------------------------------------------------
// Failing scenarios — parent set to {} or object lacking the registered key
// These demonstrate the remaining bug: sync does not fire when source leaf
// is absent from both shadow (cleared by parent write) and incoming value.
// ---------------------------------------------------------------------------

describe('[WASM] Sync parent expansion — empty/partial parent write (regression)', () => {
  it('clears deeply nested peer when source pricing object is reset to {}', () => {
    // order.items.sku001.pricing.salePrice was set previously (exists in shadow).
    // A parent reset to {} clears salePrice from shadow AND the incoming value has no key.
    // Expected: cart peer must receive null to stay in sync.
    interface State {
      order: { items: { sku001: { pricing: { salePrice?: number } } } }
      cart: { lines: { sku001: { pricing: { salePrice?: number } } } }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      {
        order: { items: { sku001: { pricing: { salePrice: 19.99 } } } },
        cart: { lines: { sku001: { pricing: { salePrice: 19.99 } } } },
      },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        [
          'order.items.sku001.pricing.salePrice',
          'cart.lines.sku001.pricing.salePrice',
        ],
      ],
    })

    // Reset pricing to {} — salePrice is gone from both shadow and incoming
    processChanges(storeInstance, [
      ['order.items.sku001.pricing', {}, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    // Cart peer must be cleared — null, not left stale at 19.99
    const cartPrice = at(
      storeInstance.state,
      'cart',
      'lines',
      'sku001',
      'pricing',
      'salePrice',
    )
    expect(cartPrice).toBeNull()

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('clears deeply nested peer when source object is replaced with sibling-only keys', () => {
    // catalog.us pricing has basePrice; it is replaced with an object containing only tax.
    // Expected: catalog.eu basePrice must receive null (not stay at 100.0).
    interface State {
      catalog: {
        us: {
          products: {
            sku001: { pricing: { basePrice?: number; tax?: number } }
          }
        }
        eu: {
          products: {
            sku001: { pricing: { basePrice?: number; tax?: number } }
          }
        }
      }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      {
        catalog: {
          us: { products: { sku001: { pricing: { basePrice: 100 } } } },
          eu: { products: { sku001: { pricing: { basePrice: 100 } } } },
        },
      },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        [
          'catalog.us.products.sku001.pricing.basePrice',
          'catalog.eu.products.sku001.pricing.basePrice',
        ],
      ],
    })

    // Replace pricing — basePrice absent, only tax present
    processChanges(storeInstance, [
      ['catalog.us.products.sku001.pricing', { tax: 0.2 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    const euPrice = at(
      storeInstance.state,
      'catalog',
      'eu',
      'products',
      'sku001',
      'pricing',
      'basePrice',
    )
    // Peer must be cleared — source no longer has basePrice
    expect(euPrice).toBeNull()

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('syncs null to peer when deeply nested source leaf is first written then cleared via parent', () => {
    // Two-step: first set the leaf (it goes into shadow), then clear parent to {}.
    // After clear, peer must reflect null.
    interface State {
      warehouse: { sku001: { availability: { inStock?: boolean } } }
      storefront: { sku001: { availability: { inStock?: boolean } } }
    }

    const { storeInstance, processChanges } = createTestStore<State>(
      {},
      {
        warehouse: { sku001: { availability: {} } },
        storefront: { sku001: { availability: {} } },
      },
    )

    const cleanup = registerSideEffects(storeInstance, 'test', {
      syncPaths: [
        [
          'warehouse.sku001.availability.inStock',
          'storefront.sku001.availability.inStock',
        ],
      ],
    })

    // Step 1: set inStock — leaf enters shadow, peer syncs
    processChanges(storeInstance, [
      ['warehouse.sku001.availability', { inStock: true }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    expect(
      at(
        storeInstance.state,
        'storefront',
        'sku001',
        'availability',
        'inStock',
      ),
    ).toBe(true)

    // Step 2: clear availability to {} — inStock gone from shadow AND incoming
    processChanges(storeInstance, [
      ['warehouse.sku001.availability', {}, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<State, GenericMeta>)

    // Peer must be cleared — null, not stale true
    const storefrontStock = at(
      storeInstance.state,
      'storefront',
      'sku001',
      'availability',
      'inStock',
    )
    expect(storefrontStock).toBeNull()

    expectShadowMatch(storeInstance)
    cleanup()
  })
})

// ---------------------------------------------------------------------------
// 7-way interconnected sync — all regions in one sync component
// ---------------------------------------------------------------------------

describe('[WASM] Sync parent expansion — 7 interconnected regional catalogs', () => {
  /** The 7 regions used across all tests in this suite. */
  const REGIONS = ['us', 'eu', 'uk', 'au', 'jp', 'ca', 'br'] as const
  type Region = (typeof REGIONS)[number]

  /** Build the initial state: all regions, pricing containers optional leaf. */
  const buildState = (
    salePriceByRegion: Partial<Record<Region, number | null>>,
  ) => {
    const catalog = Object.fromEntries(
      REGIONS.map((r) => {
        const pricing: Record<string, unknown> =
          salePriceByRegion[r] !== undefined
            ? { salePrice: salePriceByRegion[r] }
            : {}
        return [r, { products: { sku001: { pricing } } }]
      }),
    )
    return { catalog } as {
      catalog: Record<
        Region,
        { products: { sku001: { pricing: { salePrice?: number | null } } } }
      >
    }
  }

  /** Register star-topology sync pairs: us ↔ every other region. */
  const registerRegionalSync = (
    storeInstance: Parameters<typeof registerSideEffects>[0],
  ) =>
    registerSideEffects(storeInstance, 'test', {
      syncPaths: REGIONS.filter((r) => r !== 'us').map((r) => [
        `catalog.us.products.sku001.pricing.salePrice`,
        `catalog.${r}.products.sku001.pricing.salePrice`,
      ]) as unknown as any,
    })

  const peerPrice = (
    state: ReturnType<typeof buildState>,
    region: Region,
  ): unknown =>
    at(state, 'catalog', region, 'products', 'sku001', 'pricing', 'salePrice')

  it('propagates first-time salePrice write via US parent change to all 6 peers', () => {
    // None of the 7 regions have salePrice in shadow yet.
    // US pricing parent is set with salePrice → all 6 peers must receive the value.
    const { storeInstance, processChanges } = createTestStore(
      {},
      buildState({}),
    )
    const cleanup = registerRegionalSync(storeInstance)

    processChanges(storeInstance, [
      [
        'catalog.us.products.sku001.pricing',
        { salePrice: 29.99 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<ReturnType<typeof buildState>, GenericMeta>)

    for (const region of REGIONS.filter((r) => r !== 'us')) {
      expect(peerPrice(storeInstance.state, region)).toBe(29.99)
    }

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('clears all 6 peers to null when US pricing is reset to {}', () => {
    // All regions start with salePrice = 49.99 in shadow.
    // US pricing cleared to {} → all 6 peers must receive null.
    const initial = buildState(
      Object.fromEntries(REGIONS.map((r) => [r, 49.99])) as Record<
        Region,
        number
      >,
    )
    const { storeInstance, processChanges } = createTestStore({}, initial)
    const cleanup = registerRegionalSync(storeInstance)

    processChanges(storeInstance, [
      ['catalog.us.products.sku001.pricing', {}, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<ReturnType<typeof buildState>, GenericMeta>)

    for (const region of REGIONS.filter((r) => r !== 'us')) {
      expect(peerPrice(storeInstance.state, region)).toBeNull()
    }

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('syncs all peers when some had salePrice in shadow and some did not (mixed state)', () => {
    // eu/uk/au already have salePrice=39.99 in shadow; jp/ca/br do not.
    // US parent update → all 6 peers must receive 19.99 regardless of prior state.
    const initial = buildState({
      eu: 39.99,
      uk: 39.99,
      au: 39.99,
    })
    const { storeInstance, processChanges } = createTestStore({}, initial)
    const cleanup = registerRegionalSync(storeInstance)

    processChanges(storeInstance, [
      [
        'catalog.us.products.sku001.pricing',
        { salePrice: 19.99 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<ReturnType<typeof buildState>, GenericMeta>)

    for (const region of REGIONS.filter((r) => r !== 'us')) {
      expect(peerPrice(storeInstance.state, region)).toBe(19.99)
    }

    expectShadowMatch(storeInstance)
    cleanup()
  })

  it('handles two sequential parent writes correctly across all 6 peers', () => {
    // Write 1: salePrice first appears (leaf absent from shadow)
    // Write 2: salePrice changes again (leaf now in shadow)
    // Both writes must propagate to all 6 peers.
    const { storeInstance, processChanges } = createTestStore(
      {},
      buildState({}),
    )
    const cleanup = registerRegionalSync(storeInstance)

    // Write 1 — first appearance
    processChanges(storeInstance, [
      [
        'catalog.us.products.sku001.pricing',
        { salePrice: 49.99 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<ReturnType<typeof buildState>, GenericMeta>)

    for (const region of REGIONS.filter((r) => r !== 'us')) {
      expect(peerPrice(storeInstance.state, region)).toBe(49.99)
    }

    // Write 2 — update (shadow lookup path taken)
    processChanges(storeInstance, [
      [
        'catalog.us.products.sku001.pricing',
        { salePrice: 34.99 },
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<ReturnType<typeof buildState>, GenericMeta>)

    for (const region of REGIONS.filter((r) => r !== 'us')) {
      expect(peerPrice(storeInstance.state, region)).toBe(34.99)
    }

    expectShadowMatch(storeInstance)
    cleanup()
  })
})

// ---------------------------------------------------------------------------
// Incremental registration — initial sync voting with undefined 6th product
// ---------------------------------------------------------------------------

describe('[WASM] Incremental registration — initial sync voting', () => {
  /**
   * Build a catalog state with N products, each having a salePrice.
   * A 6th product can be added with no salePrice (undefined / absent).
   */
  const PRODUCT_IDS = ['p1', 'p2', 'p3', 'p4', 'p5'] as const

  interface CatalogState {
    catalog: Record<string, { pricing: { salePrice?: number } }>
    shared: { pricing: { salePrice?: number } }
  }

  /** Register syncPaths for a given product id: product.pricing.salePrice ↔ shared.pricing.salePrice */
  const registerProduct = (
    storeInstance: Parameters<typeof registerSideEffects>[0],
    productId: string,
  ) =>
    registerSideEffects(storeInstance, productId, {
      syncPaths: [
        [`catalog.${productId}.pricing.salePrice`, 'shared.pricing.salePrice'],
      ] as unknown as any,
    })

  it('converges all 5 products to the shared established value when registered one by one', () => {
    // shared.pricing.salePrice is the authoritative price (set before any product mounts).
    // Products start with 5 different values. They register one by one.
    //
    // Why shared wins in a tie: its path is shallower (depth 2) than any product path
    // (depth 3). Tie-breaking by shallowest path means shared always wins 1:1 ties.
    // Once it wins, subsequent registrations see 2+ votes for shared's value vs 1 for
    // the newcomer → shared's value propagates to all products.
    const state: CatalogState = {
      catalog: {
        p1: { pricing: { salePrice: 10 } },
        p2: { pricing: { salePrice: 20 } },
        p3: { pricing: { salePrice: 40 } },
        p4: { pricing: { salePrice: 50 } },
        p5: { pricing: { salePrice: 60 } },
      },
      shared: { pricing: { salePrice: 30 } }, // established price — wins via depth tie-break
    }

    const { storeInstance } = createTestStore<CatalogState>({}, state)

    const cleanups = PRODUCT_IDS.map((id) => registerProduct(storeInstance, id))

    // After all registrations: every path in the component converges to 30
    const price = (id: string) =>
      at(storeInstance.state, 'catalog', id, 'pricing', 'salePrice')

    for (const id of PRODUCT_IDS) {
      expect(price(id)).toBe(30)
    }
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(30)

    expectShadowMatch(storeInstance)
    cleanups.forEach((c) => c())
  })

  it('syncs undefined 6th product to the agreed value of the existing 5', () => {
    // 5 products are already registered and all agree on salePrice = 99.
    // A 6th product is added with NO salePrice (path absent from shadow — simulates
    // a brand-new product that has never had a price set).
    // After the 6th registers its sync pair, it must receive 99 from the component.
    const state: CatalogState = {
      catalog: {
        p1: { pricing: { salePrice: 99 } },
        p2: { pricing: { salePrice: 99 } },
        p3: { pricing: { salePrice: 99 } },
        p4: { pricing: { salePrice: 99 } },
        p5: { pricing: { salePrice: 99 } },
        p6: { pricing: {} }, // no salePrice — absent from shadow
      },
      shared: { pricing: { salePrice: 99 } },
    }

    const { storeInstance } = createTestStore<CatalogState>({}, state)

    // Register 5 first
    const cleanups = PRODUCT_IDS.map((id) => registerProduct(storeInstance, id))

    // All 5 already agree on 99 — no change needed from voting
    for (const id of PRODUCT_IDS) {
      expect(
        at(storeInstance.state, 'catalog', id, 'pricing', 'salePrice'),
      ).toBe(99)
    }

    // Register 6th — its salePrice is absent from shadow → doesn't vote
    // → winner is 99 (5 votes) → 6th receives 99
    const cleanup6 = registerProduct(storeInstance, 'p6')

    expect(
      at(storeInstance.state, 'catalog', 'p6', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(99)

    expectShadowMatch(storeInstance)
    ;[...cleanups, cleanup6].forEach((c) => c())
  })

  it('6th product with explicit undefined does not corrupt the winning value', () => {
    // Same as above, but the 6th product's salePrice is explicitly set to undefined
    // (stored as the UNDEFINED_SENTINEL in shadow). Undefined is excluded from voting
    // just like null — so the winner is still determined by the other 5.
    const state: CatalogState = {
      catalog: {
        p1: { pricing: { salePrice: 55 } },
        p2: { pricing: { salePrice: 55 } },
        p3: { pricing: { salePrice: 55 } },
        p4: { pricing: { salePrice: 55 } },
        p5: { pricing: { salePrice: 55 } },
        p6: { pricing: {} }, // salePrice absent — simulates explicit undefined via processChanges below
      },
      shared: { pricing: { salePrice: 55 } },
    }

    const { storeInstance, processChanges } = createTestStore<CatalogState>(
      {},
      state,
    )

    // Register 5 established products
    const cleanups = PRODUCT_IDS.map((id) => registerProduct(storeInstance, id))

    // Explicitly write undefined to p6.salePrice (so it's in shadow as sentinel)
    processChanges(storeInstance, [
      [
        'catalog.p6.pricing.salePrice',
        undefined as unknown as number,
        {} as GenericMeta,
      ],
    ] as unknown as ArrayOfChanges<CatalogState, GenericMeta>)

    // Register 6th — undefined in shadow → excluded from voting → gets 55
    const cleanup6 = registerProduct(storeInstance, 'p6')

    expect(
      at(storeInstance.state, 'catalog', 'p6', 'pricing', 'salePrice'),
    ).toBe(55)

    expectShadowMatch(storeInstance)
    ;[...cleanups, cleanup6].forEach((c) => c())
  })

  it('changing 6th product after registration syncs correctly to all 5 peers', () => {
    // After the 6th registers and gets the shared value, further changes to the 6th
    // must propagate to all other products and the shared path.
    const state: CatalogState = {
      catalog: {
        p1: { pricing: { salePrice: 10 } },
        p2: { pricing: { salePrice: 10 } },
        p3: { pricing: { salePrice: 10 } },
        p4: { pricing: { salePrice: 10 } },
        p5: { pricing: { salePrice: 10 } },
        p6: { pricing: {} },
      },
      shared: { pricing: { salePrice: 10 } },
    }

    const { storeInstance, processChanges } = createTestStore<CatalogState>(
      {},
      state,
    )

    const cleanups = PRODUCT_IDS.map((id) => registerProduct(storeInstance, id))
    const cleanup6 = registerProduct(storeInstance, 'p6')

    // All 6 should now have 10
    for (const id of [...PRODUCT_IDS, 'p6']) {
      expect(
        at(storeInstance.state, 'catalog', id, 'pricing', 'salePrice'),
      ).toBe(10)
    }

    // Change p6.salePrice → all others must follow
    processChanges(storeInstance, [
      ['catalog.p6.pricing.salePrice', 77, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<CatalogState, GenericMeta>)

    for (const id of [...PRODUCT_IDS, 'p6']) {
      expect(
        at(storeInstance.state, 'catalog', id, 'pricing', 'salePrice'),
      ).toBe(77)
    }
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(77)

    expectShadowMatch(storeInstance)
    ;[...cleanups, cleanup6].forEach((c) => c())
  })
})

// ---------------------------------------------------------------------------
// All-undefined initial state — sync propagation after first real value set
// ---------------------------------------------------------------------------

describe('[WASM] All-null initial state — first value change must reach all products', () => {
  /**
   * Flat product structure: each product has its own salePrice synced to shared.salePrice.
   * All start null/absent. After one product is changed, all must follow.
   */
  interface FlatState {
    products: {
      p1: { salePrice: number | null }
      p2: { salePrice: number | null }
      p3: { salePrice: number | null }
      p4: { salePrice: number | null }
      p5: { salePrice: number | null }
    }
    shared: { salePrice: number | null }
  }

  const registerFlat = (
    storeInstance: Parameters<typeof registerSideEffects>[0],
    productId: string,
  ) =>
    registerSideEffects(storeInstance, productId, {
      syncPaths: [
        [`products.${productId}.salePrice`, 'shared.salePrice'],
      ] as unknown as any,
    })

  it('syncs first change to ALL products when all started null (flat paths)', () => {
    // All 5 products start with salePrice = null. Sequential registration.
    // No voting winner on registration (all null → skipped).
    // Changing p3.salePrice = 100 must reach p1 (first-registered) AND all others.
    const state: FlatState = {
      products: {
        p1: { salePrice: null },
        p2: { salePrice: null },
        p3: { salePrice: null },
        p4: { salePrice: null },
        p5: { salePrice: null },
      },
      shared: { salePrice: null },
    }

    const { storeInstance, processChanges } = createTestStore<FlatState>(
      {},
      state,
    )

    const cleanups = ['p1', 'p2', 'p3', 'p4', 'p5'].map((id) =>
      registerFlat(storeInstance, id),
    )

    // Change p3 — must propagate to all products INCLUDING p1 (first registered)
    processChanges(storeInstance, [
      ['products.p3.salePrice', 100, {} as GenericMeta],
    ] as ArrayOfChanges<FlatState, GenericMeta>)

    expect(at(storeInstance.state, 'products', 'p1', 'salePrice')).toBe(100)
    expect(at(storeInstance.state, 'products', 'p2', 'salePrice')).toBe(100)
    expect(at(storeInstance.state, 'products', 'p3', 'salePrice')).toBe(100)
    expect(at(storeInstance.state, 'products', 'p4', 'salePrice')).toBe(100)
    expect(at(storeInstance.state, 'products', 'p5', 'salePrice')).toBe(100)
    expect(at(storeInstance.state, 'shared', 'salePrice')).toBe(100)

    expectShadowMatch(storeInstance)
    cleanups.forEach((c) => c())
  })

  it('syncs first change to ALL products when all started absent (no key in shadow)', () => {
    // Pricing containers exist but the salePrice key is absent entirely.
    // This is stricter than null: shadow has no entry at all for these paths.
    interface NestedState {
      catalog: {
        p1: { pricing: { salePrice?: number } }
        p2: { pricing: { salePrice?: number } }
        p3: { pricing: { salePrice?: number } }
        p4: { pricing: { salePrice?: number } }
        p5: { pricing: { salePrice?: number } }
      }
      shared: { pricing: { salePrice?: number } }
    }

    const registerNested = (
      storeInstance: Parameters<typeof registerSideEffects>[0],
      productId: string,
    ) =>
      registerSideEffects(storeInstance, productId, {
        syncPaths: [
          [
            `catalog.${productId}.pricing.salePrice`,
            'shared.pricing.salePrice',
          ],
        ] as unknown as any,
      })

    const nestedState: NestedState = {
      catalog: {
        p1: { pricing: {} },
        p2: { pricing: {} },
        p3: { pricing: {} },
        p4: { pricing: {} },
        p5: { pricing: {} },
      },
      shared: { pricing: {} },
    }

    const { storeInstance, processChanges } = createTestStore<NestedState>(
      {},
      nestedState,
    )

    const cleanups = ['p1', 'p2', 'p3', 'p4', 'p5'].map((id) =>
      registerNested(storeInstance, id),
    )

    // Change p3 via exact path — must reach p1 (first registered, never had salePrice)
    processChanges(storeInstance, [
      ['catalog.p3.pricing.salePrice', 99, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<NestedState, GenericMeta>)

    expect(
      at(storeInstance.state, 'catalog', 'p1', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p2', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p3', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p4', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p5', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(99)

    expectShadowMatch(storeInstance)
    cleanups.forEach((c) => c())
  })
})

// ---------------------------------------------------------------------------
// Parent write with missing intermediate containers
// parent_exists guard vs paths where container was never initialized
// ---------------------------------------------------------------------------

describe('[WASM] Parent write — missing intermediate container blocks sync (regression)', () => {
  it('syncs via parent write when peers have NO pricing container in shadow', () => {
    // Products start with NO pricing key at all (not even {}).
    // Sync pairs are registered: catalog.pX.pricing.salePrice ↔ shared.pricing.salePrice
    // A PARENT write to catalog.p3.pricing = {salePrice: 100} triggers Case 3.
    // BUG: parent_exists("catalog.p1.pricing.salePrice") = false because catalog.p1.pricing
    //      was never in shadow → sync to p1 is blocked by the parent_exists guard.
    interface NestedState {
      catalog: {
        p1: Record<string, unknown>
        p2: Record<string, unknown>
        p3: Record<string, unknown>
        p4: Record<string, unknown>
        p5: Record<string, unknown>
      }
      shared: Record<string, unknown>
    }

    const state: NestedState = {
      catalog: {
        p1: {}, // NO pricing key
        p2: {},
        p3: {},
        p4: {},
        p5: {},
      },
      shared: {},
    }

    const { storeInstance, processChanges } = createTestStore<NestedState>(
      {},
      state,
    )

    // Register all 5 — containers don't exist yet, no voting winner
    const cleanups = ['p1', 'p2', 'p3', 'p4', 'p5'].map((id) =>
      registerSideEffects(storeInstance, id, {
        syncPaths: [
          [`catalog.${id}.pricing.salePrice`, 'shared.pricing.salePrice'],
        ] as unknown as any,
      }),
    )

    // PARENT write to p3 — pricing container created for the first time
    processChanges(storeInstance, [
      ['catalog.p3.pricing', { salePrice: 100 }, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<NestedState, GenericMeta>)

    // All peers must receive 100 — even those whose pricing container was never in shadow
    expect(
      at(storeInstance.state, 'catalog', 'p1', 'pricing', 'salePrice'),
    ).toBe(100)
    expect(
      at(storeInstance.state, 'catalog', 'p2', 'pricing', 'salePrice'),
    ).toBe(100)
    expect(
      at(storeInstance.state, 'catalog', 'p3', 'pricing', 'salePrice'),
    ).toBe(100)
    expect(
      at(storeInstance.state, 'catalog', 'p4', 'pricing', 'salePrice'),
    ).toBe(100)
    expect(
      at(storeInstance.state, 'catalog', 'p5', 'pricing', 'salePrice'),
    ).toBe(100)
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(100)

    cleanups.forEach((c) => c())
  })

  it('syncs exact-path change when p1 has NO pricing container but others do', () => {
    // p1 starts with no pricing container. Others have pricing: {}.
    // Direct change to catalog.p3.pricing.salePrice = 99.
    // BUG: parent_exists("catalog.p1.pricing.salePrice") fails because catalog.p1.pricing
    //      is absent from shadow → p1 never receives the sync.
    interface NestedState {
      catalog: {
        p1: { pricing?: { salePrice?: number } }
        p2: { pricing: { salePrice?: number } }
        p3: { pricing: { salePrice?: number } }
        p4: { pricing: { salePrice?: number } }
        p5: { pricing: { salePrice?: number } }
      }
      shared: { pricing: { salePrice?: number } }
    }

    const state: NestedState = {
      catalog: {
        p1: {}, // missing pricing container
        p2: { pricing: {} },
        p3: { pricing: {} },
        p4: { pricing: {} },
        p5: { pricing: {} },
      },
      shared: { pricing: {} },
    }

    const { storeInstance, processChanges } = createTestStore<NestedState>(
      {},
      state,
    )

    const cleanups = ['p1', 'p2', 'p3', 'p4', 'p5'].map((id) =>
      registerSideEffects(storeInstance, id, {
        syncPaths: [
          [`catalog.${id}.pricing.salePrice`, 'shared.pricing.salePrice'],
        ] as unknown as any,
      }),
    )

    // Direct exact-path change to p3
    processChanges(storeInstance, [
      ['catalog.p3.pricing.salePrice', 99, {} as GenericMeta],
    ] as unknown as ArrayOfChanges<NestedState, GenericMeta>)

    // p1 is the problem child — no pricing container → parent_exists guard fires
    expect(
      at(storeInstance.state, 'catalog', 'p1', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p2', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p3', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p4', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(
      at(storeInstance.state, 'catalog', 'p5', 'pricing', 'salePrice'),
    ).toBe(99)
    expect(at(storeInstance.state, 'shared', 'pricing', 'salePrice')).toBe(99)

    cleanups.forEach((c) => c())
  })
})
