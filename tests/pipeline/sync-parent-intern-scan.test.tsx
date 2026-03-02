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
