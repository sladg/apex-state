/**
 * Real-World Benchmark: 15 Orders Currency Change
 *
 * Scenario:
 * - 15 orders, each with currency, confirmed (bool), totals, line items
 * - Changing currency on order_0 syncs to all 14 others (sync paths)
 * - Each order.confirmed flips to invoice.pending (flip paths)
 * - 15 listeners, each watching one order and producing ~15 changes
 * - Target: ~250 total changes applied
 *
 * Compares: JS-only pipeline vs WASM-accelerated pipeline
 */
import { proxy } from 'valtio/vanilla'
import { beforeAll, bench, describe } from 'vitest'

import { addEdge, createPathGroups } from '../../src/core/pathGroups'
import type {
  ListenerHandlerRef,
  ListenerRegistration,
  StoreInstance,
} from '../../src/core/types'
import { processChanges } from '../../src/pipeline/processChanges'
import type { ArrayOfChanges, GenericMeta } from '../../src/types'
import { getPathDepth } from '../../src/utils/pathUtils'
import { createTiming } from '../../src/utils/timing'
import {
  initWasm,
  registerListenersBatch,
  resetWasm,
  shadowInit,
} from '../../src/wasm/bridge'

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const ORDER_COUNT = 15

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type OrderState = Record<string, unknown>

// ---------------------------------------------------------------------------
// Listener handler — produces 15 changes per order
// ---------------------------------------------------------------------------

const makeListenerHandler =
  (orderIdx: number) =>
  (
    _changes: ArrayOfChanges<OrderState, GenericMeta>,
    _state: unknown,
  ): ArrayOfChanges<OrderState, GenericMeta> =>
    [
      [`orders.order_${orderIdx}.lastModified`, Date.now(), {}],
      [`orders.order_${orderIdx}.status`, 'currency_updated', {}],
      [`orders.order_${orderIdx}.updateCount`, orderIdx + 1, {}],
      [`orders.order_${orderIdx}.lineItems.0.converted`, true, {}],
      [`orders.order_${orderIdx}.lineItems.1.converted`, true, {}],
      [`orders.order_${orderIdx}.lineItems.2.converted`, true, {}],
      [`orders.order_${orderIdx}.lineItems.3.converted`, true, {}],
      [`orders.order_${orderIdx}.lineItems.4.converted`, true, {}],
      [`orders.order_${orderIdx}.subtotal`, 100 + orderIdx * 10, {}],
      [`orders.order_${orderIdx}.tax`, 20 + orderIdx * 2, {}],
      [`orders.order_${orderIdx}.total`, 120 + orderIdx * 12, {}],
      [`orders.order_${orderIdx}.discount`, orderIdx * 5, {}],
      [`orders.order_${orderIdx}.shipping`, 15, {}],
      [`orders.order_${orderIdx}.audit.updatedBy`, 'system', {}],
      [`orders.order_${orderIdx}.audit.updatedAt`, Date.now(), {}],
    ] as ArrayOfChanges<OrderState, GenericMeta>

// ---------------------------------------------------------------------------
// State builder
// ---------------------------------------------------------------------------

const buildInitialState = (): Record<string, unknown> => {
  const orders: Record<string, unknown> = {}
  const invoices: Record<string, unknown> = {}

  for (let i = 0; i < ORDER_COUNT; i++) {
    orders[`order_${i}`] = {
      currency: 'USD',
      confirmed: false,
      status: 'pending',
      lastModified: 0,
      updateCount: 0,
      subtotal: 100 + i * 10,
      tax: 20 + i * 2,
      total: 120 + i * 12,
      discount: 0,
      shipping: 15,
      lineItems: [
        { name: 'item_0', converted: false },
        { name: 'item_1', converted: false },
        { name: 'item_2', converted: false },
        { name: 'item_3', converted: false },
        { name: 'item_4', converted: false },
      ],
      audit: { updatedBy: null, updatedAt: null },
    }
    invoices[`inv_${i}`] = { pending: true }
  }

  return { orders, invoices }
}

// ---------------------------------------------------------------------------
// Listeners map builder (shared by both paths)
// ---------------------------------------------------------------------------

const buildListenersMap = () => {
  const listeners = new Map<
    string,
    ListenerRegistration<OrderState, GenericMeta>[]
  >()
  const listenerHandlers = new Map<number, ListenerHandlerRef>()
  for (let i = 0; i < ORDER_COUNT; i++) {
    const key = `orders.order_${i}`
    listeners.set(key, [
      {
        path: key as any,
        scope: key as any,
        fn: makeListenerHandler(i),
      },
    ])
    listenerHandlers.set(i, {
      scope: key,
      fn: makeListenerHandler(i) as (...args: unknown[]) => unknown,
    })
  }
  const sortedListenerPaths = Array.from(listeners.keys()).sort(
    (a, b) => getPathDepth(b) - getPathDepth(a),
  )
  return { listeners, sortedListenerPaths, listenerHandlers }
}

// ---------------------------------------------------------------------------
// Sync/flip pair definitions
// ---------------------------------------------------------------------------

const syncPairs: [string, string][] = Array.from(
  { length: ORDER_COUNT - 1 },
  (_, i) => ['orders.order_0.currency', `orders.order_${i + 1}.currency`],
)

const flipPairs: [string, string][] = Array.from(
  { length: ORDER_COUNT },
  (_, i) => [`orders.order_${i}.confirmed`, `invoices.inv_${i}.pending`],
)

// ---------------------------------------------------------------------------
// Store factory helpers
// ---------------------------------------------------------------------------

const makeStoreShell = (
  syncGraph: ReturnType<typeof createPathGroups>,
  flipGraph: ReturnType<typeof createPathGroups>,
): StoreInstance<OrderState, GenericMeta> => {
  const { listeners, sortedListenerPaths, listenerHandlers } =
    buildListenersMap()

  return {
    state: proxy(buildInitialState()),
    _concerns: proxy({}),
    _internal: {
      graphs: {
        sync: syncGraph,
        flip: flipGraph,
        listeners,
        sortedListenerPaths,
        listenerHandlers,
      },
      registrations: {
        concerns: new Map(),
        effectCleanups: new Set(),
        sideEffectCleanups: new Map(),
        aggregations: new Map(),
      },
      processing: { queue: [] },
      timing: createTiming({ timing: false, timingThreshold: 16 }),
      config: {
        errorStorePath: '_errors',
        maxIterations: 100,
        debug: { timing: false, timingThreshold: 16 },
      },
    },
  } as StoreInstance<OrderState, GenericMeta>
}

/** Reset valtio proxy + processing queue so the store can be reused. */
const resetStoreState = (store: StoreInstance<OrderState, GenericMeta>) => {
  store.state = proxy(buildInitialState())
  store._concerns = proxy({})
  store._internal.processing.queue = []
}

// ---------------------------------------------------------------------------
// Trigger
// ---------------------------------------------------------------------------

const triggerChanges = (): ArrayOfChanges<OrderState, GenericMeta> =>
  [
    ['orders.order_0.currency', 'EUR', {}],
    ['orders.order_0.confirmed', true, {}],
  ] as ArrayOfChanges<OrderState, GenericMeta>

// Expected total:
//   2 initial + 14 sync + 15 flip + (15 listeners × 15) = ~256 changes

// ---------------------------------------------------------------------------
// WASM module
// ---------------------------------------------------------------------------

let wasmModule: Record<string, unknown> | null = null

beforeAll(async () => {
  try {
    wasmModule =
      (await import('../../rust/pkg-node/apex_state_wasm.js')) as Record<
        string,
        unknown
      >
  } catch {
    // WASM not available
  }
})

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

describe('Real-World: 15 Orders Currency Change (~256 changes)', () => {
  // ---- JS-ONLY ----
  describe('JS-only pipeline', () => {
    let jsStore: StoreInstance<OrderState, GenericMeta>

    bench(
      'processChanges',
      () => {
        resetStoreState(jsStore)
        processChanges(jsStore, triggerChanges())
      },
      {
        setup: () => {
          // Ensure WASM is NOT loaded so processChanges takes JS path
          resetWasm()

          const syncGraph = createPathGroups()
          const flipGraph = createPathGroups()
          for (const [a, b] of syncPairs) addEdge(syncGraph, a, b)
          for (const [a, b] of flipPairs) addEdge(flipGraph, a, b)

          jsStore = makeStoreShell(syncGraph, flipGraph)
        },
      },
    )
  })

  // ---- WASM (full cycle including registration — cold start) ----
  describe('WASM pipeline (cold start: register + process)', () => {
    bench(
      'processChanges',
      () => {
        // Full cold start: reset, init, register, process
        resetWasm()
        initWasm(wasmModule!)

        const syncGraph = createPathGroups('sync')
        const flipGraph = createPathGroups('flip')
        for (const [a, b] of syncPairs) addEdge(syncGraph, a, b)
        for (const [a, b] of flipPairs) addEdge(flipGraph, a, b)

        registerListenersBatch(
          Array.from({ length: ORDER_COUNT }, (_, i) => ({
            subscriber_id: i,
            topic_path: `orders.order_${i}`,
            scope_path: `orders.order_${i}`,
          })),
        )
        shadowInit(buildInitialState())

        const store = makeStoreShell(syncGraph, flipGraph)
        processChanges(store, triggerChanges())
      },
      { skip: !wasmModule },
    )
  })

  // ---- WASM (warm: graphs pre-registered, only reset state) ----
  describe('WASM pipeline (warm: pre-registered, reset state only)', () => {
    let wasmStore: StoreInstance<OrderState, GenericMeta>

    bench(
      'processChanges',
      () => {
        // Only reset state — graphs, listeners, sync, flip stay registered
        shadowInit(buildInitialState())
        resetStoreState(wasmStore)
        processChanges(wasmStore, triggerChanges())
      },
      {
        skip: !wasmModule,
        setup: () => {
          if (!wasmModule) return
          resetWasm()
          initWasm(wasmModule)

          const syncGraph = createPathGroups('sync')
          const flipGraph = createPathGroups('flip')
          for (const [a, b] of syncPairs) addEdge(syncGraph, a, b)
          for (const [a, b] of flipPairs) addEdge(flipGraph, a, b)

          registerListenersBatch(
            Array.from({ length: ORDER_COUNT }, (_, i) => ({
              subscriber_id: i,
              topic_path: `orders.order_${i}`,
              scope_path: `orders.order_${i}`,
            })),
          )
          shadowInit(buildInitialState())
          wasmStore = makeStoreShell(syncGraph, flipGraph)
        },
      },
    )
  })
})
