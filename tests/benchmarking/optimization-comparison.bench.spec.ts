/**
 * Optimization Comparison Benchmark
 *
 * Tests TWO specific optimizations:
 *
 * 1. serde-wasm-bindgen vs JSON.stringify() for large objects/change sets
 *    - Measures serialization overhead for 100, 500, 1000 changes
 *    - Compares direct wasm-bindgen vs manual JSON round-trip
 *
 * 2. Single execution plan vs multiple WASM roundtrips
 *    - Measures benefit of pre-computed execution plan
 *    - Simulates old approach (multiple WASM calls) vs new (single call)
 */

import { proxy } from 'valtio/vanilla'
import { beforeAll, bench, describe } from 'vitest'

import { createPathGroups } from '../../src/core/pathGroups'
import type { StoreInstance } from '../../src/core/types'
import { processChanges } from '../../src/pipeline/processChanges'
import type { GenericMeta } from '../../src/types'
import { createTiming } from '../../src/utils/timing'
import type { Change } from '../../src/wasm/bridge'
import {
  initWasm,
  processChanges as wasmProcessChanges,
  registerListenersBatch,
  resetWasm,
  shadowInit,
} from '../../src/wasm/bridge'

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

type BenchState = Record<string, unknown>

let wasmReady = false

beforeAll(async () => {
  try {
    const wasmModule = await import('../../rust/pkg-node/apex_state_wasm.js')
    initWasm(wasmModule)
    wasmReady = true
  } catch {
    wasmReady = false
  }
})

// ---------------------------------------------------------------------------
// Test 1: Serialization Performance (serde-wasm-bindgen vs JSON.stringify)
// ---------------------------------------------------------------------------

describe('Serialization: serde-wasm-bindgen vs JSON.stringify', () => {
  /**
   * Simulates the OLD approach: manually JSON.stringify, pass string to WASM,
   * WASM does JSON.parse internally.
   */
  const processChangesWithJSONStringify = (changes: Change[]) => {
    // Simulate old approach: stringify in JS
    const jsonString = JSON.stringify(changes)
    // Simulate WASM receiving string and parsing it
    const parsedInWasm = JSON.parse(jsonString) as Change[]
    // Then call WASM with parsed data (this simulates the round-trip cost)
    return wasmProcessChanges(parsedInWasm)
  }

  /**
   * NEW approach: serde-wasm-bindgen handles serialization directly
   */
  const processChangesWithWasmBindgen = (changes: Change[]) => {
    // Direct call - wasm-bindgen handles serialization
    return wasmProcessChanges(changes)
  }

  const makeChanges = (count: number): Change[] =>
    Array.from({ length: count }, (_, i) => ({
      path: `field_${i}`,
      value: {
        id: i,
        name: `Item ${i}`,
        description: `This is item number ${i} with some extra data`,
        metadata: {
          created: Date.now(),
          updated: Date.now(),
          tags: ['tag1', 'tag2', 'tag3'],
        },
        nested: {
          level1: {
            level2: {
              level3: {
                value: i * 100,
              },
            },
          },
        },
      },
    }))

  const setupWasmForTest = (changeCount: number) => {
    resetWasm()
    const wasmModule =
      // eslint-disable-next-line @typescript-eslint/no-require-imports
      require('../../rust/pkg-node/apex_state_wasm.js') as Record<
        string,
        unknown
      >
    initWasm(wasmModule)

    const state: Record<string, unknown> = {}
    for (let i = 0; i < changeCount; i++) {
      state[`field_${i}`] = { value: i }
    }
    shadowInit(state)
  }

  bench(
    '100 changes - OLD (JSON.stringify + parse)',
    () => {
      setupWasmForTest(100)
      processChangesWithJSONStringify(makeChanges(100))
    },
    { skip: !wasmReady },
  )

  bench(
    '100 changes - NEW (serde-wasm-bindgen)',
    () => {
      setupWasmForTest(100)
      processChangesWithWasmBindgen(makeChanges(100))
    },
    { skip: !wasmReady },
  )

  bench(
    '500 changes - OLD (JSON.stringify + parse)',
    () => {
      setupWasmForTest(500)
      processChangesWithJSONStringify(makeChanges(500))
    },
    { skip: !wasmReady },
  )

  bench(
    '500 changes - NEW (serde-wasm-bindgen)',
    () => {
      setupWasmForTest(500)
      processChangesWithWasmBindgen(makeChanges(500))
    },
    { skip: !wasmReady },
  )

  bench(
    '1000 changes - OLD (JSON.stringify + parse)',
    () => {
      setupWasmForTest(1000)
      processChangesWithJSONStringify(makeChanges(1000))
    },
    { skip: !wasmReady },
  )

  bench(
    '1000 changes - NEW (serde-wasm-bindgen)',
    () => {
      setupWasmForTest(1000)
      processChangesWithWasmBindgen(makeChanges(1000))
    },
    { skip: !wasmReady },
  )
})

// ---------------------------------------------------------------------------
// Test 2: Execution Plan - Single Call vs Multiple Roundtrips
// ---------------------------------------------------------------------------

describe('Execution Plan: Single call vs Multiple roundtrips', () => {
  const ORDER_COUNT = 20

  const buildInitialState = (): Record<string, unknown> => {
    const orders: Record<string, unknown> = {}
    for (let i = 0; i < ORDER_COUNT; i++) {
      orders[`order_${i}`] = {
        currency: 'USD',
        total: 100 + i * 10,
        status: 'pending',
      }
    }
    return { orders }
  }

  const makeListenerHandler = (orderIdx: number) => (): [string, unknown][] => [
    [`orders.order_${orderIdx}.status`, 'processed'],
    [`orders.order_${orderIdx}.total`, 100 + orderIdx * 10],
  ]

  const buildListenersMap = () => {
    const listenerHandlers = new Map()
    for (let i = 0; i < ORDER_COUNT; i++) {
      listenerHandlers.set(i, {
        scope: `orders.order_${i}`,
        fn: makeListenerHandler(i),
      })
    }
    return listenerHandlers
  }

  const makeStoreShell = (): StoreInstance<BenchState, GenericMeta> => {
    const syncGraph = createPathGroups('sync')
    const flipGraph = createPathGroups('flip')

    return {
      state: proxy(buildInitialState()),
      _concerns: proxy({}),
      _internal: {
        graphs: {
          sync: syncGraph,
          flip: flipGraph,
          listeners: new Map(),
          sortedListenerPaths: [],
          listenerHandlers: buildListenersMap(),
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
    } as StoreInstance<BenchState, GenericMeta>
  }

  /**
   * NEW approach: Single WASM call returns complete execution plan.
   * TypeScript executes all listeners in one loop, no additional WASM calls.
   */
  bench(
    'NEW: Single WASM call + TS execution loop (20 listeners)',
    () => {
      resetWasm()
      const wasmModule =
        // eslint-disable-next-line @typescript-eslint/no-require-imports
        require('../../rust/pkg-node/apex_state_wasm.js') as Record<
          string,
          unknown
        >
      initWasm(wasmModule)
      shadowInit(buildInitialState())

      registerListenersBatch(
        Array.from({ length: ORDER_COUNT }, (_, i) => ({
          subscriber_id: i,
          topic_path: `orders.order_${i}`,
          scope_path: `orders.order_${i}`,
        })),
      )

      const store = makeStoreShell()
      // This does: WASM call (returns plan) → TS loop (executes plan)
      processChanges(store, [['orders.order_0.currency', 'EUR', {}]])
    },
    { skip: !wasmReady },
  )

  /**
   * OLD approach (simulated): Multiple WASM roundtrips.
   * Call WASM once, get plan, then call WASM again for each listener dispatch.
   */
  bench(
    'OLD: Multiple WASM roundtrips (20 listeners, simulated)',
    () => {
      resetWasm()
      const wasmModule =
        // eslint-disable-next-line @typescript-eslint/no-require-imports
        require('../../rust/pkg-node/apex_state_wasm.js') as Record<
          string,
          unknown
        >
      initWasm(wasmModule)
      shadowInit(buildInitialState())

      registerListenersBatch(
        Array.from({ length: ORDER_COUNT }, (_, i) => ({
          subscriber_id: i,
          topic_path: `orders.order_${i}`,
          scope_path: `orders.order_${i}`,
        })),
      )

      // First WASM call
      wasmProcessChanges([{ path: 'orders.order_0.currency', value: 'EUR' }])

      // Simulate multiple roundtrips (old approach had ~17 WASM calls)
      // Each listener execution would route back to WASM
      for (let i = 0; i < ORDER_COUNT; i++) {
        // Simulate routing produced changes back to WASM
        const changes: Change[] = [
          { path: `orders.order_${i}.status`, value: 'processed' },
        ]
        // This represents the OLD approach: routeProducedChanges() → WASM
        JSON.stringify(changes) // Simulate stringify overhead
        JSON.parse(JSON.stringify(changes)) // Simulate parse overhead
        // Then call WASM again (boundary crossing)
        wasmProcessChanges(changes)
      }
    },
    { skip: !wasmReady },
  )
})
