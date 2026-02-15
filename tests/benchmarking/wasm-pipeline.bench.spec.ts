/**
 * WASM vs JS Pipeline Performance Comparison
 *
 * Compares WASM-accelerated pipeline against JS-only pipeline
 * for: processChanges, sync, flip, BoolLogic, dispatch plans.
 */
import { proxy } from 'valtio/vanilla'
import { beforeAll, bench, describe } from 'vitest'

import { addEdge, createPathGroups } from '../../src/core/pathGroups'
import type { StoreInstance } from '../../src/core/types'
import { processChanges as processChangesEntry } from '../../src/pipeline/processChanges'
import type { GenericMeta } from '../../src/types'
import { createTiming } from '../../src/utils/timing'
import type { Change } from '../../src/wasm/bridge'
import {
  createDispatchPlan,
  initWasm,
  processChanges as wasmProcessChanges,
  registerBoolLogic,
  registerFlipBatch,
  registerListenersBatch,
  registerSyncBatch,
  resetWasm,
  shadowInit,
} from '../../src/wasm/bridge'
import { typeHelpers } from '../mocks/helpers'

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
// WASM helpers
// ---------------------------------------------------------------------------

const setupWasm = (fields: number) => {
  resetWasm()
  const wasmModule =
    // eslint-disable-next-line @typescript-eslint/no-require-imports
    require('../../rust/pkg-node/apex_state_wasm.js') as Record<string, unknown>
  initWasm(wasmModule)
  const state: Record<string, unknown> = {}
  for (let i = 0; i < fields; i++) {
    state[`f${i}`] = i
  }
  shadowInit(state)
}

const makeChanges = (count: number): Change[] =>
  Array.from({ length: count }, (_, i) => ({
    path: `f${i}`,
    value: i + 1000,
  }))

// ---------------------------------------------------------------------------
// JS mock store helpers
// ---------------------------------------------------------------------------

const createJSStore = (
  syncPaths: [string, string][] = [],
  flipPaths: [string, string][] = [],
): StoreInstance<BenchState, GenericMeta> => {
  const syncGraph = createPathGroups()
  const flipGraph = createPathGroups()

  for (const [p1, p2] of syncPaths) addEdge(syncGraph, p1, p2)
  for (const [p1, p2] of flipPaths) addEdge(flipGraph, p1, p2)

  const state: Record<string, unknown> = {}
  for (let i = 0; i < 100; i++) state[`f${i}`] = i

  return {
    state: proxy(state),
    _concerns: proxy({}),
    _internal: {
      graphs: {
        sync: syncGraph,
        flip: flipGraph,
        listeners: new Map(),
        sortedListenerPaths: [],
        listenerHandlers: new Map(),
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

// =============================================================================
// WASM-only: raw processChanges throughput
// =============================================================================

describe('WASM raw processChanges', () => {
  bench(
    '5 changes',
    () => {
      setupWasm(20)
      wasmProcessChanges(makeChanges(5))
    },
    { skip: !wasmReady },
  )

  bench(
    '50 changes',
    () => {
      setupWasm(100)
      wasmProcessChanges(makeChanges(50))
    },
    { skip: !wasmReady },
  )

  bench(
    '200 changes',
    () => {
      setupWasm(300)
      wasmProcessChanges(makeChanges(200))
    },
    { skip: !wasmReady },
  )
})

// =============================================================================
// WASM processChanges + BoolLogic
// =============================================================================

describe('WASM processChanges + BoolLogic', () => {
  bench(
    '5 changes, 5 BoolLogics',
    () => {
      setupWasm(20)
      for (let i = 0; i < 5; i++) {
        registerBoolLogic(`_concerns.f${i}.active`, { GTE: [`f${i}`, 50] })
      }
      wasmProcessChanges(makeChanges(5))
    },
    { skip: !wasmReady },
  )

  bench(
    '50 changes, 10 BoolLogics',
    () => {
      setupWasm(100)
      for (let i = 0; i < 10; i++) {
        registerBoolLogic(`_concerns.f${i}.active`, { GTE: [`f${i}`, 50] })
      }
      wasmProcessChanges(makeChanges(50))
    },
    { skip: !wasmReady },
  )

  bench(
    '200 changes, 20 BoolLogics',
    () => {
      setupWasm(300)
      for (let i = 0; i < 20; i++) {
        registerBoolLogic(`_concerns.f${i}.active`, { GTE: [`f${i}`, 50] })
      }
      wasmProcessChanges(makeChanges(200))
    },
    { skip: !wasmReady },
  )
})

// =============================================================================
// WASM processChanges with sync + flip
// =============================================================================

describe('WASM processChanges + sync/flip', () => {
  bench(
    '5 changes, 5 sync pairs',
    () => {
      setupWasm(20)
      const pairs: [string, string][] = Array.from({ length: 5 }, (_, i) => [
        `f${i}`,
        `f${i + 10}`,
      ])
      registerSyncBatch(pairs)
      wasmProcessChanges(makeChanges(5))
    },
    { skip: !wasmReady },
  )

  bench(
    '50 changes, 20 sync + 10 flip pairs',
    () => {
      setupWasm(100)
      const syncPairs: [string, string][] = Array.from(
        { length: 20 },
        (_, i) => [`f${i}`, `f${i + 50}`],
      )
      const flipPairs: [string, string][] = Array.from(
        { length: 10 },
        (_, i) => [`f${i + 20}`, `f${i + 70}`],
      )
      registerSyncBatch(syncPairs)
      registerFlipBatch(flipPairs)
      wasmProcessChanges(makeChanges(50))
    },
    { skip: !wasmReady },
  )
})

// =============================================================================
// WASM dispatch plan creation
// =============================================================================

describe('WASM dispatch plan', () => {
  bench(
    '10 listeners, 5 changes',
    () => {
      setupWasm(20)
      registerListenersBatch(
        Array.from({ length: 10 }, (_, i) => ({
          subscriber_id: i,
          topic_path: `f${i % 5}`,
          scope_path: `f${i % 5}`,
        })),
      )
      createDispatchPlan(makeChanges(5))
    },
    { skip: !wasmReady },
  )

  bench(
    '50 listeners, 20 changes',
    () => {
      setupWasm(50)
      registerListenersBatch(
        Array.from({ length: 50 }, (_, i) => ({
          subscriber_id: i,
          topic_path: `f${i % 20}`,
          scope_path: `f${i % 20}`,
        })),
      )
      createDispatchPlan(makeChanges(20))
    },
    { skip: !wasmReady },
  )
})

// =============================================================================
// WASM full cycle: process + BoolLogic + sync + flip + dispatch
// =============================================================================

describe('WASM full pipeline cycle', () => {
  bench(
    '20 changes, 10 sync, 5 flip, 10 BoolLogic, 20 listeners',
    () => {
      setupWasm(60)
      registerSyncBatch(
        Array.from(
          { length: 10 },
          (_, i) => [`f${i}`, `f${i + 30}`] as [string, string],
        ),
      )
      registerFlipBatch(
        Array.from(
          { length: 5 },
          (_, i) => [`f${i + 10}`, `f${i + 40}`] as [string, string],
        ),
      )
      for (let i = 0; i < 10; i++) {
        registerBoolLogic(`_concerns.f${i}.active`, { GTE: [`f${i}`, 50] })
      }
      registerListenersBatch(
        Array.from({ length: 20 }, (_, i) => ({
          subscriber_id: i,
          topic_path: `f${i % 15}`,
          scope_path: `f${i % 15}`,
        })),
      )

      const results = wasmProcessChanges(makeChanges(20))
      const stateChanges = results.filter(
        (c) => !c.path.startsWith('_concerns.'),
      )
      createDispatchPlan(stateChanges)
    },
    { skip: !wasmReady },
  )
})

// =============================================================================
// JS-only pipeline (for direct comparison)
// =============================================================================

describe('JS pipeline (no WASM)', () => {
  bench('simple mutation, no relationships', () => {
    const store = createJSStore()
    processChangesEntry(store, typeHelpers.changes<BenchState>([['f0', 'new']]))
  })

  bench('5 changes, 5 sync pairs', () => {
    const syncPairs: [string, string][] = Array.from({ length: 5 }, (_, i) => [
      `f${i}`,
      `f${i + 10}`,
    ])
    const store = createJSStore(syncPairs)
    processChangesEntry(
      store,
      typeHelpers.changes<BenchState>(
        Array.from({ length: 5 }, (_, i) => [`f${i}`, i + 1000]),
      ),
    )
  })

  bench('50 changes, 20 sync + 10 flip pairs', () => {
    const syncPairs: [string, string][] = Array.from({ length: 20 }, (_, i) => [
      `f${i}`,
      `f${i + 50}`,
    ])
    const flipPairs: [string, string][] = Array.from({ length: 10 }, (_, i) => [
      `f${i + 20}`,
      `f${i + 70}`,
    ])
    const store = createJSStore(syncPairs, flipPairs)
    processChangesEntry(
      store,
      typeHelpers.changes<BenchState>(
        Array.from({ length: 50 }, (_, i) => [`f${i}`, i + 1000]),
      ),
    )
  })
})
