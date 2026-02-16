/**
 * Execution Plan Architecture Comparison
 *
 * Compares the OLD vs NEW listener dispatch approaches:
 *
 * OLD (multi-call):
 *   1. wasm.processChanges() → WASM
 *   2. wasm.createDispatchPlan() → WASM
 *   3. For each depth level:
 *      - Execute listeners
 *      - wasm.routeProducedChanges(depth) → WASM (per level!)
 *   Total: 1 + 1 + depth_levels WASM calls
 *
 * NEW (single plan):
 *   1. wasm.processChanges() → WASM (returns FullExecutionPlan)
 *   2. Execute all listeners in TypeScript loop (no more WASM calls)
 *   Total: 1 WASM call
 *
 * Expected: NEW should be 2-3x faster for scenarios with multiple depth levels
 */

import { proxy } from 'valtio/vanilla'
import { beforeAll, bench, describe } from 'vitest'

import { createPathGroups } from '../../src/core/path-groups'
import type { StoreInstance } from '../../src/core/types'
import type { GenericMeta } from '../../src/types'
import { createTiming } from '../../src/utils/timing'
import type { Change } from '../../src/wasm/bridge'
import { initWasm, resetWasm, wasm } from '../../src/wasm/bridge'

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

type BenchState = Record<string, unknown>

let wasmReady = false

beforeAll(async () => {
  try {
    const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
    initWasm(wasmModule)
    wasmReady = true
  } catch {
    wasmReady = false
  }
})

// ---------------------------------------------------------------------------
// Test Data: Nested listeners (3 depth levels)
// ---------------------------------------------------------------------------

const LISTENERS_PER_LEVEL = 10

const buildNestedState = (): Record<string, unknown> => {
  const state: Record<string, unknown> = {}

  // Level 0 (root): 10 items
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    state[`item_${i}`] = { value: i, status: 'pending' }
  }

  // Level 1: 10 collections
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    state[`collection_${i}`] = { count: 0 }
  }

  // Level 2: 10 aggregates
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    state[`aggregate_${i}`] = { total: 0 }
  }

  return state
}

const makeListenerHandlers = () => {
  const handlers = new Map()

  // Level 0 handlers: produce changes for level 1
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    handlers.set(i, {
      scope: `item_${i}`,
      fn: (): [string, unknown][] => [
        [`collection_${i % LISTENERS_PER_LEVEL}.count`, 1],
      ],
    })
  }

  // Level 1 handlers: produce changes for level 2
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    handlers.set(LISTENERS_PER_LEVEL + i, {
      scope: `collection_${i}`,
      fn: (): [string, unknown][] => [
        [`aggregate_${i % LISTENERS_PER_LEVEL}.total`, 10],
      ],
    })
  }

  // Level 2 handlers: final level (no further propagation)
  for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
    handlers.set(2 * LISTENERS_PER_LEVEL + i, {
      scope: `aggregate_${i}`,
      fn: (): [string, unknown][] => [],
    })
  }

  return handlers
}

const makeStoreShell = (): StoreInstance<BenchState, GenericMeta> => {
  const syncGraph = createPathGroups('sync')
  const flipGraph = createPathGroups('flip')

  return {
    state: proxy(buildNestedState()),
    _concerns: proxy({}),
    _internal: {
      graphs: {
        sync: syncGraph,
        flip: flipGraph,
        listeners: new Map(),
        sortedListenerPaths: [],
        listenerHandlers: makeListenerHandlers(),
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

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

describe('Execution Plan: Single call vs Multi-call (REAL implementations)', () => {
  /**
   * NEW APPROACH: Single wasm.processChanges() returns FullExecutionPlan
   * TypeScript executes all listeners with no additional WASM calls
   */
  bench(
    'NEW: Single plan (1 WASM call total)',
    () => {
      resetWasm()
      const wasmModule =
        // eslint-disable-next-line @typescript-eslint/no-require-imports
        require('../../rust/pkg/apex_state_wasm.js') as Record<string, unknown>
      initWasm(wasmModule)
      wasm.shadowInit(buildNestedState())

      // Register all listeners
      const registrations = []
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: i,
          topic_path: `item_${i}`,
          scope_path: `item_${i}`,
        })
      }
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: LISTENERS_PER_LEVEL + i,
          topic_path: `collection_${i}`,
          scope_path: `collection_${i}`,
        })
      }
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: 2 * LISTENERS_PER_LEVEL + i,
          topic_path: `aggregate_${i}`,
          scope_path: `aggregate_${i}`,
        })
      }
      wasm.registerListenersBatch(registrations)

      const store = makeStoreShell()

      // Single call - returns complete execution plan
      // TypeScript then executes all listeners with no further WASM calls
      wasm.processChanges(store, [['item_0.value', 100, {}]])
    },
    { skip: !wasmReady },
  )

  /**
   * OLD APPROACH: Multiple WASM calls
   * 1. wasm.processChanges() - initial
   * 2. wasm.createDispatchPlan() - get first level
   * 3. wasm.routeProducedChanges() - for each depth level after execution
   *
   * Total: 1 (process) + 1 (create plan) + 3 (route per level) = 5 WASM calls
   */
  bench(
    'OLD: Multi-call (5 WASM calls: process + plan + 3 routing)',
    () => {
      resetWasm()
      const wasmModule =
        // eslint-disable-next-line @typescript-eslint/no-require-imports
        require('../../rust/pkg/apex_state_wasm.js') as Record<string, unknown>
      initWasm(wasmModule)
      wasm.shadowInit(buildNestedState())

      // Register all listeners
      const registrations = []
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: i,
          topic_path: `item_${i}`,
          scope_path: `item_${i}`,
        })
      }
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: LISTENERS_PER_LEVEL + i,
          topic_path: `collection_${i}`,
          scope_path: `collection_${i}`,
        })
      }
      for (let i = 0; i < LISTENERS_PER_LEVEL; i++) {
        registrations.push({
          subscriber_id: 2 * LISTENERS_PER_LEVEL + i,
          topic_path: `aggregate_${i}`,
          scope_path: `aggregate_${i}`,
        })
      }
      wasm.registerListenersBatch(registrations)

      const handlers = makeListenerHandlers()

      // WASM call 1: Process initial changes
      const result = wasm.processChanges([{ path: 'item_0.value', value: 100 }])

      // WASM call 2: Create initial dispatch plan
      const initialPlan = wasm.createDispatchPlan(result.changes)

      // Execute each depth level with WASM routing calls
      for (const level of initialPlan.levels) {
        const producedChanges: Change[] = []

        // Execute listeners at this depth
        for (const dispatch of level.dispatches) {
          const handler = handlers.get(dispatch.subscriber_id)
          if (handler) {
            const result = handler.fn()
            for (const [path, value] of result) {
              producedChanges.push({ path, value })
            }
          }
        }

        // WASM call 3+: Route produced changes to next level
        if (producedChanges.length > 0) {
          wasm.routeProducedChanges(level.depth, producedChanges)
        }
      }
    },
    { skip: !wasmReady },
  )
})

// ---------------------------------------------------------------------------
// Varying depth levels to show scaling
// ---------------------------------------------------------------------------

describe('Scaling: More depth levels = bigger gap', () => {
  const makeDeepState = (levels: number) => {
    const state: Record<string, unknown> = {}
    for (let lvl = 0; lvl < levels; lvl++) {
      for (let i = 0; i < 5; i++) {
        state[`level${lvl}_item${i}`] = { value: i }
      }
    }
    return state
  }

  const makeDeepHandlers = (levels: number) => {
    const handlers = new Map()
    for (let lvl = 0; lvl < levels; lvl++) {
      for (let i = 0; i < 5; i++) {
        const id = lvl * 5 + i
        handlers.set(id, {
          scope: `level${lvl}_item${i}`,
          fn: (): [string, unknown][] => {
            // Produce changes for next level if not last
            if (lvl < levels - 1) {
              return [[`level${lvl + 1}_item${i % 5}.value`, i * 10]]
            }
            return []
          },
        })
      }
    }
    return handlers
  }

  const testDepth = (levels: number) => {
    bench(
      `NEW: ${levels} depth levels (1 WASM call)`,
      () => {
        resetWasm()
        const wasmModule =
          // eslint-disable-next-line @typescript-eslint/no-require-imports
          require('../../rust/pkg/apex_state_wasm.js') as Record<
            string,
            unknown
          >
        initWasm(wasmModule)
        wasm.shadowInit(makeDeepState(levels))

        const registrations = []
        for (let lvl = 0; lvl < levels; lvl++) {
          for (let i = 0; i < 5; i++) {
            registrations.push({
              subscriber_id: lvl * 5 + i,
              topic_path: `level${lvl}_item${i}`,
              scope_path: `level${lvl}_item${i}`,
            })
          }
        }
        wasm.registerListenersBatch(registrations)

        const store = makeStoreShell()
        store._internal.graphs.listenerHandlers = makeDeepHandlers(levels)
        store.state = proxy(makeDeepState(levels))

        wasm.processChanges(store, [['level0_item0.value', 999, {}]])
      },
      { skip: !wasmReady },
    )

    bench(
      `OLD: ${levels} depth levels (${1 + 1 + levels} WASM calls)`,
      () => {
        resetWasm()
        const wasmModule =
          // eslint-disable-next-line @typescript-eslint/no-require-imports
          require('../../rust/pkg/apex_state_wasm.js') as Record<
            string,
            unknown
          >
        initWasm(wasmModule)
        wasm.shadowInit(makeDeepState(levels))

        const registrations = []
        for (let lvl = 0; lvl < levels; lvl++) {
          for (let i = 0; i < 5; i++) {
            registrations.push({
              subscriber_id: lvl * 5 + i,
              topic_path: `level${lvl}_item${i}`,
              scope_path: `level${lvl}_item${i}`,
            })
          }
        }
        wasm.registerListenersBatch(registrations)

        const handlers = makeDeepHandlers(levels)

        // Initial process
        const result = wasm.processChanges([
          { path: 'level0_item0.value', value: 999 },
        ])
        const initialPlan = wasm.createDispatchPlan(result.changes)

        // Route through each level
        for (const level of initialPlan.levels) {
          const producedChanges: Change[] = []
          for (const dispatch of level.dispatches) {
            const handler = handlers.get(dispatch.subscriber_id)
            if (handler) {
              const result = handler.fn()
              for (const [path, value] of result) {
                producedChanges.push({ path, value })
              }
            }
          }
          if (producedChanges.length > 0) {
            wasm.routeProducedChanges(level.depth, producedChanges)
          }
        }
      },
      { skip: !wasmReady },
    )
  }

  testDepth(5) // 5 levels: 1+1+5 = 7 WASM calls vs 1
  testDepth(10) // 10 levels: 1+1+10 = 12 WASM calls vs 1
})
