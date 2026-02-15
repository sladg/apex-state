/**
 * Execution Plan: Properly Isolated Benchmark
 *
 * ONLY measures the execution time, NOT setup/initialization.
 *
 * NEW: Single wasm.processChanges() call (returns plan, TS executes)
 * OLD: Multiple WASM roundtrips (process + plan + route per level)
 */

import { proxy } from 'valtio/vanilla'
import { beforeAll, beforeEach, bench, describe } from 'vitest'

import { createPathGroups } from '../../src/core/pathGroups'
import type { StoreInstance } from '../../src/core/types'
import type { GenericMeta } from '../../src/types'
import { createTiming } from '../../src/utils/timing'
import type { Change } from '../../src/wasm/bridge'
import { initWasm, resetWasm } from '../../src/wasm/bridge'

type BenchState = Record<string, unknown>

let wasmReady = false
let wasmModule: Record<string, unknown> | null = null

beforeAll(async () => {
  try {
    wasmModule = (await import('../../rust/pkg/apex_state_wasm.js')) as Record<
      string,
      unknown
    >
    wasmReady = true
  } catch {
    wasmReady = false
  }
})

// ---------------------------------------------------------------------------
// Shared test setup: 50 listeners across 15 depth levels
// ---------------------------------------------------------------------------

const TOTAL_LISTENERS = 50
const DEPTH_LEVELS = 15
const ROOT_LISTENERS = 10 // At least 10 root listeners

// Distribution: 10 at root, 40 scattered across levels 1-14
const LISTENERS_DISTRIBUTION = [
  ROOT_LISTENERS, // Level 0 (root)
  5,
  4,
  4,
  3,
  3,
  3,
  3,
  3,
  2,
  2,
  2,
  2,
  2,
  2, // Levels 1-14
]

const buildNestedState = (): Record<string, unknown> => {
  const state: Record<string, unknown> = {}
  let listenerId = 0

  for (let level = 0; level < DEPTH_LEVELS; level++) {
    const count = LISTENERS_DISTRIBUTION[level]
    for (let i = 0; i < count; i++) {
      state[`level${level}_item${i}`] = {
        value: listenerId++,
        status: 'pending',
      }
    }
  }

  return state
}

const makeListenerHandlers = () => {
  const handlers = new Map()
  let listenerId = 0

  for (let level = 0; level < DEPTH_LEVELS; level++) {
    const count = LISTENERS_DISTRIBUTION[level]

    for (let i = 0; i < count; i++) {
      const currentId = listenerId++
      const currentPath = `level${level}_item${i}`

      // Each listener produces changes for 1-2 listeners at the next level
      handlers.set(currentId, {
        scope: currentPath,
        fn: (): [string, unknown][] => {
          if (level >= DEPTH_LEVELS - 1) {
            // Last level: no propagation
            return []
          }

          const nextLevel = level + 1
          const nextLevelCount = LISTENERS_DISTRIBUTION[nextLevel]

          // Produce changes for next level (1-2 targets)
          const changes: [string, unknown][] = []
          const target1 = i % nextLevelCount
          changes.push([`level${nextLevel}_item${target1}.value`, currentId])

          // Sometimes produce a second change
          if (nextLevelCount > 1 && i % 2 === 0) {
            const target2 = (i + 1) % nextLevelCount
            changes.push([
              `level${nextLevel}_item${target2}.value`,
              currentId + 1000,
            ])
          }

          return changes
        },
      })
    }
  }

  return handlers
}

const makeStoreShell = (): StoreInstance<BenchState, GenericMeta> => ({
  state: proxy(buildNestedState()),
  _concerns: proxy({}),
  _internal: {
    graphs: {
      sync: createPathGroups('sync'),
      flip: createPathGroups('flip'),
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
})

const setupWasmEnvironment = () => {
  if (!wasmModule) return
  resetWasm()
  initWasm(wasmModule)
  wasm.shadowInit(buildNestedState())

  const registrations = []
  let listenerId = 0

  for (let level = 0; level < DEPTH_LEVELS; level++) {
    const count = LISTENERS_DISTRIBUTION[level]
    for (let i = 0; i < count; i++) {
      const path = `level${level}_item${i}`
      registrations.push({
        subscriber_id: listenerId++,
        topic_path: path,
        scope_path: path,
      })
    }
  }

  wasm.registerListenersBatch(registrations)
}

// ---------------------------------------------------------------------------
// Benchmark: Real-world scenario
// ---------------------------------------------------------------------------

describe('Execution Plan: 50 listeners across 15 depth levels', () => {
  let store: StoreInstance<BenchState, GenericMeta>

  beforeEach(() => {
    if (!wasmReady) return
    setupWasmEnvironment()
    store = makeStoreShell()
  })

  bench(
    'NEW: Single WASM call (returns full plan)',
    () => {
      // ONLY measure this call - setup already done
      // Trigger 10 root listeners at level 0
      wasm.processChanges(store, [['level0_item0.value', 100, {}]])
    },
    {
      skip: !wasmReady,
      setup() {
        setupWasmEnvironment()
        store = makeStoreShell()
      },
    },
  )

  bench(
    'OLD: Multiple WASM calls (process + plan + route per level)',
    () => {
      const handlers = makeListenerHandlers()

      // WASM call 1: process
      const result = wasm.processChanges([
        { path: 'level0_item0.value', value: 100 },
      ])

      // WASM call 2: create plan
      const plan = wasm.createDispatchPlan(result.changes)

      // WASM calls 3+: route per level (up to 15 calls)
      for (const level of plan.levels) {
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
    {
      skip: !wasmReady,
      setup() {
        setupWasmEnvironment()
      },
    },
  )
})

// ---------------------------------------------------------------------------
// Benchmark: Scaling with depth
// ---------------------------------------------------------------------------

describe('Scaling: More depth = more OLD overhead', () => {
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

  const setupDeepWasm = (levels: number) => {
    if (!wasmModule) return
    resetWasm()
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
  }

  const testDepth = (levels: number) => {
    let store: StoreInstance<BenchState, GenericMeta>

    bench(
      `NEW: ${levels} levels (1 call)`,
      () => {
        wasm.processChanges(store, [['level0_item0.value', 999, {}]])
      },
      {
        skip: !wasmReady,
        setup() {
          setupDeepWasm(levels)
          store = makeStoreShell()
          store._internal.graphs.listenerHandlers = makeDeepHandlers(levels)
          store.state = proxy(makeDeepState(levels))
        },
      },
    )

    bench(
      `OLD: ${levels} levels (${2 + levels} calls)`,
      () => {
        const handlers = makeDeepHandlers(levels)

        const result = wasm.processChanges([
          { path: 'level0_item0.value', value: 999 },
        ])
        const plan = wasm.createDispatchPlan(result.changes)

        for (const level of plan.levels) {
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
      {
        skip: !wasmReady,
        setup() {
          setupDeepWasm(levels)
        },
      },
    )
  }

  testDepth(5)
  testDepth(10)
  testDepth(20)
})
