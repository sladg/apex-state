/**
 * WASM Pipeline Performance Benchmarks
 *
 * Baseline benchmarks for processChanges + BoolLogic evaluation.
 * Sync/flip/aggregation and listener dispatch benchmarks are marked
 * with skip until those WASM exports are compiled.
 */
import { beforeAll, bench, describe } from 'vitest'

import type { Change } from '../../src/wasm/bridge'
import { initWasm } from '../../src/wasm/bridge'

let wasmAvailable = false
let wasmModule: Record<string, unknown> | null = null

beforeAll(async () => {
  try {
    wasmModule = (await import('../../rust/pkg/apex_state_wasm.js')) as Record<
      string,
      unknown
    >
    initWasm(wasmModule)
    wasmAvailable = true
  } catch {
    wasmAvailable = false
  }
})

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const setupState = (fieldCount: number) => {
  wasm.pipelineReset()
  const state: Record<string, number> = {}
  for (let i = 0; i < fieldCount; i++) {
    state[`field${i}`] = 0
  }
  wasm.shadowInit(state)
}

const setupStateWithBoolLogic = (
  fieldCount: number,
  boolLogicCount: number,
) => {
  setupState(fieldCount)
  for (let i = 0; i < boolLogicCount; i++) {
    wasm.registerBoolLogic(`_concerns.field${i}.active`, {
      GTE: [`field${i}`, 50],
    })
  }
}

const makeChanges = (count: number): Change[] =>
  Array.from({ length: count }, (_, i) => ({
    path: `field${i}`,
    value: i + 100,
  }))

// ---------------------------------------------------------------------------
// processChanges throughput (available now)
// ---------------------------------------------------------------------------

describe('WASM processChanges Throughput', () => {
  bench(
    '5 changes, no BoolLogic',
    () => {
      setupState(20)
      wasm.processChanges(makeChanges(5))
    },
    { skip: !wasmAvailable },
  )

  bench(
    '50 changes, no BoolLogic',
    () => {
      setupState(100)
      wasm.processChanges(makeChanges(50))
    },
    { skip: !wasmAvailable },
  )

  bench(
    '200 changes, no BoolLogic',
    () => {
      setupState(300)
      wasm.processChanges(makeChanges(200))
    },
    { skip: !wasmAvailable },
  )
})

describe('WASM processChanges + BoolLogic', () => {
  bench(
    '5 changes, 5 BoolLogics',
    () => {
      setupStateWithBoolLogic(20, 5)
      wasm.processChanges(makeChanges(5))
    },
    { skip: !wasmAvailable },
  )

  bench(
    '50 changes, 10 BoolLogics',
    () => {
      setupStateWithBoolLogic(100, 10)
      wasm.processChanges(makeChanges(50))
    },
    { skip: !wasmAvailable },
  )

  bench(
    '200 changes, 20 BoolLogics',
    () => {
      setupStateWithBoolLogic(300, 20)
      wasm.processChanges(makeChanges(200))
    },
    { skip: !wasmAvailable },
  )
})

// ---------------------------------------------------------------------------
// Sync/flip/aggregation + listener dispatch (pending WASM compilation)
// ---------------------------------------------------------------------------

const hasEp2 =
  wasmAvailable &&
  wasmModule !== null &&
  typeof (wasmModule as Record<string, unknown>).register_sync_batch ===
    'function'

const hasEp3 =
  wasmAvailable &&
  wasmModule !== null &&
  typeof (wasmModule as Record<string, unknown>).register_listeners_batch ===
    'function'

describe('WASM Full Pipeline with sync + flip (pending)', () => {
  bench(
    '5 changes through full pipeline with sync + flip',
    () => {
      // Will be enabled when WASM exports are available
    },
    { skip: !hasEp2 },
  )

  bench(
    '50 changes through full pipeline with sync + flip',
    () => {
      // Will be enabled when WASM exports are available
    },
    { skip: !hasEp2 },
  )
})

describe('WASM Dispatch Plan (pending)', () => {
  bench(
    'dispatch plan: 10 listeners, 20 changes',
    () => {
      // Will be enabled when WASM exports are available
    },
    { skip: !hasEp3 },
  )
})

describe('WASM Full Cycle: process + dispatch (pending)', () => {
  bench(
    'process 20 changes + create dispatch plan',
    () => {
      // Will be enabled when WASM exports are available
    },
    { skip: !hasEp3 },
  )
})
