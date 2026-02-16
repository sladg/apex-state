/**
 * E-commerce Benchmark: WASM vs Legacy Pipeline
 *
 * 16 scenarios using describe.each(MODES) — same implementation, Legacy/WASM flag switched.
 * Uses real store API: createGenericStore + mountStore + useSideEffects + useConcerns.
 *
 * Store shape: 3 departments × 5 products × 4 variants = 60 variants
 * Paths traverse 3 dynamic Record layers:
 *   catalog.departments[dept_X].products[p_X].variants[v_X].price.base
 *
 * Registrations (via real hooks):
 * - 100 BoolLogic conditions via useConcerns
 * - ~75 sync pairs via useSideEffects
 * - ~40 flip pairs via useSideEffects
 * - 75 listeners via useSideEffects
 */
import { bench, describe } from 'vitest'

import { initWasm, isWasmLoaded } from '../../src/wasm/bridge'
import {
  buildConcernRegistrations,
  buildEcommerceState,
  buildSideEffects,
  SCENARIOS,
} from '../mocks/ecommerce-bench'
import { createTestStore, MODES } from '../utils/react'

// ---------------------------------------------------------------------------
// WASM initialization — top-level await required because vitest bench
// does NOT run beforeAll/beforeEach hooks before bench() callbacks,
// and setup.ts afterEach calls resetWasm() which would wipe state.
// ---------------------------------------------------------------------------

if (!isWasmLoaded()) {
  const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
  initWasm(wasmModule)
}

// ---------------------------------------------------------------------------
// Benchmarks — describe.each(MODES) runs same code for Legacy and WASM
// ---------------------------------------------------------------------------

describe.each(MODES)(
  '[$name] E-commerce (3 Records, 60 variants, 75 syncs, 40 flips, 100 BoolLogic, 75 listeners)',
  ({ config }) => {
    // Create store directly (no React) — WASM already loaded via top-level await
    const { storeInstance, processChanges } = createTestStore(
      config,
      buildEcommerceState(),
      {
        concerns: buildConcernRegistrations(),
        sideEffects: buildSideEffects(),
        sideEffectsId: 'bench',
      },
    )

    for (const scenario of SCENARIOS) {
      bench(
        scenario.name,
        () => {
          processChanges(storeInstance, scenario.trigger())
        },
        { iterations: 10, warmupIterations: 2 },
      )
    }
  },
)
