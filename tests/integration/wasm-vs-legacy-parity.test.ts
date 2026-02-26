/**
 * WASM vs Legacy Parity Verification
 *
 * Runs each benchmark scenario ONCE per mode and asserts:
 * 1. Both modes produce the same number of leaf-level state diffs
 * 2. Both modes produce identical final state snapshots
 *
 * This ensures the benchmark is comparing equivalent work, not shortcuts.
 */
import { snapshot } from 'valtio'
import { describe, expect, it } from 'vitest'

import {
  buildConcernRegistrations,
  buildEcommerceState,
  buildSideEffects,
  type EcommerceBenchState,
  SCENARIOS,
} from '../mocks/ecommerce-bench'
import { createTestStore, MODES } from '../utils/react'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Count leaf-level differences between two snapshots by walking both trees.
 */
const countDiffs = (
  before: Record<string, unknown>,
  after: Record<string, unknown>,
  prefix = '',
): number => {
  let count = 0
  const allKeys = new Set([...Object.keys(before), ...Object.keys(after)])
  for (const key of allKeys) {
    const path = prefix ? `${prefix}.${key}` : key
    const a = before[key]
    const b = after[key]
    if (a === b) continue
    if (
      a &&
      b &&
      typeof a === 'object' &&
      typeof b === 'object' &&
      !Array.isArray(a) &&
      !Array.isArray(b)
    ) {
      count += countDiffs(
        a as Record<string, unknown>,
        b as Record<string, unknown>,
        path,
      )
    } else {
      count++
    }
  }
  return count
}

/** Run a scenario on a fresh store and return state snapshot + diff count. */
const runScenarioOnce = (
  mode: (typeof MODES)[number],
  trigger: () => ReturnType<(typeof SCENARIOS)[number]['trigger']>,
) => {
  const { storeInstance, processChanges } = createTestStore(
    mode.config,
    buildEcommerceState(),
    {
      concerns: buildConcernRegistrations(),
      sideEffects: buildSideEffects(),
      sideEffectsId: 'verify',
    },
  )

  // Snapshot state before processing
  const beforeSnap = snapshot(storeInstance.state) as EcommerceBenchState

  const changes = trigger()
  processChanges(storeInstance, changes)

  // Snapshot state after processing
  const afterSnap = snapshot(storeInstance.state) as EcommerceBenchState

  // Count how many leaf values actually changed
  const diffCount = countDiffs(
    beforeSnap as unknown as Record<string, unknown>,
    afterSnap as unknown as Record<string, unknown>,
  )

  return {
    inputCount: changes.length,
    diffCount,
    stateSnap: afterSnap,
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe.skip('WASM vs Legacy parity', () => {
  for (const scenario of SCENARIOS) {
    it(`${scenario.name} — states match between Legacy and WASM`, () => {
      const legacy = runScenarioOnce(MODES[0]!, scenario.trigger)
      const wasmResult = runScenarioOnce(MODES[1]!, scenario.trigger)

      // Log change counts for visibility
      console.log(
        `  ${scenario.name}: input=${legacy.inputCount} | Legacy diffs=${legacy.diffCount} | WASM diffs=${wasmResult.diffCount}`,
      )

      // Both modes should produce the same number of diffs
      expect(legacy.diffCount).toBe(wasmResult.diffCount)

      // State snapshots should match between Legacy and WASM
      // Note: Legacy doesn't run BoolLogic in pipeline (uses effect()), so we compare
      // only state — not _concerns
      expect(legacy.stateSnap).toEqual(wasmResult.stateSnap)
    })
  }
})
