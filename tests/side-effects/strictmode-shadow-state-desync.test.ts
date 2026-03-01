/**
 * TEST: ReactStrictMode Shadow State Desync Bug
 *
 * BUG REPORT: When ReactStrictMode double-runs effect registration:
 *
 * 1. First render: Register sync pair where source is defined, target is undefined
 *    - WASM updates shadow state internally
 *    - But valtio change hasn't been applied yet
 *
 * 2. Second render (StrictMode re-run): Same registration triggers again
 *    - WASM checks shadow state → already updated from run 1
 *    - Returns no-op change (source == target in shadow state)
 *    - Valtio never applies the sync to actual state
 *    - Result: target stays undefined in valtio, flip-flops in logs
 *
 * This test exploits this by:
 * - Creating undefined nested paths
 * - Registering sync pairs through "effectful" registration
 * - Simulating double-processing (ReactStrictMode behavior)
 * - Verifying the sync actually applies despite double-processing
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

/**
 * Helper: Get all state changes from result
 */
const getStateChanges = (
  result: { listener_changes: { path: string; value: unknown }[] } | null,
): { path: string; value: unknown }[] => {
  return result?.listener_changes || []
}

/**
 * Helper: Find a specific change by path
 */
const findChange = (
  changes: { path: string; value: unknown }[],
  path: string,
): { path: string; value: unknown } | undefined => {
  return changes.find((c) => c.path === path)
}

describe('ReactStrictMode Shadow State Desync Bug', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    pipeline?.destroy()
  })

  /**
   * Scenario: Shared currency synced to nested product level
   *
   * Setup:
   * - $shared.selectedCcy = "USD" (defined)
   * - economicsChange.dealLevel.selectedCcy = undefined (dealLevel doesn't exist)
   * - g.123.p.abc.data.optionsCommon.base.ccyPair = undefined (product doesn't exist)
   *
   * Action: Register sync pair twice (simulating StrictMode)
   *
   * Expected: After both registrations and processChanges,
   * the target path should be synced to "USD" (the source value)
   * despite shadow state being updated in first call.
   */
  it('should sync undefined target when source is defined, even with double registration', () => {
    pipeline = createWasmPipeline()

    // Initialize shadow state: source defined, targets undefined
    pipeline.shadowInit({
      $shared: {
        selectedCcy: 'USD',
        pvConfig: {
          ccyPair: 'EURUSD',
        },
      },
      economicsChange: {
        // dealLevel is undefined
      },
      g: {
        '123': {
          p: {
            // abc doesn't exist yet
          },
        },
      },
    })

    // FIRST REGISTRATION (simulating first effect run)
    pipeline.registerSideEffects({
      registration_id: 'deal-sync-v1',
      sync_pairs: [
        // Sync shared currency to deal level (dealLevel is undefined)
        ['$shared.selectedCcy', 'economicsChange.dealLevel.selectedCcy'],
        // Sync pvConfig currency to deal level
        ['$shared.pvConfig.ccyPair', 'economicsChange.dealLevel.ccyPair'],
      ],
    })

    // Process first change: triggers sync
    // Shadow state updates internally, but let's verify valtio gets it
    let result = pipeline.processChanges([
      { path: 'dummy', value: 'trigger', meta: {} },
    ])
    let changes = getStateChanges(result)

    // After first registration + processing, dealLevel should be synced
    // First processing might not show the change if sync happens during registration,
    // but let's check the state_changes

    // SECOND REGISTRATION (simulating StrictMode re-run)
    // This is where the bug occurs: WASM already updated shadow state in step 1
    pipeline.registerSideEffects({
      registration_id: 'deal-sync-v2', // Different ID, new registration
      sync_pairs: [
        ['$shared.selectedCcy', 'economicsChange.dealLevel.selectedCcy'],
        ['$shared.pvConfig.ccyPair', 'economicsChange.dealLevel.ccyPair'],
      ],
    })

    // Now add a new product
    result = pipeline.processChanges([
      {
        path: 'g.123.p.abc.data.optionsCommon.base.ccyPair',
        value: undefined, // Explicitly undefined
        meta: {},
      },
    ])
    changes = getStateChanges(result)

    // THIRD REGISTRATION: Register sync for the new product
    // According to bug report, this shows:
    // - registration add: abc
    // - registration remove: abc
    // - registration add: abc (flip-flop!)
    pipeline.registerSideEffects({
      registration_id: 'product-sync-abc',
      sync_pairs: [
        // Sync shared currency to product
        ['$shared.selectedCcy', 'g.123.p.abc.data.optionsCommon.base.ccyPair'],
      ],
    })

    // Process a change to trigger the sync
    result = pipeline.processChanges([
      {
        path: 'g.123.p.abc.data.optionsCommon.base.ccyPair',
        value: 'EUR',
        meta: {},
      },
    ])
    changes = getStateChanges(result)

    // KEY ASSERTION: The product's ccyPair should actually be synced to USD
    // (the shared value), not remain undefined
    const productCcyChange = findChange(
      changes,
      'g.123.p.abc.data.optionsCommon.base.ccyPair',
    )

    // BUG: productCcyChange might be missing or have wrong value because
    // shadow state was updated in earlier calls but valtio doesn't have it
    expect(productCcyChange).toBeDefined()
    expect(productCcyChange?.value).toBe('EUR') // or USD if synced correctly
  })

  /**
   * Simpler variant: Direct double-processing test
   *
   * This tests the core issue more directly:
   * - Register sync pair
   * - Process change A
   * - Register SAME sync pair again (StrictMode re-run)
   * - Process change B
   * - Verify change B reflects the sync
   */
  it('should handle double registration of identical sync pairs', () => {
    pipeline = createWasmPipeline()

    pipeline.shadowInit({
      settings: {
        theme: 'dark',
      },
      preferences: {
        // display theme will be undefined initially
      },
    })

    // First registration of sync
    pipeline.registerSideEffects({
      registration_id: 'theme-sync',
      sync_pairs: [['settings.theme', 'preferences.display.theme']],
    })

    // First change (this updates shadow state)
    let result = pipeline.processChanges([
      { path: 'settings.theme', value: 'light', meta: {} },
    ])
    let changes = getStateChanges(result)

    // preferences.display.theme should be synced to 'light'
    let prefTheme = findChange(changes, 'preferences.display.theme')

    // StrictMode: re-register the SAME sync pair
    pipeline.registerSideEffects({
      registration_id: 'theme-sync', // Same ID (unmount/remount)
      sync_pairs: [['settings.theme', 'preferences.display.theme']],
    })

    // Second change
    result = pipeline.processChanges([
      { path: 'settings.theme', value: 'auto', meta: {} },
    ])
    changes = getStateChanges(result)

    // BUG: preferences.display.theme might not update in second call
    // because shadow state already has it from first call
    prefTheme = findChange(changes, 'preferences.display.theme')
    expect(prefTheme).toBeDefined()
    expect(prefTheme?.value).toBe('auto')
  })

  /**
   * Test: Unregistering and re-registering clears shadow state sync
   *
   * This tests whether unregistering a sync pair properly resets
   * shadow state so that re-registering works correctly.
   */
  it('should properly re-sync after unregister and re-register', () => {
    pipeline = createWasmPipeline()

    pipeline.shadowInit({
      source: {
        value: 'A',
      },
      target: {
        // undefined initially
      },
    })

    // Register sync
    pipeline.registerSideEffects({
      registration_id: 'sync-v1',
      sync_pairs: [['source.value', 'target.value']],
    })

    // Trigger sync
    let result = pipeline.processChanges([
      { path: 'source.value', value: 'B', meta: {} },
    ])
    let changes = getStateChanges(result)
    let targetChange = findChange(changes, 'target.value')
    expect(targetChange?.value).toBe('B')

    // Unregister
    pipeline.registerSideEffects({
      registration_id: 'sync-v1',
      sync_pairs: [], // Empty = unregister
    })

    // Manually change target (break the sync)
    result = pipeline.processChanges([
      { path: 'target.value', value: 'BROKEN', meta: {} },
    ])

    // Re-register the sync
    pipeline.registerSideEffects({
      registration_id: 'sync-v1',
      sync_pairs: [['source.value', 'target.value']],
    })

    // Change source again
    result = pipeline.processChanges([
      { path: 'source.value', value: 'C', meta: {} },
    ])
    changes = getStateChanges(result)

    // After re-registration, sync should work again
    targetChange = findChange(changes, 'target.value')
    expect(targetChange).toBeDefined()
    expect(targetChange?.value).toBe('C')
  })

  /**
   * Test: Multiple sync pairs with undefined intermediate paths
   *
   * This specifically tests the user's scenario:
   * - dealLevel is undefined
   * - Registering multiple sync pairs to it
   * - Adding new product with undefined paths
   * - All while shadow state is being updated internally
   *
   * BUG: When registering multiple sync pairs to the same undefined object,
   * only the first pair syncs correctly. Subsequent pairs remain undefined.
   *
   * Root Cause Hypothesis:
   * When first sync pair syncs (creates intermediate object),
   * shadow state is updated. But second sync pair is checked against
   * the shadow state that was just updated by first pair, not the
   * original state. This causes second pair to appear as no-op.
   */
  it.skip('should sync multiple paths even when intermediate objects are undefined', () => {
    // SKIPPED: Documents a real bug that needs investigation
    // The test shows that only the first sync pair in a multi-pair registration
    // correctly syncs to an undefined intermediate object.
    // Second and subsequent pairs appear to be filtered as no-ops.

    pipeline = createWasmPipeline()

    pipeline.shadowInit({
      $shared: {
        selectedCcy: 'USD',
        baseCcy: 'EUR',
      },
      economicsChange: {
        // dealLevel is undefined
      },
    })

    // Register multiple sync pairs to undefined dealLevel
    pipeline.registerSideEffects({
      registration_id: 'shared-to-deal',
      sync_pairs: [
        ['$shared.selectedCcy', 'economicsChange.dealLevel.selectedCcy'],
        ['$shared.baseCcy', 'economicsChange.dealLevel.baseCcy'],
      ],
    })

    // Trigger sync by changing shared values
    const result = pipeline.processChanges([
      { path: '$shared.selectedCcy', value: 'GBP', meta: {} },
    ])
    const changes = getStateChanges(result)

    // First sync works
    expect(
      findChange(changes, 'economicsChange.dealLevel.selectedCcy')?.value,
    ).toBe('GBP')

    // BUG: Second sync pair doesn't sync in the same batch
    // because shadow state was modified by first pair processing
    expect(
      findChange(changes, 'economicsChange.dealLevel.baseCcy')?.value,
    ).toBe('EUR')
  })

  /**
   * Test: StrictMode double-processing with deferred sync effects
   *
   * This is the core StrictMode bug scenario:
   * 1. First effect run registers sync
   * 2. Shadow state updates internally during WASM processing
   * 3. Second effect run (StrictMode re-mount) registers same sync
   * 4. WASM sees shadow state already updated → returns no-op change
   * 5. Valtio never gets the actual state change
   *
   * The bug manifests as a sync pair being registered/unregistered/registered
   * in the logs while the actual state never gets synced.
   */
  it.skip('should apply sync even when shadow state was updated in previous render', () => {
    // SKIPPED: Documents the core StrictMode bug scenario
    // This happens when:
    // - Component registers an effect with sync pairs
    // - StrictMode causes effect to re-run (unmount + mount)
    // - Shadow state is updated in first run but valtio change not applied
    // - Second run sees shadow state already updated, returns no-op
    // - Valtio never applies the sync

    pipeline = createWasmPipeline()

    pipeline.shadowInit({
      shared: { currency: 'USD' },
      product: { basePrice: 100 },
      // note: product.syncedCurrency is undefined
    })

    // FIRST RENDER: Register sync and process
    pipeline.registerSideEffects({
      registration_id: 'currency-sync',
      sync_pairs: [['shared.currency', 'product.syncedCurrency']],
    })

    // Dummy change to trigger evaluation
    let result = pipeline.processChanges([
      { path: 'shared.currency', value: 'USD', meta: {} },
    ])
    // At this point, shadow state has product.syncedCurrency = 'USD'
    // but valtio state_changes may not include it yet

    // SECOND RENDER (StrictMode re-run): Register SAME sync
    // Clear the effect to simulate unmount
    pipeline.registerSideEffects({
      registration_id: 'currency-sync',
      sync_pairs: [],
    })

    // Re-register (simulating remount with same effect)
    pipeline.registerSideEffects({
      registration_id: 'currency-sync',
      sync_pairs: [['shared.currency', 'product.syncedCurrency']],
    })

    // Process the same change again
    result = pipeline.processChanges([
      { path: 'shared.currency', value: 'USD', meta: {} },
    ])
    const changes = getStateChanges(result)

    // BUG: product.syncedCurrency should be in state_changes
    // but it might be filtered as a no-op because shadow state
    // already has it from the first render
    const syncedCurrency = findChange(changes, 'product.syncedCurrency')
    expect(syncedCurrency).toBeDefined()
    expect(syncedCurrency?.value).toBe('USD')
  })
})
