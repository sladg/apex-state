/**
 * TEST: Deep Nesting Sync Bug - REPRODUCES THE REPORTED BUG
 *
 * CONFIRMED BUG: Deeply nested sync paths don't sync correctly
 * even though shallower nested paths work fine.
 *
 * Evidence:
 * - checkout.shipping.selectedCountry ✅ SYNCS (1-2 levels deep)
 * - c.123.p.abc.data.productConfig.base.currency ❌ DOES NOT SYNC (7 levels deep)
 *
 * This test isolates the exact issue without parent re-renders.
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import { flushEffects, mountStore } from '../utils/react'

interface BugReproState {
  $shared: {
    selectedCountry: string
  }
  checkout: {
    shipping?: {
      selectedCountry?: string
    }
  }
  c?: {
    '123'?: {
      p?: {
        abc?: {
          data?: {
            productConfig?: {
              base?: {
                currency?: string
              }
            }
          }
        }
      }
    }
  }
}

describe('Deep Nesting Sync Bug - Reproduction', () => {
  /**
   * Test: Compare shallow vs deep nested path syncing
   *
   * Shallow (1-2 levels): checkout.shipping.selectedCountry ← → $shared.selectedCountry
   * Deep (7 levels): c.123.p.abc.data.productConfig.base.currency ← → $shared.selectedCountry
   *
   * Expected: Both sync to same value
   * Actual: Deep path stays undefined
   */
  it('should demonstrate shallow vs deep path sync discrepancy', async () => {
    const store = createGenericStore<BugReproState>({})

    const initialState: BugReproState = {
      $shared: {
        selectedCountry: 'US',
      },
      checkout: {},
      c: {
        '123': {
          p: {
            abc: {
              data: {
                productConfig: {
                  base: {},
                },
              },
            },
          },
        },
      },
    }

    const { storeInstance, setValue } = (mountStore as any)(
      store,
      initialState,
      {
        sideEffectsId: 'deep-nest-bug',
        sideEffects: {
          syncPaths: [
            // Shallow: 1-2 levels deep
            ['$shared.selectedCountry', 'checkout.shipping.selectedCountry'],
            // Deep: 7 levels deep
            [
              '$shared.selectedCountry',
              'c.123.p.abc.data.productConfig.base.currency',
            ],
          ],
        },
      },
    )

    await flushEffects()

    // ===  TEST: Change source and verify BOTH sync ===
    const testValue = 'DE'
    setValue('$shared.selectedCountry', testValue)
    await flushEffects()

    // Read back synced values
    const shallowSynced =
      storeInstance.state.checkout?.shipping?.selectedCountry
    const deepSynced =
      storeInstance.state.c?.['123']?.p?.abc?.data?.productConfig?.base
        ?.currency

    // Shallow should sync
    expect(shallowSynced).toBe(testValue)

    // Deep should sync too
    expect(deepSynced).toBe(testValue)
  })

  /**
   * Test: Multiple syncs to deeply nested paths fail
   *
   * When multiple sync pairs point to different deeply nested targets,
   * only some of them sync correctly.
   */
  it('should handle multiple deep path syncs (10 cycles)', async () => {
    const failures: { cycle: number; deepValue: string | undefined }[] = []

    for (let cycle = 1; cycle <= 10; cycle++) {
      const store = createGenericStore<BugReproState>({})
      const initialState: BugReproState = {
        $shared: { selectedCountry: 'US' },
        checkout: {},
        c: {
          '123': {
            p: {
              abc: {
                data: { productConfig: { base: {} } },
              },
            },
          },
        },
      }

      const { storeInstance, setValue } = (mountStore as any)(
        store,
        initialState,
        {
          sideEffectsId: `deep-multi-${cycle}`,
          sideEffects: {
            syncPaths: [
              [
                '$shared.selectedCountry',
                'c.123.p.abc.data.productConfig.base.currency',
              ],
            ],
          },
        },
      )

      await flushEffects()

      setValue('$shared.selectedCountry', 'GB')
      await flushEffects()

      const deepValue =
        storeInstance.state.c?.['123']?.p?.abc?.data?.productConfig?.base
          ?.currency

      if (deepValue !== 'GB') {
        failures.push({ cycle, deepValue })
      }
    }

    if (failures.length > 0) {
      console.error(`\n Deep path sync failed in ${failures.length} cycles:`)
      failures.forEach((f) => {
        console.error(
          `   Cycle ${f.cycle}: expected "GB", got "${f.deepValue ?? 'undefined'}"`,
        )
      })
    }

    expect(failures).toEqual([])
  })

  /**
   * Test: Nesting depth sensitivity
   *
   * Try syncing to different nesting depths and see where it breaks
   */
  it('should work at shallow depths but fail at deeper nesting', async () => {
    const store = createGenericStore<BugReproState>({})

    const initialState: BugReproState = {
      $shared: { selectedCountry: 'US' },
      checkout: { shipping: {} },
      c: {
        '123': {
          p: {
            abc: {
              data: { productConfig: { base: {} } },
            },
          },
        },
      },
    }

    // Test different nesting depths (non-conflicting sync pairs)
    const { storeInstance, setValue } = (mountStore as any)(
      store,
      initialState,
      {
        sideEffectsId: 'depth-test',
        sideEffects: {
          syncPaths: [
            // Depth 2: object-level sync
            ['$shared', 'checkout.shipping'],
            // Depth 7: leaf-level sync (no conflict with above)
            [
              '$shared.selectedCountry',
              'c.123.p.abc.data.productConfig.base.currency',
            ],
          ],
        },
      },
    )

    await flushEffects()

    const sourceValue = { selectedCountry: 'US' }
    setValue('$shared', sourceValue)
    await flushEffects()

    // Check what got synced at each depth
    const depth2 = storeInstance.state.checkout?.shipping
    const depth7 =
      storeInstance.state.c?.['123']?.p?.abc?.data?.productConfig?.base
        ?.currency

    // Depth 2 should sync the object
    expect(depth2).toEqual(sourceValue)

    // Depth 7 should sync the leaf value
    expect(depth7).toBe('US')
  })
})
