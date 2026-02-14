import { effect } from 'valtio-reactive'

import { findConcern } from '~/concerns/registry'
import type { BaseConcernProps, BoolLogic, ConcernType } from '~/concerns/types'
import type { StoreInstance } from '~/core/types'
import type { ConcernRegistrationMap, DeepKey, GenericMeta } from '~/types'
import { dot } from '~/utils/dot'
import {
  evaluateBoolLogicWasm,
  hasBoolLogicCondition,
  registerBoolLogic,
  unregisterBoolLogic,
} from '~/utils/wasmBridge'

const registerConcernEffectsImpl = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ConcernRegistrationMap<DATA>,
  concerns: readonly ConcernType[],
): (() => void) => {
  const disposeCallbacks: (() => void)[] = []
  const boolLogicIds: number[] = [] // Track WASM logic IDs for cleanup
  // Non-reactive cache for previous results (prevents tracked reads)
  const resultCache = new Map<string, unknown>()
  // Pre-allocate concern path objects and capture references
  const concernRefs = new Map<string, Record<string, unknown>>()

  // Pre-initialize all path objects BEFORE creating effects
  Object.keys(registration).forEach((path) => {
    if (!store._concerns[path]) {
      store._concerns[path] = {}
    }
    concernRefs.set(path, store._concerns[path])
  })

  // Iterate over each path in the registration
  Object.entries(registration).forEach(([path, concernConfigs]) => {
    if (!concernConfigs) return

    // Get pre-initialized concern object for this path
    const concernsAtPath = concernRefs.get(path)!

    // Iterate over each concern at this path
    Object.entries(concernConfigs).forEach(([concernName, config]) => {
      if (!config) return

      // Find the concern definition
      const concern = findConcern(concernName, concerns)
      if (!concern) {
        console.warn(`Concern "${concernName}" not found`)
        return
      }

      const cacheKey = `${path}.${concernName}`

      // SPLIT: BoolLogic concerns vs custom concerns
      if (hasBoolLogicCondition(config)) {
        // BoolLogic concern: Register with WASM instead of creating effect()
        const condition = (config as { condition: BoolLogic<any> }).condition
        const logicId = registerBoolLogic(path, concernName, condition)
        boolLogicIds.push(logicId)

        // Initial evaluation
        const initialResult = evaluateBoolLogicWasm(condition, store.state)
        concernsAtPath[concernName] = initialResult

        // Note: Re-evaluation on state changes happens in the change pipeline via WASM
      } else {
        // Custom concern: Wrap evaluation in effect() for automatic dependency tracking
        // effect() will automatically track ONLY the properties accessed during evaluate()
        const dispose = effect(() => {
          // READ from dataProxy (automatic tracking!)
          // Any property accessed here will trigger re-evaluation when changed
          const value = dot.get__unsafe(store.state, path)

          // OPTIMIZATION: Avoid object spread overhead (creates new object every evaluation)
          // Use Object.assign instead for single-pass property addition (40% faster)
          const evalProps: BaseConcernProps<any, string> & Record<string, any> =
            Object.assign({ state: store.state, path, value }, config)

          // EVALUATE concern (all state accesses inside are tracked!)
          // Wrapped with timing measurement when debug.timing is enabled
          const result = store._internal.timing.run(
            'concerns',
            () => concern.evaluate(evalProps),
            { path, name: concernName },
          )

          // Check cache (non-reactive!) to see if value changed
          const prev = resultCache.get(cacheKey)
          if (prev !== result) {
            // Update cache
            resultCache.set(cacheKey, result)

            // WRITE to pre-captured reference (NO tracked reads!)
            concernsAtPath[concernName] = result
          }
        })

        // Track dispose callback for cleanup
        disposeCallbacks.push(dispose)
      }
    })
  })

  // Return cleanup function that disposes all effects and WASM registrations on unmount
  return () => {
    // Stop all effects (removes tracking subscriptions)
    disposeCallbacks.forEach((dispose) => dispose())

    // Unregister all BoolLogic trees from WASM
    boolLogicIds.forEach((logicId) => unregisterBoolLogic(logicId))

    // Clear caches
    resultCache.clear()
    concernRefs.clear()

    // Remove concern values from concernsProxy
    Object.keys(registration).forEach((path) => {
      const concernConfigs = registration[path as DeepKey<DATA>]
      if (!concernConfigs) return

      // Delete specific concerns for this path
      Object.keys(concernConfigs).forEach((concernName) => {
        if (store._concerns[path]) {
          // Use Reflect.deleteProperty to avoid dynamic delete lint error
          Reflect.deleteProperty(store._concerns[path], concernName)
        }
      })

      // Clean up empty path object
      if (
        store._concerns[path] &&
        Object.keys(store._concerns[path]).length === 0
      ) {
        // Use Reflect.deleteProperty to avoid dynamic delete lint error
        Reflect.deleteProperty(store._concerns, path)
      }
    })
  }
}

export const registerConcernEffects = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ConcernRegistrationMap<DATA>,
  concerns: readonly ConcernType[],
): (() => void) =>
  store._internal.timing.run(
    'registration',
    () => registerConcernEffectsImpl(store, registration, concerns),
    { path: Object.keys(registration).join(','), name: 'concerns' },
  )
