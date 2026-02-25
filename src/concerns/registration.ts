import { effect } from 'valtio-reactive'

import type { StoreInstance } from '../core/types'
import type { ConcernRegistrationMap, DeepKey, GenericMeta } from '../types'
import { evaluateBoolLogic } from '../utils/bool-logic'
import { dot } from '../utils/dot'
import { findConcern } from './registry'
import type { BaseConcernProps, ConcernType } from './types'

/** Legacy JS implementation - uses effect() for all concerns */
const registerConcernEffectsImpl = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ConcernRegistrationMap<DATA>,
  concerns: readonly ConcernType[],
): (() => void) => {
  const disposeCallbacks: (() => void)[] = []
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
      let concern = findConcern(concernName, concerns)
      if (!concern) {
        // Support custom/ad-hoc concerns that provide an inline evaluate function
        if ('evaluate' in config && typeof config.evaluate === 'function') {
          concern = {
            name: concernName,
            description: `Custom concern: ${concernName}`,
            evaluate: config.evaluate,
          }
        } else if ('boolLogic' in config && config.boolLogic) {
          // Support ad-hoc BoolLogic concerns with a boolLogic but no prebuilt match
          concern = {
            name: concernName,
            description: `Ad-hoc BoolLogic concern: ${concernName}`,
            evaluate: (props: any) =>
              evaluateBoolLogic(props.boolLogic, props.state),
          }
        } else {
          console.warn(`Concern "${concernName}" not found`)
          return
        }
      }

      // --- Pure JS/effect-based path for ALL concerns ---
      const cacheKey = `${path}.${concernName}`

      // Wrap evaluation in effect() for automatic dependency tracking
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
        // If config provides custom evaluate(), use that instead of prebuilt's evaluate
        // Wrapped with timing measurement when debug.timing is enabled
        // @FIXME: this should be coming from cocnern registration. we should have evaluate function for validation there, not here.
        const evaluateFn =
          'evaluate' in config && typeof config.evaluate === 'function'
            ? config.evaluate
            : concern.evaluate

        const result = store._internal.timing.run(
          'concerns',
          () => evaluateFn(evalProps),
          { path, name: concernName },
        )

        store._internal.observer.concernEval(path, concernName, value, result)

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
    })
  })

  // Return cleanup function that disposes all effects on unmount
  return () => {
    // Stop all effects and unregister WASM BoolLogic
    disposeCallbacks.forEach((dispose) => dispose())

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

/** Legacy implementation - uses effect() for all concerns */
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
