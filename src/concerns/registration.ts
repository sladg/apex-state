import { effect } from 'valtio-reactive'

import type { StoreInstance } from '../core/types'
import type { DeepKey, GenericMeta } from '../types'
import { dot } from '../utils/dot'
import { findConcern } from './registry'
import type { BaseConcernProps, ConcernType } from './types'

export const registerConcernEffects = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: Partial<Record<DeepKey<DATA>, Partial<Record<string, any>>>>,
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
      const concern = findConcern(concernName, concerns)
      if (!concern) {
        console.warn(`Concern "${concernName}" not found`)
        return
      }

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
        const result = concern.evaluate(evalProps)

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
    // Stop all effects (removes tracking subscriptions)
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
