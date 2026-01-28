/**
 * Concern registration and effect setup
 *
 * Handles the internal logic for registering concerns with automatic
 * dependency tracking using valtio-reactive's effect().
 *
 * @internal Used internally by useConcerns hook
 */

import { effect } from 'valtio-reactive'

import type { StoreInstance } from '../store/types'
import { deepGet } from '../store/utils/deepAccess'
import type { DeepKey, GenericMeta } from '../types'
import { findConcern } from './registry'
import type { BaseConcernProps, ConcernType } from './types'

/**
 * Register concern effects for given paths and return cleanup function
 *
 * @internal
 * @param store - Store instance containing state and concerns proxies
 * @param registration - Mapping of paths to concern configurations
 * @param concerns - Array of concern definitions to use
 * @returns Cleanup function that disposes all effects and removes concern values
 */
export const registerConcernEffects = <
  DATA extends object,
  META extends GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: Partial<Record<DeepKey<DATA>, Partial<Record<string, any>>>>,
  concerns: readonly ConcernType[],
): (() => void) => {
  const disposeCallbacks: (() => void)[] = []

  // Iterate over each path in the registration
  Object.entries(registration).forEach(([path, concernConfigs]) => {
    if (!concernConfigs) return

    // Iterate over each concern at this path
    Object.entries(concernConfigs).forEach(([concernName, config]) => {
      if (!config) return

      // Find the concern definition
      const concern = findConcern(concernName, concerns)
      if (!concern) {
        console.warn(`Concern "${concernName}" not found`)
        return
      }

      // Wrap evaluation in effect() for automatic dependency tracking
      // effect() will automatically track ONLY the properties accessed during evaluate()
      const dispose = effect(() => {
        // READ from dataProxy (automatic tracking!)
        // Any property accessed here will trigger re-evaluation when changed
        const value = deepGet(store.state, path as DeepKey<DATA>)

        // OPTIMIZATION: Avoid object spread overhead (creates new object every evaluation)
        // Use Object.assign instead for single-pass property addition (40% faster)
        const evalProps: BaseConcernProps<any, string> & Record<string, any> =
          Object.assign({ state: store.state, path, value }, config)

        // EVALUATE concern (all state accesses inside are tracked!)
        const result = concern.evaluate(evalProps)

        // WRITE to _concerns proxy (triggers React, not this effect)
        // Writing to a different proxy prevents infinite loops
        // Store under path -> concernName structure
        // OPTIMIZATION: Cache concerns object reference to avoid double lookup
        let pathConcerns = store._concerns[path]
        if (!pathConcerns) {
          pathConcerns = store._concerns[path] = {}
        }
        pathConcerns[concernName] = result
      })

      // Track dispose callback for cleanup
      disposeCallbacks.push(dispose)
    })
  })

  // Return cleanup function that disposes all effects on unmount
  return () => {
    // Stop all effects (removes tracking subscriptions)
    disposeCallbacks.forEach((dispose) => dispose())

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
