/**
 * Listeners Synchronizer
 *
 * Invokes registered listeners when state changes occur.
 * Uses smart breakdown to efficiently trigger only relevant listeners.
 *
 * Key features:
 * - Global listeners receive all changes
 * - Scoped listeners receive filtered changes
 * - Smart breakdown only when nested listeners exist
 * - Error handling to prevent pipeline breakage
 */

import type { Synchronizer } from '../types'
import type { GenericMeta, DeepKey } from '../../types'
import type { ListenersRegistry } from '../../sideEffects/listeners/registry'
import { breakdownChanges } from '../../sideEffects/listeners/breakdown'
import { snapshot } from 'valtio'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Creates a synchronizer for listeners side-effect
 *
 * Breaks down changes smartly and invokes relevant listeners.
 * Does NOT add new changes - only triggers listeners.
 *
 * @param registry - Listener registry with registered listeners
 * @returns Synchronizer function that invokes listeners
 *
 * @example
 * ```typescript
 * const registry = createListenersRegistry<AppState, Meta>()
 * registry.register('global', null, (changes, state) => {
 *   console.log('All changes:', changes)
 * })
 * registry.register('user-listener', 'user', (changes, user) => {
 *   console.log('User changed:', user)
 * })
 *
 * const synchronizer = createListenersSynchronizer(registry)
 * // Use in pipeline...
 * ```
 */
export const createListenersSynchronizer = <
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(
  registry: ListenersRegistry<DATA, META>
): Synchronizer<DATA, META> => {
  return (changes, state) => {
    // Break down changes for listener triggering
    // This creates virtual nested changes based on registered listeners
    const allChanges = breakdownChanges(changes, registry, state)

    // Get current state snapshot for listeners
    // Listeners receive immutable snapshot, not proxy
    const currentState = snapshot(state) as DATA

    // Invoke global listeners
    // Global listeners receive ALL changes and full state
    const globalListeners = registry.getGlobalListeners()
    for (const listener of globalListeners) {
      try {
        listener.fn(allChanges, currentState)
      } catch (error) {
        console.error('[APEX Listeners] Error in global listener:', error)
        // Don't break pipeline execution - catch and log
      }
    }

    // Invoke scoped listeners
    // Track processed paths to avoid duplicate invocations
    const processedPaths = new Set<string>()

    for (const [path] of allChanges) {
      const pathKey = path as string

      // Check all parent paths and the path itself for listeners
      const pathParts = pathKey.split('.')
      const pathsToCheck: string[] = []

      // Build list of paths to check (path itself + all parent paths)
      for (let i = 1; i <= pathParts.length; i++) {
        pathsToCheck.push(pathParts.slice(0, i).join('.'))
      }

      // Process each path that has listeners
      for (const checkPath of pathsToCheck) {
        if (processedPaths.has(checkPath)) continue

        const listeners = registry.getListeners(checkPath as DeepKey<DATA>)
        if (listeners.length === 0) continue

        processedPaths.add(checkPath)

        // Filter changes relevant to this listener's path
        // Include the path itself and any nested paths
        const relevantChanges = allChanges.filter(([p]) => {
          const pStr = p as string
          return pStr === checkPath || pStr.startsWith(checkPath + '.')
        })

        // Get scoped state for this path
        let scopedState: any
        try {
          scopedState = deepGet(currentState, checkPath as DeepKey<DATA>)
        } catch (error) {
          console.error(
            `[APEX Listeners] Error getting state for path "${checkPath}":`,
            error
          )
          continue
        }

        // Invoke all listeners for this path
        for (const listener of listeners) {
          try {
            listener.fn(relevantChanges as any, scopedState)
          } catch (error) {
            console.error(
              `[APEX Listeners] Error in listener for path "${checkPath}":`,
              error
            )
            // Don't break pipeline execution
          }
        }
      }
    }

    // Listeners don't add changes - return original
    return changes
  }
}