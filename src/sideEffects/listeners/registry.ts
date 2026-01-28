/**
 * Listeners Registry
 *
 * Manages listener registration and provides fast path-based lookup.
 * Supports both global listeners (key=null) and scoped listeners (key=path).
 *
 * Uses factory pattern instead of classes for functional style.
 */

import type { DeepKey, GenericMeta } from '../../types'
import type { OnStateChangesListenerFunction, RegisteredListener } from './types'

export interface ListenersRegistry<
  DATA extends object,
  META extends GenericMeta
> {
  register<K extends DeepKey<DATA> | null>(
    id: string,
    key: K,
    fn: OnStateChangesListenerFunction<DATA, META, K>['fn']
  ): void

  unregister(id: string): void

  getListeners(key: DeepKey<DATA>): OnStateChangesListenerFunction<DATA, META>[]

  getGlobalListeners(): OnStateChangesListenerFunction<DATA, META, null>[]

  hasListenerForPath(path: string): boolean

  hasListenerForNestedPath(basePath: string): boolean
}

/**
 * Creates a new listeners registry
 *
 * Factory function for creating listener registries with path indexing.
 * Provides O(1) lookup for path-based listener queries.
 *
 * @example
 * ```typescript
 * const registry = createListenersRegistry<AppState, Meta>()
 *
 * // Register global listener
 * registry.register('global-1', null, (changes, state) => {
 *   console.log('All changes:', changes)
 * })
 *
 * // Register scoped listener
 * registry.register('user-listener', 'user.name', (changes, name) => {
 *   console.log('Name changed to:', name)
 * })
 *
 * // Query listeners
 * const userListeners = registry.getListeners('user.name')
 * const hasNestedListeners = registry.hasListenerForNestedPath('user')
 * ```
 */
export const createListenersRegistry = <
  DATA extends object,
  META extends GenericMeta
>(): ListenersRegistry<DATA, META> => {
  // Map: path → array of listeners for that path
  const listeners = new Map<string, RegisteredListener<DATA, META>[]>()

  // Array of global listeners (key = null)
  const globalListeners: RegisteredListener<DATA, META>[] = []

  // Map: listener id → { key, index } for fast unregister
  const listenerIds = new Map<
    string,
    { key: string | null; index: number }
  >()

  // Set of paths that have listeners (for fast lookup)
  const listenerPathsIndex = new Set<string>()

  return {
    register<K extends DeepKey<DATA> | null>(
      id: string,
      key: K,
      fn: OnStateChangesListenerFunction<DATA, META, K>['fn']
    ): void {
      const listener = {
        key,
        fn,
      } as OnStateChangesListenerFunction<DATA, META>

      if (key === null) {
        // Global listener
        const index = globalListeners.length
        globalListeners.push({ id, listener })
        listenerIds.set(id, { key: null, index })
      } else {
        // Scoped listener
        const pathKey = key as string
        if (!listeners.has(pathKey)) {
          listeners.set(pathKey, [])
        }
        const pathListeners = listeners.get(pathKey)!
        const index = pathListeners.length
        pathListeners.push({ id, listener })
        listenerPathsIndex.add(pathKey)
        listenerIds.set(id, { key: pathKey, index })
      }
    },

    unregister(id: string): void {
      const info = listenerIds.get(id)
      if (!info) return

      if (info.key === null) {
        // Remove from global listeners
        const idx = globalListeners.findIndex((l) => l.id === id)
        if (idx !== -1) {
          globalListeners.splice(idx, 1)
        }
      } else {
        // Remove from scoped listeners
        const pathListeners = listeners.get(info.key)
        if (pathListeners) {
          const idx = pathListeners.findIndex((l) => l.id === id)
          if (idx !== -1) {
            pathListeners.splice(idx, 1)
          }

          // Remove path from index if no more listeners
          if (pathListeners.length === 0) {
            listeners.delete(info.key)
            listenerPathsIndex.delete(info.key)
          }
        }
      }

      listenerIds.delete(id)
    },

    getListeners(key: DeepKey<DATA>): OnStateChangesListenerFunction<DATA, META>[] {
      const pathListeners = listeners.get(key as string) || []
      return pathListeners.map((l) => l.listener)
    },

    getGlobalListeners(): OnStateChangesListenerFunction<DATA, META, null>[] {
      return globalListeners.map((l) => l.listener as OnStateChangesListenerFunction<DATA, META, null>)
    },

    hasListenerForPath(path: string): boolean {
      return listenerPathsIndex.has(path)
    },

    hasListenerForNestedPath(basePath: string): boolean {
      // Check if any listener path starts with basePath + '.'
      for (const path of listenerPathsIndex) {
        if (path.startsWith(basePath + '.')) {
          return true
        }
      }
      return false
    },
  }
}
