/**
 * useSideEffects Hook
 *
 * Registers side effects on mount and unregisters on unmount.
 * Uses useLayoutEffect for synchronous execution.
 *
 * @example
 * ```typescript
 * store.useSideEffects('user-sync', {
 *   // Side effect configuration (placeholder for now)
 * })
 * ```
 */

import { useLayoutEffect } from 'react'
import { useStoreContext } from './useStoreContext'
import type { SideEffects } from '../types/sideEffects'

/**
 * Hook for registering side effects
 *
 * @param id - Unique identifier for this set of side effects
 * @param effects - Side effects configuration
 */
export const useSideEffects = <DATA extends object>(
  id: string,
  effects: SideEffects<DATA>
): void => {
  const store = useStoreContext<DATA>()

  useLayoutEffect(() => {
    // Register side effects on mount
    store.sideEffectsRegistry.register(id, effects)

    // Cleanup: unregister side effects on unmount
    return () => {
      store.sideEffectsRegistry.unregister(id)
    }
    // Re-register if id or effects change
  }, [store, id, effects])
}
