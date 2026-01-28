/**
 * Core store factory
 *
 * Creates a store instance with valtio proxy, derived values support,
 * and React Provider component.
 */

import type { FC } from 'react'
import type { GenericMeta, DeepKey, DeepValue, ArrayOfChanges } from '../types'
import type { StoreConfig, ProviderProps } from './types'
import type { SideEffects } from '../types/sideEffects'
import { createProvider } from './Provider'
import { useStore } from '../hooks/useStore'
import { useJitStore, type JitStoreReturn } from '../hooks/useJitStore'
import { useSideEffects } from '../hooks/useSideEffects'

/**
 * Return type of createGenericStore
 * Contains the Provider component and hooks for accessing the store
 */
export interface StoreReturn<DATA extends object, META extends GenericMeta> {
  /**
   * React Provider component for wrapping the app or subtree
   */
  Provider: FC<ProviderProps<DATA>>

  /**
   * useState-like hook for accessing and updating specific paths
   */
  useStore: <P extends DeepKey<DATA>>(
    path: P
  ) => [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void]

  /**
   * Just-In-Time hook for bulk operations and non-reactive reads
   */
  useJitStore: () => JitStoreReturn<DATA, META>

  /**
   * Hook for registering side effects
   */
  useSideEffects: (id: string, effects: SideEffects<DATA>) => void
}

/**
 * Creates a generic store with valtio proxy and React context
 *
 * This is a factory function that returns a configured store with:
 * - Valtio proxy for reactive state
 * - Automatic derived value detection from getter properties
 * - React Provider component for context
 * - Deep get/set utilities (via exported utils)
 *
 * @example
 * ```typescript
 * type AppState = {
 *   user: { name: string }
 *   count: number
 *   get doubled() { return this.count * 2 }
 * }
 *
 * const store = createGenericStore<AppState>()
 *
 * function App() {
 *   return (
 *     <store.Provider initialState={{ user: { name: 'Alice' }, count: 0 }}>
 *       <YourApp />
 *     </store.Provider>
 *   )
 * }
 * ```
 *
 * @param config - Optional configuration (errorStorePath, etc.)
 * @returns Store object with Provider component
 */
export function createGenericStore<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(config?: StoreConfig): StoreReturn<DATA, META> {
  // Create the Provider component for this store
  const Provider = createProvider<DATA>()

  return {
    Provider,
    useStore: useStore as <P extends DeepKey<DATA>>(
      path: P
    ) => [DeepValue<DATA, P>, (value: DeepValue<DATA, P>, meta?: META) => void],
    useJitStore: useJitStore<DATA, META>,
    useSideEffects: useSideEffects<DATA>,
  }
}
