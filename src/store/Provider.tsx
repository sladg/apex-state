/**
 * Store Provider Component
 *
 * React component that initializes and provides the store to child components.
 * Supports derived value auto-detection via getter properties.
 */

import { useMemo } from 'react'
import { proxy } from 'valtio'
import { StoreContext } from './StoreContext'
import { createSideEffectsRegistry } from './sideEffectsRegistry'
import { createDefaultPipeline } from '../pipeline/synchronizers'
import type { ProviderProps, StoreInstance } from './types'
import type { GenericMeta } from '../types'

/**
 * Creates a Provider component for a specific data type
 */
export const createProvider = <DATA extends object, META extends GenericMeta = GenericMeta>() => {
  const Provider = ({ initialState, errorStorePath = '_errors', children }: ProviderProps<DATA>) => {
    const store = useMemo<StoreInstance<DATA, META>>(() => {
      // Create valtio proxy
      // Getters in the initialState object will work automatically with valtio proxy
      // as they are part of the object's property descriptors
      const state = proxy(initialState)

      // Initialize side effects registry
      const sideEffectsRegistry = createSideEffectsRegistry<DATA>()

      // Initialize default pipeline configuration
      const pipelineConfig = createDefaultPipeline<DATA, META>()

      return {
        state,
        config: {
          errorStorePath,
        },
        sideEffectsRegistry,
        pipelineConfig,
      }
      // Only initialize once - ignore changes to initialState after mount
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return <StoreContext.Provider value={store as any}>{children}</StoreContext.Provider>
  }

  Provider.displayName = 'StoreProvider'

  return Provider
}
