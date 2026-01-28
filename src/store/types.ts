/**
 * Store-specific types
 *
 * Type definitions for the store configuration, provider props,
 * and internal store instance structure.
 */

import type { ReactNode } from 'react'
import type { GenericMeta } from '../types'
import type { SideEffectsRegistry } from './sideEffectsRegistry'
import type { PipelineConfig } from '../pipeline/types'

/**
 * Configuration options for store creation
 */
export interface StoreConfig {
  /**
   * Path in the state object where errors will be stored
   * @default "_errors"
   */
  errorStorePath?: string
}

/**
 * Props for the Provider component
 */
export interface ProviderProps<DATA extends object> {
  /**
   * Initial state data
   */
  initialState: DATA

  /**
   * Optional path for error storage
   * @default "_errors"
   */
  errorStorePath?: string

  /**
   * Child components to wrap
   */
  children: ReactNode
}

/**
 * Internal store instance structure
 * Contains the valtio proxy state and configuration
 */
export interface StoreInstance<DATA extends object, META extends GenericMeta = GenericMeta> {
  /**
   * The valtio proxy state
   */
  state: DATA

  /**
   * Store configuration
   */
  config: Required<StoreConfig>

  /**
   * Side effects registry for managing effect subscriptions
   */
  sideEffectsRegistry: SideEffectsRegistry<DATA>

  /**
   * Pipeline configuration for change processing
   */
  pipelineConfig: PipelineConfig<DATA, META>
}
