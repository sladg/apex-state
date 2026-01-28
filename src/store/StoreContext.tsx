/**
 * Store React Context
 *
 * Provides the store instance to React components via Context API.
 */

import { createContext } from 'react'

import type { GenericMeta } from '../types'
import type { StoreInstance } from './types'

/**
 * React Context for the store instance
 * Null by default - will be populated by Provider
 */
export const StoreContext = createContext<StoreInstance<
  any,
  GenericMeta
> | null>(null)

/**
 * Display name for debugging
 */
StoreContext.displayName = 'StoreContext'
