/**
 * Side Effects Registry
 *
 * Manages registration and unregistration of side effects by ID.
 * Used by the useSideEffects hook to track active side effect subscriptions.
 */

import type { SideEffects } from '../types/sideEffects'

/**
 * Registry for managing side effects
 */
export interface SideEffectsRegistry<DATA extends object> {
  /**
   * Register side effects with a unique ID
   */
  register: (id: string, effects: SideEffects<DATA>) => void

  /**
   * Unregister side effects by ID
   */
  unregister: (id: string) => void

  /**
   * Get side effects by ID
   */
  get: (id: string) => SideEffects<DATA> | undefined

  /**
   * Get all registered effect IDs
   */
  getIds: () => string[]

  /**
   * Check if effects are registered for an ID
   */
  has: (id: string) => boolean

  /**
   * Clear all registered effects
   */
  clear: () => void
}

/**
 * Create a new side effects registry instance
 *
 * Uses closure to encapsulate state (functional programming pattern)
 */
export const createSideEffectsRegistry = <DATA extends object>(): SideEffectsRegistry<DATA> => {
  const effects = new Map<string, SideEffects<DATA>>()

  return {
    register: (id: string, sideEffects: SideEffects<DATA>) => {
      if (effects.has(id)) {
        console.warn(`Side effects with ID "${id}" are already registered. Overwriting.`)
      }
      effects.set(id, sideEffects)
    },

    unregister: (id: string) => {
      effects.delete(id)
    },

    get: (id: string) => {
      return effects.get(id)
    },

    getIds: () => {
      return Array.from(effects.keys())
    },

    has: (id: string) => {
      return effects.has(id)
    },

    clear: () => {
      effects.clear()
    },
  }
}
