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
export class SideEffectsRegistry<DATA extends object> {
  private effects: Map<string, SideEffects<DATA>> = new Map()

  /**
   * Register side effects with a unique ID
   */
  register(id: string, effects: SideEffects<DATA>): void {
    if (this.effects.has(id)) {
      console.warn(`Side effects with ID "${id}" are already registered. Overwriting.`)
    }
    this.effects.set(id, effects)
  }

  /**
   * Unregister side effects by ID
   */
  unregister(id: string): void {
    this.effects.delete(id)
  }

  /**
   * Get side effects by ID
   */
  get(id: string): SideEffects<DATA> | undefined {
    return this.effects.get(id)
  }

  /**
   * Get all registered effect IDs
   */
  getIds(): string[] {
    return Array.from(this.effects.keys())
  }

  /**
   * Check if effects are registered for an ID
   */
  has(id: string): boolean {
    return this.effects.has(id)
  }

  /**
   * Clear all registered effects
   */
  clear(): void {
    this.effects.clear()
  }
}
