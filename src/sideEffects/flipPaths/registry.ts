/**
 * FlipPathsRegistry for managing flip path registrations
 *
 * Maintains bidirectional mapping: when path1 changes, path2 gets opposite value.
 * Uses factory function pattern (NOT classes) to align with project architecture.
 */

import type { DeepKey } from '../../types'

/**
 * Registry state for flip path management
 */
interface FlipPathsRegistryState<DATA extends object> {
  // Bidirectional mapping: path → flipped path
  flips: Map<string, string>
  // ID-based tracking for unregistration: id → [path1, path2]
  flipIds: Map<string, [string, string]>
}

/**
 * Registry for flip path registrations
 */
export interface FlipPathsRegistry<DATA extends object> {
  /**
   * Register a flip relationship between two paths
   * Both directions are registered: path1 ↔ path2
   */
  register(id: string, path1: DeepKey<DATA>, path2: DeepKey<DATA>): void

  /**
   * Unregister a flip relationship by ID
   */
  unregister(id: string): void

  /**
   * Get the flipped path for a given path
   * @returns The opposite path, or undefined if no flip exists
   */
  getFlippedPath(path: DeepKey<DATA>): DeepKey<DATA> | undefined

  /**
   * Check if a path has a flip pair registered
   */
  hasFlip(path: DeepKey<DATA>): boolean
}

/**
 * Create a new FlipPathsRegistry instance
 *
 * Factory function pattern (NOT class) following project architecture.
 */
export function createFlipPathsRegistry<
  DATA extends object
>(): FlipPathsRegistry<DATA> {
  const state: FlipPathsRegistryState<DATA> = {
    flips: new Map<string, string>(),
    flipIds: new Map<string, [string, string]>(),
  }

  return {
    register(
      id: string,
      path1: DeepKey<DATA>,
      path2: DeepKey<DATA>
    ): void {
      const p1 = path1 as string
      const p2 = path2 as string

      // If ID already exists, unregister old mapping first
      const existingPaths = state.flipIds.get(id)
      if (existingPaths) {
        const [oldPath1, oldPath2] = existingPaths
        state.flips.delete(oldPath1)
        state.flips.delete(oldPath2)
      }

      // Bidirectional mapping
      state.flips.set(p1, p2)
      state.flips.set(p2, p1)

      // Track by ID for unregistration
      state.flipIds.set(id, [p1, p2])
    },

    unregister(id: string): void {
      const paths = state.flipIds.get(id)
      if (!paths) return

      const [path1, path2] = paths
      state.flips.delete(path1)
      state.flips.delete(path2)
      state.flipIds.delete(id)
    },

    getFlippedPath(path: DeepKey<DATA>): DeepKey<DATA> | undefined {
      const flipped = state.flips.get(path as string)
      return flipped as DeepKey<DATA> | undefined
    },

    hasFlip(path: DeepKey<DATA>): boolean {
      return state.flips.has(path as string)
    },
  }
}
