/**
 * Flip Paths Synchronizer
 *
 * For each change, if a flipped path exists, adds a change with opposite value.
 * Handles booleans (true ↔ false) and two-value enums (swap values).
 * Skips changes that are already flip changes to avoid infinite loops.
 *
 * CRITICAL: Uses isFlipPathChange meta flag for loop prevention.
 */

import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { FlipPathsRegistry } from '../../sideEffects/flipPaths/registry'
import { deepGet } from '../../store/utils/deepAccess'

/**
 * Create a synchronizer for flip paths side-effect.
 *
 * @param registry - The FlipPathsRegistry instance managing flip relationships
 * @returns A synchronizer function that propagates opposite values across flipped paths
 */
export const createFlipPathsSynchronizer = <
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(registry: FlipPathsRegistry<DATA>): Synchronizer<DATA, META> => {
  return (changes, state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []

    for (const [path, value, meta] of changes) {
      // CRITICAL: Skip if this is already a flip change (avoid infinite loops)
      if (meta.isFlipPathChange) {
        continue
      }

      // Check if this path has a flip pair
      const flippedPath = registry.getFlippedPath(path)
      if (!flippedPath) {
        continue
      }

      // Get the OLD value of the path being changed (for enum swap)
      const oldValue = deepGet(state, path)

      // Calculate flipped value
      const flippedValue = flipValue(value, oldValue)

      // Add flip change with meta flag
      newChanges.push([
        flippedPath,
        flippedValue,
        { ...meta, isFlipPathChange: true } as META,
      ])
    }

    return [...changes, ...newChanges]
  }
}

/**
 * Flip a value to its opposite.
 *
 * - Boolean: true ↔ false
 * - Enum/other: swap with old value (maintains two-value enum behavior)
 *
 * @param newValue - The new value being set
 * @param oldValue - The old value of the path being changed
 * @returns The flipped value
 */
function flipValue(newValue: any, oldValue: any): any {
  // Boolean flip: simple negation
  if (typeof newValue === 'boolean') {
    return !newValue
  }

  // For enums and other types: use the old value of the changing path
  // This creates a "swap" behavior for two-value enums
  // Example: a='light', b='dark' → set a='dark' → b becomes 'light' (a's old value)
  return oldValue
}
