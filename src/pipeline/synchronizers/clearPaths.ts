/**
 * Clear Paths Synchronizer
 *
 * Implements clear path side-effects. When trigger paths change,
 * configured target paths are set to undefined.
 */

import type { Synchronizer } from '../types'
import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ClearPathsRegistry } from '../../sideEffects/clearPaths/registry'

/**
 * Create a synchronizer for clear paths side-effect.
 *
 * When trigger paths change, clear specified paths (set to undefined).
 * Optionally trigger on nested changes too via clearOnNested flag.
 *
 * @param registry - The ClearPathsRegistry instance managing clear rules
 * @returns A synchronizer function that clears paths when triggers fire
 */
export function createClearPathsSynchronizer<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(registry: ClearPathsRegistry<DATA>): Synchronizer<DATA, META> {
  return (changes, _state) => {
    const newChanges: ArrayOfChanges<DATA, META> = []
    const clearedPaths = new Set<string>() // Avoid duplicate clears

    for (const [path, _value, meta] of changes) {
      // Find clear rules triggered by this change
      const rules = registry.getClearRulesTriggeredBy(path)

      for (const rule of rules) {
        // Clear all target paths
        for (const clearPath of rule.clearPaths) {
          const clearPathStr = clearPath as string
          if (clearedPaths.has(clearPathStr)) continue
          clearedPaths.add(clearPathStr)

          newChanges.push([
            clearPath,
            undefined as any,
            { ...meta, isProgramaticChange: true } as META,
          ])
        }
      }
    }

    return [...changes, ...newChanges]
  }
}
