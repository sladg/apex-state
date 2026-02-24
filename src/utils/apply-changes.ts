/**
 * Apply Changes Utility
 *
 * Applies an array of changes to an object, returning a new object.
 */

import type { ArrayOfChanges } from '../types'
import { deepClone } from './deep-clone'
import { dot } from './dot'

/**
 * Applies changes to an object, returning a new object with changes applied.
 * Does not mutate the original object.
 *
 * @param obj - Source object
 * @param changes - Array of [path, value, meta] tuples
 * @returns New object with changes applied
 *
 * @example
 * ```typescript
 * const state = { user: { name: 'Alice', age: 30 } }
 * const changes: ArrayOfChanges<typeof state> = [
 *   ['user.name', 'Bob', {}],
 *   ['user.age', 31, {}],
 * ]
 *
 * const newState = applyChangesToObject(state, changes)
 * // newState: { user: { name: 'Bob', age: 31 } }
 * // state is unchanged
 * ```
 */
export const applyChangesToObject = <T extends object>(
  obj: T,
  changes: ArrayOfChanges<T>,
): T => {
  // Deep clone the object — deepClone handles Proxy objects (structuredClone does not)
  const result = deepClone(obj)

  // Apply each change
  for (const [path, value] of changes) {
    dot.set__unsafe(result, path as string, value)
  }

  return result
}
