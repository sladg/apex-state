/**
 * Apply Changes Utility
 *
 * Applies an array of changes to an object, returning a new object.
 */

import type { DeepKey, DeepValue } from '../types'
import { deepClone } from './deep-clone'
import { dot } from './dot'

/** A change tuple: [path, value] or [path, value, anyMeta]. Meta is ignored. */
type AnyChange<DATA> = {
  [K in DeepKey<DATA>]:
    | [K, DeepValue<DATA, K>]
    | [K, DeepValue<DATA, K>, ...unknown[]]
}[DeepKey<DATA>]

/**
 * Applies changes to an object, returning a new object with changes applied.
 * Does not mutate the original object. Meta fields on change tuples are ignored.
 *
 * @param obj - Source object
 * @param changes - Array of [path, value] or [path, value, meta] tuples
 * @returns New object with changes applied
 *
 * @example
 * ```typescript
 * const state = { user: { name: 'Alice', age: 30 } }
 * const newState = applyChangesToObject(state, [
 *   ['user.name', 'Bob'],
 *   ['user.age', 31],
 * ])
 * // newState: { user: { name: 'Bob', age: 31 } }
 * // state is unchanged
 * ```
 */
export const applyChangesToObject = <T extends object>(
  obj: T,
  changes: AnyChange<T>[],
): T => {
  // Deep clone the object — deepClone handles Proxy objects (structuredClone does not)
  const result = deepClone(obj)

  // Apply each change
  for (const [path, value] of changes) {
    dot.set__unsafe(result, path as string, value)
  }

  return result
}
