/**
 * Deep access utilities for safe nested object access
 *
 * Wraps lodash get/set with type-safe path access using DeepKey/DeepValue.
 * Maintains valtio reactivity when setting values.
 */

import _get from 'lodash/get'
import _set from 'lodash/set'
import type { DeepKey, DeepValue } from '../../types'

/**
 * Safely gets a value from a nested object using dot notation
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const city = deepGet(user, 'address.city') // 'NYC'
 * const missing = deepGet(user, 'address.zip') // undefined
 * ```
 */
export const deepGet = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P
): DeepValue<T, P> | undefined => {
  return _get(obj, path as string) as DeepValue<T, P> | undefined
}

/**
 * Safely sets a value in a nested object using dot notation
 * Maintains valtio proxy reactivity by mutating the original object
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * deepSet(user, 'address.city', 'LA')
 * // user.address.city is now 'LA'
 * ```
 */
export const deepSet = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P,
  value: DeepValue<T, P>
): void => {
  // lodash set mutates the object, which is what we want for valtio reactivity
  _set(obj, path as string, value)
}

/**
 * Checks if a deep path exists in an object
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * deepHas(user, 'address.city') // true
 * deepHas(user, 'address.zip') // false
 * ```
 */
export const deepHas = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P
): boolean => {
  return _get(obj, path as string) !== undefined
}
