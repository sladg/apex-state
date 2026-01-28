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
 * Safely gets a value from a nested object using dot notation (type-safe)
 *
 * Use this when the path is known at compile time for full type safety.
 * For runtime/dynamic paths, use `deepGetUnsafe` instead.
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const city = deepGet(user, 'address.city') // string | undefined
 * ```
 */
export const deepGet = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P,
): DeepValue<T, P> | undefined => {
  return _get(obj, path as string) as DeepValue<T, P> | undefined
}

/**
 * Gets a value from a nested object using a runtime string path (unsafe)
 *
 * Use this when the path is determined at runtime and cannot be validated
 * at compile time. Returns `unknown` since the type cannot be inferred.
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const dynamicPath = 'address.city'
 * const value = deepGetUnsafe(user, dynamicPath) // unknown
 * ```
 */
export const deepGetUnsafe = <T extends object>(
  obj: T,
  path: string,
): unknown => {
  return _get(obj, path)
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
  value: DeepValue<T, P>,
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
  path: P,
): boolean => {
  return _get(obj, path as string) !== undefined
}
