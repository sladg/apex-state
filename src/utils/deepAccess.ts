/**
 * Deep access utilities for safe nested object access
 *
 * Uses native Reflect for reads (~2x faster) and writes (~3x faster) vs lodash.
 * Keeps lodash isEqual where no native equivalent exists.
 * Maintains valtio reactivity when setting values.
 */

import isEqual from 'lodash/isEqual'

import type { DeepKey, DeepValue } from '../types'

/** Native deep get — replaces lodash _get for ~2x speedup */
const _get = (obj: object, path: string): unknown =>
  path
    .split('.')
    .reduce<unknown>(
      (acc, key) =>
        acc != null && typeof acc === 'object'
          ? Reflect.get(acc, key)
          : undefined,
      obj,
    )

/** Native deep set — replaces lodash _set for ~3x speedup */
const _set = (obj: object, path: string, value: unknown): void => {
  const keys = path.split('.')
  const last = keys.length - 1
  let current: object = obj
  for (let i = 0; i < last; i++) {
    let next = Reflect.get(current, keys[i]!)
    if (next == null || typeof next !== 'object') {
      next = {}
      Reflect.set(current, keys[i]!, next)
    }
    current = next as object
  }
  Reflect.set(current, keys[last]!, value)
}

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
 * Unsafely sets a value in a nested object using a runtime path string
 * Use when paths are determined at runtime and cannot be statically typed
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const dynamicPath = 'address.city'
 * deepSetUnsafe(user, dynamicPath, 'LA')
 * ```
 */
export const deepSetUnsafe = <T extends object>(
  obj: T,
  path: string,
  value: unknown,
): void => {
  _set(obj, path, value)
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

/**
 * Performs a deep equality comparison between two values
 *
 * Uses lodash isEqual for comprehensive deep comparison including:
 * - Nested objects and arrays
 * - Dates, RegExps, and other complex types
 * - Circular references
 *
 * @example
 * ```typescript
 * deepEqual({ a: 1 }, { a: 1 }) // true
 * deepEqual({ a: { b: 2 } }, { a: { b: 2 } }) // true
 * deepEqual([1, 2, 3], [1, 2, 3]) // true
 * deepEqual({ a: 1 }, { a: 2 }) // false
 * ```
 */
export const deepEqual = (a: unknown, b: unknown): boolean => {
  return isEqual(a, b)
}
