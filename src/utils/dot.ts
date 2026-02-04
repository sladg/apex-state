/**
 * Deep access utilities for safe nested object access
 *
 * Uses native Reflect for reads (~2x faster) and writes (~3x faster) vs lodash.
 * Maintains valtio reactivity when setting values.
 */

import type { DeepKey, DeepValue } from '../types'
import { is } from './is'

// Cache split paths to avoid repeated string splitting overhead
const pathCache = new Map<string, string[]>()
const MAX_CACHE_SIZE = 1000

const getPathParts = (path: string): string[] => {
  let parts = pathCache.get(path)
  if (!parts) {
    parts = path.split('.')
    if (pathCache.size >= MAX_CACHE_SIZE) {
      pathCache.clear()
    }
    pathCache.set(path, parts)
  }
  return parts
}

/**
 * Safely gets a value from a nested object using dot notation (type-safe)
 *
 * Use this when the path is known at compile time for full type safety.
 * For runtime/dynamic paths, use `dot.get__unsafe` instead.
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const city = dot.get(user, 'address.city') // string | undefined
 * ```
 */
const get = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P,
): DeepValue<T, P> => {
  const parts = getPathParts(path as string)
  let current: any = obj
  for (const part of parts) {
    if (is.not.object(current)) {
      return undefined as DeepValue<T, P>
    }
    current = Reflect.get(current, part)
  }
  return current as DeepValue<T, P>
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
 * const value = dot.get__unsafe(user, dynamicPath) // unknown
 * ```
 */
const get__unsafe = (obj: unknown, path: string): unknown => {
  const parts = getPathParts(path)
  let current: any = obj
  for (const part of parts) {
    if (is.not.object(current)) {
      return undefined
    }
    current = Reflect.get(current, part)
  }
  return current
}

/**
 * Safely sets a value in a nested object using dot notation
 * Maintains valtio proxy reactivity by mutating the original object
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * dot.set(user, 'address.city', 'LA')
 * // user.address.city is now 'LA'
 * ```
 */
const set = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P,
  value: DeepValue<T, P>,
): void => {
  const keys = getPathParts(path as string)
  const last = keys.length - 1
  let current: object = obj
  for (let i = 0; i < last; i++) {
    let next = Reflect.get(current, keys[i]!)
    if (!is.object(next)) {
      next = {}
      Reflect.set(current, keys[i]!, next)
    }
    current = next as object
  }
  Reflect.set(current, keys[last]!, value)
}

/**
 * Unsafely sets a value in a nested object using a runtime path string
 * Use when paths are determined at runtime and cannot be statically typed
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * const dynamicPath = 'address.city'
 * dot.set__unsafe(user, dynamicPath, 'LA')
 * ```
 */
const set__unsafe = <T extends object>(
  obj: T,
  path: string,
  value: unknown,
): void => {
  const keys = getPathParts(path)
  const last = keys.length - 1
  let current: object = obj
  for (let i = 0; i < last; i++) {
    let next = Reflect.get(current, keys[i]!)
    if (!is.object(next)) {
      next = {}
      Reflect.set(current, keys[i]!, next)
    }
    current = next as object
  }
  Reflect.set(current, keys[last]!, value)
}

/**
 * Checks if a value exists at a nested path using dot notation (type-safe)
 *
 * Returns true if the path exists and is not undefined.
 * Note: null and other falsy values (except undefined) return true.
 *
 * @example
 * ```typescript
 * const user = { address: { city: 'NYC' } }
 * dot.has(user, 'address.city') // true
 * dot.has(user, 'address.zip') // false
 * ```
 */
const has = <T extends object, P extends DeepKey<T>>(
  obj: T,
  path: P,
): boolean => {
  const parts = getPathParts(path as string)
  let current: any = obj
  for (const part of parts) {
    if (is.not.object(current)) {
      return false
    }
    current = Reflect.get(current, part)
  }
  return is.not.undefined(current)
}

/**
 * Deep equality check for values at multiple paths
 *
 * Compares values at multiple runtime paths for deep equality.
 * Returns true if all values are deeply equal to each other.
 * Useful for aggregations and change detection.
 *
 * @example
 * ```typescript
 * const obj = { a: { b: 1 }, c: { b: 1 }, d: { b: 1 } }
 * dot.same(obj, 'a.b', 'c.b', 'd.b') // true
 * dot.same(obj, 'a.b', 'c.b') // true
 * ```
 */
const same = <T extends object>(obj: T, ...paths: string[]): boolean => {
  if (paths.length === 0) return true
  if (paths.length === 1) return true

  const firstValue = get__unsafe(obj, paths[0]!)
  for (let i = 1; i < paths.length; i++) {
    if (!is.equal(get__unsafe(obj, paths[i]!), firstValue)) {
      return false
    }
  }
  return true
}

/**
 * Unified namespace for dot notation path operations
 *
 * Provides type-safe and unsafe variants for working with nested objects
 * using dot notation paths (e.g., 'user.address.city')
 */
export const dot = { get, get__unsafe, set, set__unsafe, has, same }
