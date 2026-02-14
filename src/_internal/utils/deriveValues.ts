/**
 * Derived value auto-detection
 *
 * Scans an object for getter properties and extracts them for use with
 * valtio's proxyWithComputed pattern.
 */

import { is } from '~/utils/is'

/**
 * Detects getter properties in an object (including nested objects)
 * Returns an object mapping property paths to their getter functions
 *
 * @example
 * ```typescript
 * const obj = {
 *   firstName: 'John',
 *   lastName: 'Doe',
 *   get fullName() { return `${this.firstName} ${this.lastName}` }
 * }
 *
 * const getters = detectGetters(obj)
 * // { fullName: (snap) => snap.firstName + ' ' + snap.lastName }
 * ```
 */
export const detectGetters = <T extends object>(
  obj: T,
  prefix = '',
): Record<string, (snap: any) => any> => {
  const getters: Record<string, (snap: any) => any> = {}

  // Get own property descriptors to detect getters
  const descriptors = Object.getOwnPropertyDescriptors(obj)

  for (const [key, descriptor] of Object.entries(descriptors)) {
    // Skip non-enumerable properties and symbols
    if (!descriptor.enumerable || is.symbol(key)) {
      continue
    }

    const fullPath = prefix ? `${prefix}.${key}` : key

    // Check if it's a getter
    if (descriptor.get) {
      // Convert getter to a function that works with valtio snapshots
      getters[fullPath] = (snap: any) => {
        // Get the nested object if there's a prefix
        const target = prefix
          ? prefix.split('.').reduce((obj, k) => obj?.[k], snap)
          : snap

        // Call the original getter with the appropriate context
        return descriptor.get!.call(target)
      }
    }
    // Recursively check nested objects (but not functions, arrays, or primitives)
    else if (is.object(descriptor.value)) {
      const nestedGetters = detectGetters(descriptor.value, fullPath)
      Object.assign(getters, nestedGetters)
    } else {
      // Primitive values or arrays - no action needed
    }
  }

  return getters
}

/**
 * Extracts getters from an object and returns base object and computed definitions
 * Suitable for use with valtio's proxyWithComputed
 *
 * @example
 * ```typescript
 * const obj = {
 *   a: 1,
 *   b: 2,
 *   get sum() { return this.a + this.b }
 * }
 *
 * const { base, computed } = extractGetters(obj)
 * // base = { a: 1, b: 2 }
 * // computed = { sum: (snap) => snap.a + snap.b }
 * ```
 */
export const extractGetters = <T extends object>(
  obj: T,
): {
  base: Partial<T>
  computed: Record<string, (snap: any) => any>
} => {
  const base: any = {}
  const computed: Record<string, (snap: any) => any> = {}

  const descriptors = Object.getOwnPropertyDescriptors(obj)

  for (const [key, descriptor] of Object.entries(descriptors)) {
    if (!descriptor.enumerable || is.symbol(key)) {
      continue
    }

    if (descriptor.get) {
      // It's a getter - add to computed
      computed[key] = (snap: any) => descriptor.get!.call(snap)
    } else {
      // Regular property - add to base
      base[key] = descriptor.value
    } // else: no action needed for other descriptor types
  }

  return { base, computed }
}
