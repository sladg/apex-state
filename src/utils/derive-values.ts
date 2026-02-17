/**
 * Derived value auto-detection
 *
 * Scans an object for getter properties and extracts them for use with
 * valtio's computed() pattern. Provides utilities to extract getters from
 * initial state and re-attach them as selectively-tracked computed proxies.
 */

import { computed } from 'valtio-reactive'

import { dot } from './dot'
import { is } from './is'

export type GetterMap = Record<
  string,
  { get: () => unknown; parentPath: string }
>

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
 * Extracts getters from an object (including nested) and returns a getter-free base
 * plus a map of dot-path keyed getter definitions for use with computed().
 *
 * @example
 * ```typescript
 * const obj = {
 *   a: 1,
 *   b: 2,
 *   get sum() { return this.a + this.b },
 *   nested: {
 *     x: 10,
 *     get double() { return this.x * 2 }
 *   }
 * }
 *
 * const { base, computed } = extractGetters(obj)
 * // base = { a: 1, b: 2, nested: { x: 10 } }
 * // computed = {
 * //   sum: { get: [getter fn], parentPath: '' },
 * //   'nested.double': { get: [getter fn], parentPath: 'nested' }
 * // }
 * ```
 */
export const extractGetters = <T extends object>(
  obj: T,
  prefix = '',
): {
  base: Partial<T>
  computed: GetterMap
} => {
  const base: any = {}
  const computed: GetterMap = {}

  const descriptors = Object.getOwnPropertyDescriptors(obj)

  for (const [key, descriptor] of Object.entries(descriptors)) {
    if (!descriptor.enumerable || is.symbol(key)) continue
    const fullPath = prefix ? `${prefix}.${key}` : key

    if (descriptor.get) {
      // Getter → store original getter fn + the parent path for context binding
      computed[fullPath] = { get: descriptor.get, parentPath: prefix }
    } else if (is.object(descriptor.value)) {
      // Recurse into nested objects
      const nested = extractGetters(descriptor.value, fullPath)
      base[key] = nested.base
      Object.assign(computed, nested.computed)
    } else {
      base[key] = descriptor.value
    }
  }

  return { base, computed }
}

/**
 * Splits raw initial state into a getter-free base (safe for JSON clone) and a getter map.
 * Used by Provider to prepare state before proxy creation.
 */
export const prepareInitialState = <T extends object>(
  rawState: T,
): { initialState: T; getterMap: GetterMap } => {
  const { base, computed: getterMap } = extractGetters(rawState)
  const clonedBase = structuredClone(base) as T
  return { initialState: clonedBase, getterMap }
}

/**
 * Re-attaches extracted getters onto a valtio proxy as computed() proxies
 * for selective dependency tracking.
 *
 * Three-phase approach:
 * 1. Attach raw getters on stateProxy so inter-getter references (e.g., totalWithTax
 *    accessing this.total) work during computed()'s eager evaluation.
 * 2. Create computed() — watch() runs eagerly, reads through the raw getters for tracking.
 * 3. Replace raw getters with computed-backed getters for selective re-evaluation.
 *
 * Cleanup: computed() internally creates watch() subscriptions that have no explicit
 * dispose API. These are cleaned up by GC once the referenced stateProxy is unreachable
 * (e.g., after Provider unmount when nothing holds a reference to the proxy).
 */
export const attachComputedGetters = <T extends object>(
  stateProxy: T,
  getterMap: GetterMap,
): void => {
  if (Object.keys(getterMap).length === 0) return

  // Phase 1: Attach raw getters on stateProxy so inter-getter references work
  for (const [dotPath, { get: getterFn, parentPath }] of Object.entries(
    getterMap,
  )) {
    const parts = dotPath.split('.')
    const propName = parts.pop()!
    const parentObj =
      parts.length > 0
        ? (dot.get__unsafe(stateProxy, parts.join('.')) as object)
        : stateProxy

    Object.defineProperty(parentObj, propName, {
      get() {
        const ctx = parentPath
          ? dot.get__unsafe(stateProxy, parentPath)
          : stateProxy
        return getterFn.call(ctx)
      },
      enumerable: true,
      configurable: true,
    })
  }

  // Phase 2: Create computed() — reads through the raw getters above for tracking
  const computedDefs: Record<string, () => unknown> = {}
  for (const [dotPath, { get: getterFn, parentPath }] of Object.entries(
    getterMap,
  )) {
    computedDefs[dotPath] = () => {
      const parent = parentPath
        ? dot.get__unsafe(stateProxy, parentPath)
        : stateProxy
      return getterFn.call(parent)
    }
  }

  const computedProxy = computed(computedDefs)

  // Phase 3: Replace raw getters with computed-backed getters for selective re-eval
  for (const dotPath of Object.keys(getterMap)) {
    const parts = dotPath.split('.')
    const propName = parts.pop()!
    const parentObj =
      parts.length > 0
        ? (dot.get__unsafe(stateProxy, parts.join('.')) as object)
        : stateProxy

    Object.defineProperty(parentObj, propName, {
      get: () => computedProxy[dotPath as keyof typeof computedProxy],
      enumerable: true,
      configurable: true,
    })
  }
}
