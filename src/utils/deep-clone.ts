/**
 * Deep clone utility — single swap point for the cloning implementation.
 *
 * Uses @jsbits/deep-clone in "exact" mode which preserves:
 * - Getter/setter property descriptors
 * - Property attributes (enumerable, configurable, writable)
 * - Prototypes
 *
 * Swap the implementation here to change cloning behavior everywhere.
 */

import _deepClone from '@jsbits/deep-clone'

/**
 * Detect circular references in an object graph.
 * Throws a descriptive error if a cycle is found.
 */
const assertNoCycles = (value: unknown): void => {
  if (value === null || typeof value !== 'object') return

  // Track current ancestor chain only — shared references (diamonds) are fine
  const ancestors = new WeakSet<object>()

  const walk = (obj: object, path: string): void => {
    if (ancestors.has(obj)) {
      throw new Error(
        `[deepClone] Circular reference detected at "${path}". ` +
          'State objects must not contain self-references.',
      )
    }
    ancestors.add(obj)

    const descriptors = Object.getOwnPropertyDescriptors(obj)
    for (const [key, desc] of Object.entries(descriptors)) {
      // Skip getters — they may have side effects or depend on runtime context
      if (desc.get) continue
      if (desc.value !== null && typeof desc.value === 'object') {
        walk(desc.value as object, path ? `${path}.${key}` : key)
      }
    }

    ancestors.delete(obj)
  }

  walk(value as object, '')
}

/**
 * Deep clone an object, preserving getters, setters, and property descriptors.
 * Returns a fully independent copy — mutations to the clone never affect the original.
 *
 * Throws if the input contains circular references.
 */
export const deepClone = <T>(value: T): T => {
  assertNoCycles(value)
  return _deepClone(value, true) as T
}
