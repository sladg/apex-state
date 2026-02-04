/**
 * Hash key utilities
 *
 * Utilities for working with hash key notation in Record-based paths.
 * Hash keys ([*]) are type-level markers for indexing into Records/HashMaps.
 *
 * @example
 * ```typescript
 * import { _, hashKey } from '@sladg/apex-state'
 *
 * // Use _ in template strings
 * const path = `users.${_('u1')}.posts.${_('p1')}.name`
 *
 * // Use hashKey namespace for validation
 * hashKey.rejectDynamic(path)
 * ```
 */

import type { HASH_KEY } from '../types'

/**
 * Converts a concrete ID to hash key notation for inline template string usage
 * Returns the concrete ID typed as HASH_KEY for Record/HashMap indexing
 *
 * @param id - The concrete ID (e.g., "u1", "p1", "c1")
 * @returns The concrete ID typed as HASH_KEY
 *
 * @example
 * ```typescript
 * const path = `users.${_('u1')}.posts.${_('p1')}.name`
 * // → "users.u1.posts.p1.name" (typed as containing HASH_KEY)
 * ```
 */
export const _ = (id: string): HASH_KEY => id as HASH_KEY

/**
 * Rejects paths with dynamic hash key notation [*]
 * Hash keys are for type-level path matching only, not runtime access
 *
 * @throws Error if path contains [*]
 *
 * @example
 * ```typescript
 * rejectDynamic('users.u1.posts.p1') // OK
 * rejectDynamic('users.[*].posts') // throws Error
 * ```
 */
const rejectDynamic = <P extends string>(path: P): void => {
  // Fast validation: check each part directly instead of scanning entire string
  const parts = path.split('.')
  for (const part of parts) {
    if (part === '[*]') {
      throw new Error(
        `Path contains [*] hash key which is not allowed in store operations. Use concrete paths only.`,
      )
    }
  }
}

/**
 * Hash key utilities namespace
 *
 * Provides utilities for working with hash keys in Record-based paths
 */
export const hashKey = {
  /** Reject paths with dynamic hash keys */
  rejectDynamic,
  /** Alias for _ function */
  _,
} as const
