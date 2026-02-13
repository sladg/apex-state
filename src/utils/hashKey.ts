/**
 * Hash key utilities
 *
 * Utilities for working with hash key notation in Record-based paths.
 * Hash keys ([*]) are type-level markers for indexing into Records/HashMaps.
 *
 * @example
 * ```typescript
 * import { _ } from '@sladg/apex-state'
 *
 * // Use _ in template strings
 * const path = `users.${_('u1')}.posts.${_('p1')}.name`
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
