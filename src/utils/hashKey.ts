/**
 * Hash key utilities
 *
 * Utilities for working with hash key notation in Record-based paths.
 * Hash keys (`[${string}]`) are type-level markers for indexing into Records/HashMaps.
 *
 * The return type depends on whether the input is a literal or generic string:
 * - `_('u1')` → type `[u1]` (literal: specific bracketed key)
 * - `_(Status.Active)` → type `[active]` (enum literal: specific)
 * - `_(dynamicVar)` → type `[*]` (generic string: concrete fallback)
 *
 * All return types extend `[${string}]` (the HASH_KEY pattern in DeepKey).
 *
 * @example
 * ```typescript
 * import { _ } from '@sladg/apex-state'
 *
 * // Use _ in template strings
 * const path = `users.${_('u1')}.posts.${_('p1')}.name`
 * ```
 */

/**
 * When K is a string literal → `[${K}]` (specific, e.g., `[u1]`, `[active]`)
 * When K is generic string → `[*]` (concrete fallback for dynamic IDs)
 *
 * Both extend HASH_KEY (`[${string}]`), so both match DeepKey patterns.
 */
type HashKeyResult<K extends string> = string extends K ? '[*]' : `[${K}]`

/**
 * Converts a concrete ID to hash key notation for inline template string usage.
 * Returns the concrete ID typed as a bracketed key for Record/HashMap indexing.
 *
 * @param id - The concrete ID (e.g., "u1", "p1", Status.Active)
 * @returns The concrete ID typed as `[${K}]` (literal) or `[*]` (dynamic)
 *
 * @example
 * ```typescript
 * const path = `users.${_('u1')}.posts.${_('p1')}.name`
 * // → "users.u1.posts.p1.name" (typed as `users.[u1].posts.[p1].name`)
 * ```
 */
export const _ = <K extends string>(id: K): HashKeyResult<K> =>
  id as unknown as HashKeyResult<K>
