/**
 * Test helper utilities
 *
 * Type-safe wrappers for runtime/dynamic paths in tests.
 */

import type { ArrayOfChanges, GenericMeta } from '~/types'
import type { FlipPair, SyncPair } from '~/types/pathsOfSameValue'

/**
 * Type-safe test helpers for dynamic paths
 *
 * These helpers centralize type assertions needed when working with
 * dynamic/runtime paths in tests. The casts are safe because tests
 * verify runtime behavior, not compile-time type safety.
 */
export const typeHelpers = {
  /**
   * Create a change tuple with runtime path.
   * Use when path is dynamic (e.g., template literal) or when TypeScript
   * can't infer the tuple type correctly.
   *
   * @example
   * ```typescript
   * // Instead of: [`items.${id}.qty`, 10, {}]
   * typeHelpers.change(`items.${id}.qty`, 10, {})
   * ```
   */
  change: <DATA extends object, META extends GenericMeta = GenericMeta>(
    path: string,
    value: unknown,
    meta: META = {} as META,
  ): ArrayOfChanges<DATA, META>[number] =>
    [path, value, meta] as ArrayOfChanges<DATA, META>[number],

  /**
   * Create a sync pair tuple.
   * Use when TypeScript infers string[] instead of [string, string].
   *
   * @example
   * ```typescript
   * // Instead of: ['firstName', 'lastName']
   * typeHelpers.syncPair('firstName', 'lastName')
   * ```
   */
  syncPair: <DATA extends object>(
    path1: string,
    path2: string,
  ): SyncPair<DATA> => [path1, path2] as unknown as SyncPair<DATA>,

  /**
   * Create a flip pair tuple.
   * Use when TypeScript infers string[] instead of [string, string].
   *
   * @example
   * ```typescript
   * // Instead of: ['isActive', 'isInactive']
   * typeHelpers.flipPair('isActive', 'isInactive')
   * ```
   */
  flipPair: <DATA extends object>(
    path1: string,
    path2: string,
  ): FlipPair<DATA> => [path1, path2] as unknown as FlipPair<DATA>,

  /**
   * Create an array of changes with runtime paths.
   * Use when creating multiple changes with dynamic paths.
   *
   * @example
   * ```typescript
   * typeHelpers.changes([
   *   [`items.${id}.qty`, 10],
   *   [`items.${id}.price`, 25],
   * ])
   * ```
   */
  changes: <DATA extends object, META extends GenericMeta = GenericMeta>(
    items: [string, unknown, META?][],
  ): ArrayOfChanges<DATA, META> =>
    items.map(([path, value, meta]) => [
      path,
      value,
      meta ?? ({} as META),
    ]) as ArrayOfChanges<DATA, META>,
}
