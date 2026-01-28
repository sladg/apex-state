/**
 * PathsOfSameValue - Type for mapping paths to paths with same value
 *
 * Used for sync paths configuration where multiple paths should maintain
 * the same value (bidirectional synchronization).
 *
 * @example
 * ```typescript
 * type UserState = {
 *   user: { email: string }
 *   profile: { email: string }
 *   settings: { notificationEmail: string }
 * }
 *
 * const syncConfig: Partial<PathsOfSameValue<UserState>> = {
 *   "user.email": ["profile.email", "settings.notificationEmail"],
 *   "profile.email": ["user.email", "settings.notificationEmail"],
 *   "settings.notificationEmail": ["user.email", "profile.email"]
 * }
 * ```
 */

import type { DeepKey } from './index'

/**
 * Record mapping each path to an array of paths with the same value.
 *
 * Each key is a path in DATA, and the value is an array of other paths
 * that should be kept in sync with that key.
 */
export type PathsOfSameValue<DATA extends object> = Record<
  DeepKey<DATA>,
  Array<DeepKey<DATA>>
>
