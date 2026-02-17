/**
 * SideEffects type definition
 *
 * Configuration types for side effects passed to useSideEffects hook.
 * Simple tuple-based API: [path1, path2]
 */

import type { ListenerRegistration } from '../core/types'
import type { DeepKey } from './deep-key'
import type { GenericMeta } from './meta'
import type { AggregationPair, FlipPair, SyncPair } from './paths-of-same-value'

/**
 * Clear path rule - "when trigger paths change, set target paths to null"
 *
 * Format: [triggers[], targets[], options?]
 * - triggers: paths that activate the rule
 * - targets: paths to set to null
 * - expandMatch: if true, [*] in targets expands to ALL keys (not just matched key)
 */
export type ClearPathRule<DATA extends object> = [
  DeepKey<DATA>[],
  DeepKey<DATA>[],
  { expandMatch?: boolean }?,
]

/**
 * Side effects configuration for useSideEffects hook
 *
 * @example
 * ```typescript
 * useSideEffects('my-effects', {
 *   syncPaths: [
 *     ['user.email', 'profile.email'],
 *   ],
 *   flipPaths: [
 *     ['isActive', 'isInactive'],
 *   ],
 *   aggregations: [
 *     ['total', 'price1'],  // target <- source (target always first)
 *     ['total', 'price2'],
 *   ],
 *   listeners: [
 *     {
 *       path: 'user.profile',  // Watch user.profile.* changes
 *       scope: 'user.profile',  // Receive scoped state
 *       fn: (changes, state) => {
 *         // changes: [['name', 'Alice', {}]]  // RELATIVE to scope
 *         // state: user.profile sub-object
 *         return [['status', 'updated', {}]]  // Return FULL paths
 *       }
 *     },
 *     {
 *       path: 'user.profile.name',  // Watch specific path
 *       scope: null,                // Get full state
 *       fn: (changes, state) => {
 *         // changes: [['user.profile.name', 'Alice', {}]]  // FULL path
 *         // state: full DATA object
 *         return undefined
 *       }
 *     },
 *     {
 *       path: 'p.123.g.abc.data.strike',  // Watch deep path
 *       scope: 'p.123.g.abc',              // Get parent scope
 *       fn: (changes, state) => {
 *         // changes: [['data.strike', value, {}]]  // RELATIVE to scope
 *         // state: p.123.g.abc object
 *         return [['some.value.elsewhere', computed, {}]]  // FULL path
 *       }
 *     }
 *   ]
 * })
 * ```
 */
export interface SideEffects<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * Sync paths - keeps specified paths synchronized
   * Format: [path1, path2] - both paths stay in sync
   */
  syncPaths?: SyncPair<DATA>[]

  /**
   * Flip paths - keeps specified paths with opposite values
   * Format: [path1, path2] - paths have inverse boolean values
   */
  flipPaths?: FlipPair<DATA>[]

  /**
   * Aggregations - aggregates sources into target
   * Format: [target, source] - target is ALWAYS first (left)
   * Multiple pairs can point to same target for multi-source aggregation
   */
  aggregations?: AggregationPair<DATA>[]

  /**
   * Clear paths - "when X changes, set Y to null"
   * Format: [triggers[], targets[], { expandMatch?: boolean }?]
   * - Default: [*] in target correlates with trigger's [*] (same key)
   * - expandMatch: true → [*] in target expands to ALL keys
   */
  clearPaths?: ClearPathRule<DATA>[]

  /**
   * Listeners - react to state changes with scoped state
   */
  listeners?: ListenerRegistration<DATA, META>[]
}
