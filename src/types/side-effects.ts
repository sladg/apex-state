/**
 * SideEffects type definition
 *
 * Configuration types for side effects passed to useSideEffects hook.
 * Simple tuple-based API: [path1, path2]
 */

import type { ListenerRegistration } from '../core/types'
import type { DeepKey, DefaultDepth } from './deep-key'
import type { GenericMeta } from './meta'
import type {
  AggregationPair,
  ComputationPair,
  FlipPair,
  SyncPair,
} from './paths-of-same-value'
import type {
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedFlipPairs,
  ValidatedSyncPairs,
} from './validated-pairs'

/**
 * Clear path rule - "when trigger paths change, set target paths to null"
 *
 * Format: [triggers[], targets[], options?]
 * - triggers: paths that activate the rule
 * - targets: paths to set to null
 * - expandMatch: if true, [*] in targets expands to ALL keys (not just matched key)
 */
export type ClearPathRule<
  DATA extends object,
  Depth extends number = DefaultDepth,
> = [DeepKey<DATA, Depth>[], DeepKey<DATA, Depth>[], { expandMatch?: boolean }?]

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
 *         return [['status', 'updated', {}]]  // Return SCOPED paths (relative to scope)
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
 *         return [['data.computed', computed, {}]]  // SCOPED path (relative to scope)
 *       }
 *     },
 *     {
 *       path: null,   // Always fires — receives ALL changes at any depth
 *       scope: null,  // Get full state
 *       fn: (changes, state) => {
 *         // changes: [['user.profile.name', 'Alice', {}]]  // FULL paths, all depths
 *         // state: full DATA object
 *         return undefined
 *       }
 *     }
 *   ]
 * })
 * ```
 */
export interface SideEffects<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  Depth extends number = DefaultDepth,
> {
  /**
   * Sync paths - keeps specified paths synchronized
   * Format: [path1, path2] - both paths stay in sync
   * Accepts direct `SyncPair<T>[]` or pre-validated result from `syncPairs()`.
   */
  syncPaths?: SyncPair<DATA, Depth>[] | ValidatedSyncPairs<DATA>

  /**
   * Flip paths - keeps specified paths with opposite values
   * Format: [path1, path2] - paths have inverse boolean values
   * Accepts direct `FlipPair<T>[]` or pre-validated result from `flipPairs()`.
   */
  flipPaths?: FlipPair<DATA, Depth>[] | ValidatedFlipPairs<DATA>

  /**
   * Aggregations - aggregates sources into target
   * Format: [target, source] - target is ALWAYS first (left)
   * Multiple pairs can point to same target for multi-source aggregation
   * Accepts direct `AggregationPair<T>[]` or pre-validated result from `aggregationPairs()`.
   */
  aggregations?:
    | AggregationPair<DATA, Depth>[]
    | ValidatedAggregationPairs<DATA>

  /**
   * Clear paths - "when X changes, set Y to null"
   * Format: [triggers[], targets[], { expandMatch?: boolean }?]
   * - Default: [*] in target correlates with trigger's [*] (same key)
   * - expandMatch: true → [*] in target expands to ALL keys
   */
  clearPaths?: ClearPathRule<DATA, Depth>[]

  /**
   * Computations - numeric reduction operations (SUM, AVG)
   * Format: [operation, target, source] - target is computed from sources
   * Multiple pairs can point to same target for multi-source computation
   * Unidirectional: source → target only (writes to target are no-op)
   * Accepts direct `ComputationPair<T>[]` or pre-validated result from `computationPairs()`.
   */
  computations?:
    | ComputationPair<DATA, Depth>[]
    | ValidatedComputationPairs<DATA>

  /**
   * Listeners - react to state changes with scoped state
   * Accepts direct `ListenerRegistration<T>[]` or pre-validated result from `listeners()`.
   */
  listeners?: ListenerRegistration<DATA, META>[]

  /**
   * Anchor path - guard for all resources in this registration.
   * When the anchor path is structurally absent from shadow state, all listeners,
   * BoolLogics, and validators silently skip execution for that pipeline run
   * (no unregistration — they resume when the path reappears).
   */
  anchorPath?: DeepKey<DATA, Depth>
}
