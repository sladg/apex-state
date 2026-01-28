/**
 * Pipeline types for synchronizer execution
 *
 * Defines the synchronizer function signature and pipeline configuration.
 * Synchronizers process ArrayOfChanges and can add/modify/remove changes.
 */

import type { ArrayOfChanges, GenericMeta } from '../types'

/**
 * A synchronizer processes changes and can add/modify/remove changes.
 * It receives current changes and state, returns modified changes.
 *
 * Synchronizers are pure functions that:
 * - Take an array of changes and current state
 * - Return a new array of changes (can be same, modified, or expanded)
 * - Do NOT mutate the state directly
 * - Can add new changes based on existing ones
 *
 * @example
 * ```typescript
 * const syncPathsSynchronizer: Synchronizer<AppState> = (changes, state) => {
 *   // Process changes, potentially add new ones
 *   return [...changes, ...derivedChanges]
 * }
 * ```
 */
export type Synchronizer<
  DATA extends object,
  META extends GenericMeta = GenericMeta
> = (
  changes: ArrayOfChanges<DATA, META>,
  state: DATA
) => ArrayOfChanges<DATA, META>

/**
 * Configuration for the pipeline execution
 *
 * Defines the order of synchronizers and max iteration limit.
 * The pipeline executes synchronizers in order until changes stabilize.
 */
export interface PipelineConfig<
  DATA extends object,
  META extends GenericMeta = GenericMeta
> {
  /**
   * Array of synchronizers to execute in order
   * Each synchronizer has a name (for debugging) and function
   */
  synchronizers: Array<{
    name: string
    fn: Synchronizer<DATA, META>
  }>

  /**
   * Maximum number of iterations before throwing an error
   * Prevents infinite loops in side-effects
   * @default 100
   */
  maxIterations: number
}
