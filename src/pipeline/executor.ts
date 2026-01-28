/**
 * Pipeline execution engine
 *
 * Runs changes through the synchronizer pipeline until they stabilize.
 * Detects infinite loops and applies changes atomically.
 */

import { pipe } from 'remeda'
import type { ArrayOfChanges, GenericMeta } from '../types'
import type { PipelineConfig } from './types'
import { deepSet } from '../store/utils/deepAccess'

/**
 * Executes the synchronizer pipeline until changes stabilize.
 *
 * The pipeline runs iteratively:
 * 1. Apply all synchronizers in order using remeda pipe
 * 2. If new changes were added, run again
 * 3. Stop when no new changes or max iterations reached
 *
 * Stabilization is detected by comparing the change array length
 * before and after a full pipeline pass. If the length is the same,
 * the changes have stabilized.
 *
 * @throws Error if infinite loop detected (max iterations exceeded)
 *
 * @example
 * ```typescript
 * const finalChanges = executePipeline(
 *   [['count', 1, {}]],
 *   state,
 *   pipelineConfig
 * )
 * ```
 */
export function executePipeline<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(
  initialChanges: ArrayOfChanges<DATA, META>,
  state: DATA,
  config: PipelineConfig<DATA, META>
): ArrayOfChanges<DATA, META> {
  let currentChanges = initialChanges
  let iteration = 0

  while (iteration < config.maxIterations) {
    const changesBeforeIteration = currentChanges.length

    // Run through all synchronizers in sequence
    // Each synchronizer receives changes and state, returns modified changes
    for (const { fn } of config.synchronizers) {
      currentChanges = fn(currentChanges, state)
    }

    // If no new changes were added, we're done
    if (currentChanges.length === changesBeforeIteration) {
      break
    }

    iteration++
  }

  if (iteration >= config.maxIterations) {
    throw new Error(
      `Pipeline exceeded max iterations (${config.maxIterations}). ` +
      'Possible infinite loop in side-effects.'
    )
  }

  return currentChanges
}

/**
 * Apply changes atomically to valtio state
 *
 * Applies all changes in sequence to the valtio proxy.
 * Valtio batches updates automatically in React 18+, ensuring
 * a single re-render for all changes.
 *
 * @example
 * ```typescript
 * applyChanges(store.state, [
 *   ['user.name', 'Alice', {}],
 *   ['count', 42, {}]
 * ])
 * // Single re-render with both changes applied
 * ```
 */
export function applyChanges<
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(
  state: DATA,
  changes: ArrayOfChanges<DATA, META>
): void {
  // Apply all changes to the valtio proxy
  // Valtio batches these automatically in React 18+
  changes.forEach((change) => {
    const [path, value] = change
    deepSet(state, path as any, value as any)
  })
}
