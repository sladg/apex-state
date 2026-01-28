/**
 * Synchronizers registry and default pipeline configuration
 *
 * Exports factory functions for creating synchronizers with proper registries.
 * Users should create synchronizers using the factory functions:
 * - createSyncPathsSynchronizer(registry)
 * - createFlipPathsSynchronizer(registry)
 * - createListenersSynchronizer(registry)
 * - createValidatorsSynchronizer(registry, errorStorePath)
 * - createAggregationsSynchronizer(registry)
 * - createClearPathsSynchronizer(registry)
 */

import type { PipelineConfig } from '../types'
import type { GenericMeta } from '../../types'

// Re-export factory functions for synchronizers
export { createSyncPathsSynchronizer } from './syncPaths'
export { createFlipPathsSynchronizer } from './flipPaths'
export { createListenersSynchronizer } from './listeners'
export { createValidatorsSynchronizer } from './validators'
export { createAggregationsSynchronizer } from './aggregations'
export { createClearPathsSynchronizer } from './clearPaths'

/**
 * Creates an empty default pipeline configuration.
 *
 * Users should configure their own pipelines using factory functions
 * and registries for the side-effects they need.
 *
 * Recommended pipeline order:
 * 1. sync - Sync path side-effects (createSyncPathsSynchronizer)
 * 2. flip - Flip path bidirectional sync (createFlipPathsSynchronizer)
 * 3. listeners - State change listeners (createListenersSynchronizer)
 * 4. validators - Validation side-effects (createValidatorsSynchronizer)
 * 5. aggregations - Aggregation recalculations (createAggregationsSynchronizer)
 * 6. clear - Clear path side-effects (createClearPathsSynchronizer)
 *
 * This order ensures:
 * - Sync/flip happen first to propagate changes
 * - Listeners can react to propagated changes
 * - Validators can check all changes
 * - Aggregations recalculate after validation
 * - Clear paths happen last
 *
 * @returns Empty pipeline configuration with max 100 iterations
 *
 * @example
 * ```typescript
 * import { createListenersRegistry } from './sideEffects/listeners'
 * import { createListenersSynchronizer } from './pipeline/synchronizers'
 *
 * const listenersRegistry = createListenersRegistry()
 * const pipelineConfig = {
 *   synchronizers: [
 *     { name: 'listeners', fn: createListenersSynchronizer(listenersRegistry) }
 *   ],
 *   maxIterations: 100
 * }
 * ```
 */
export const createDefaultPipeline = <
  DATA extends object,
  META extends GenericMeta = GenericMeta
>(): PipelineConfig<DATA, META> => {
  return {
    synchronizers: [],
    maxIterations: 100,
  }
}
