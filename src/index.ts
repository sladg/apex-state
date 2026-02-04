/**
 * @sladg/apex-state
 *
 * Advanced state management wrapper around Valtio with:
 * - Type-safe deep path access
 * - Reactive concerns for computed state and validation
 * - Side effects (sync paths, listeners, validators, aggregations, etc.)
 * - Comprehensive configuration via registries
 */

// =============================================================================
// CORE PUBLIC API
// =============================================================================

// Store factory
export type { ProviderProps, StoreConfig, StoreInstance } from './core/types'
export { createGenericStore } from './store/createStore'

// Standalone hooks
export { useFieldTransformedStore } from './hooks/useFieldTransformedStore'

// Graph builders for side-effect registration
export type {
  Aggregation,
  ListenerRegistration,
  OnStateListener,
} from './core/types'
export {
  registerFlipPair,
  registerListener,
  registerSideEffects,
  registerSyncPair,
} from './sideEffects'

// Type utilities
export type {
  AggregationPair,
  ArrayOfChanges,
  DeepKey,
  DeepKeyFiltered,
  DeepValue,
  EvaluatedConcerns,
  ExtractEvaluateReturn,
  FieldTransformConfig,
  FlipPair,
  GenericMeta,
  PathsWithSameValueAs,
  SyncPair,
} from './types'

// =============================================================================
// CONCERNS SYSTEM (Main Extension Point)
// =============================================================================

// Pre-built concerns for common patterns
export type {
  BaseConcernProps,
  BoolLogic,
  ConcernRegistration,
  ConcernType,
} from './concerns'
export { defaultConcerns, findConcern } from './concerns'
export * as prebuilts from './concerns/prebuilts'

// Validation concern types
export type {
  ValidationError,
  ValidationStateResult,
} from './concerns/prebuilts/validationState'

// Utilities for custom concern builders
/**
 * @for-custom-concerns
 * Evaluates boolean logic expressions against state objects.
 * Used when building custom conditional concerns.
 */
export { evaluateBoolLogic } from './utils/boolLogic'

/**
 * @for-custom-concerns
 * Interpolates template strings with state values.
 * Used when building custom dynamic text concerns.
 */
export { extractPlaceholders, interpolateTemplate } from './utils/interpolation'

// =============================================================================
// SIDE EFFECTS (Internal Configuration)
// =============================================================================

// Note: Side effects are configured internally via the pipeline.
// Users extend functionality primarily through concerns, not side effects registries.
// The old side effects system (validators, listeners, sync paths, etc.) is being
// gradually replaced by the concerns pattern which provides better reactivity
// and type safety.

export type { SideEffects } from './types/sideEffects'

// =============================================================================
// ADVANCED: DIRECT PATH ACCESS
// =============================================================================

/**
 * @advanced
 * Direct deep path access utilities for specialized use cases.
 * Most users should use store hooks (useStore, useFieldStore, etc.) instead.
 * Use these for performance-critical hot paths or advanced patterns.
 *
 * @example
 * ```typescript
 * const value = dot.get(state, 'user.address.street')
 * dot.set(state, 'user.address.street', 'New Street')
 * ```
 */
export { dot } from './utils/dot'
export { _, hashKey } from './utils/hashKey'

/**
 * @advanced
 * Type-checking predicates for runtime value inspection.
 *
 * @example
 * ```typescript
 * import { is } from '@sladg/apex-state'
 *
 * if (is.object(value)) { ... }
 * if (is.array(value)) { ... }
 * ```
 */
export { is } from './utils/is'
