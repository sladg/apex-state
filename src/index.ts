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
export type {
  DebugConfig,
  DebugTrack,
  DebugTrackEntry,
  ProviderProps,
  StoreConfig,
  StoreInstance,
} from './core/types'
export { createGenericStore, type GenericStoreApi } from './store/create-store'

// Standalone hooks - composable field utilities
export {
  type BufferedField,
  type FieldInput,
  useBufferedField,
} from './hooks/use-buffered-field'
export {
  type KeyboardSelectConfig,
  type SelectOption,
  useKeyboardSelect,
} from './hooks/use-keyboard-select'
export {
  type ThrottleConfig,
  type ThrottleFieldInput,
  useThrottledField,
} from './hooks/use-throttled-field'
export {
  type TransformConfig,
  useTransformedField,
} from './hooks/use-transformed-field'

// Graph builders for side-effect registration
export type {
  Aggregation,
  ListenerRegistration,
  OnStateListener,
} from './core/types'
export { registerSideEffects } from './sideEffects'

// Type utilities
export type {
  AggregationPair,
  ArrayOfChanges,
  CheckAggregationPairs,
  CheckComputationPairs,
  CheckPairValueMatch,
  CheckSyncPairs,
  ComputationOp,
  ComputationPair,
  ConcernRegistrationMap,
  DeepKey,
  DeepKeyFiltered,
  DeepPartial,
  DeepRequired,
  DeepValue,
  EvaluatedConcerns,
  ExtractEvaluateReturn,
  FlipPair,
  GenericMeta,
  HASH_KEY,
  PathsWithSameValueAs,
  SyncPair,
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedFlipPairs,
  ValidatedListeners,
  ValidatedSyncPairs,
  ValidationSchema,
} from './types'

// Brand symbols for validated pairs (unique symbols require value export for .d.ts)
export { STORE_DATA, VALIDATED } from './types/validated-pairs'

// Lazy-validated pair helpers (scale to large state types without TS2589)
export {
  aggregationPairs,
  computationPairs,
  flipPairs,
  listeners,
  syncPairs,
} from './utils/pair-helpers'

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
  ValidationStateConcern,
  ValidationStateInput,
  ValidationStateResult,
} from './concerns/prebuilts/validation-state'

// Utilities for custom concern builders
/**
 * @for-custom-concerns
 * Evaluates boolean logic expressions against state objects.
 * Used when building custom conditional concerns.
 */
export { evaluateBoolLogic } from './utils/bool-logic'

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

export type { ClearPathRule, SideEffects } from './types/side-effects'

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
export { _, hashKey } from './utils/hash-key'

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

/**
 * @advanced
 * Utilities for applying changes to objects.
 *
 * @example
 * ```typescript
 * import { applyChangesToObject } from '@sladg/apex-state'
 *
 * const state = { user: { name: 'Alice' } }
 * const newState = applyChangesToObject(state, [['user.name', 'Bob']])
 * ```
 */
export { applyChangesToObject } from './utils/apply-changes'

/**
 * @advanced
 * Deep clone that preserves getters, setters, and property descriptors.
 * Returns a fully independent copy — mutations to the clone never affect the original.
 *
 * @example
 * ```typescript
 * import { deepClone } from '@sladg/apex-state'
 *
 * const original = { name: 'Alice', get greeting() { return `Hi ${this.name}` } }
 * const cloned = deepClone(original)
 * cloned.name = 'Bob'
 * cloned.greeting // 'Hi Bob'
 * original.greeting // 'Hi Alice' — untouched
 * ```
 */
export { deepClone } from './utils/deep-clone'
