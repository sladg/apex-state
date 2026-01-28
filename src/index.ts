/**
 * @sladg/apex-state
 *
 * Advanced state management wrapper around Valtio with:
 * - Sync paths for bidirectional synchronization
 * - Aggregations for computed state
 * - Side effects (listeners, validators, clear paths, flip paths)
 * - Deep path operations with type safety
 */

// Export core type utilities
export type { DeepKey, DeepValue, GenericMeta, ArrayOfChanges } from './types'

// Export store factory and types
export { createGenericStore } from './store/createStore'
export type { StoreReturn } from './store/createStore'
export type { StoreConfig, ProviderProps, StoreInstance } from './store/types'

// Export hooks
export { useStore, useJitStore, useSideEffects, useStoreContext } from './hooks'
export { useErrors } from './hooks/useErrors'
export type { JitStoreReturn } from './hooks'

// Export side effects types and registries
export type { SideEffects } from './types/sideEffects'
export { SideEffectsRegistry } from './store/sideEffectsRegistry'

// Export side effect registry factories
export { createSyncPathsRegistry, createSyncPathGraph } from './sideEffects/syncPaths'
export type { SyncPathsRegistry, SyncPathGraph } from './sideEffects/syncPaths'

export { createFlipPathsRegistry } from './sideEffects/flipPaths'
export type { FlipPathsRegistry } from './sideEffects/flipPaths'

export { createListenersRegistry } from './sideEffects/listeners'
export type { ListenersRegistry, OnStateChangesListenerFunction } from './sideEffects/listeners'

export { createValidatorsRegistry } from './sideEffects/validators'
export type { ValidatorsRegistry, ValidatorConfig, StoredError } from './sideEffects/validators'

export { createAggregationGraph, createAggregationsRegistry } from './sideEffects/aggregations'
export type { AggregationGraph, AggregationsRegistry, Aggregation } from './sideEffects/aggregations'

export { createClearPathsRegistry } from './sideEffects/clearPaths'
export type { ClearPathsRegistry, ClearPathConfig } from './sideEffects/clearPaths'

// Export pipeline types and executor
export type { Synchronizer, PipelineConfig } from './pipeline/types'
export { executePipeline, applyChanges } from './pipeline/executor'
export {
  createSyncPathsSynchronizer,
  createFlipPathsSynchronizer,
  createListenersSynchronizer,
  createValidatorsSynchronizer,
  createAggregationsSynchronizer,
  createClearPathsSynchronizer,
  createDefaultPipeline,
} from './pipeline/synchronizers'

// Export store utilities
export { deepGet, deepSet, deepHas } from './store/utils/deepAccess'
export { detectGetters, extractGetters } from './store/utils/deriveValues'

// Placeholder export for initial setup
export const VERSION = '0.1.0'
