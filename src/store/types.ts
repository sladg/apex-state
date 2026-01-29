/**
 * Store-specific types
 *
 * Type definitions for the store configuration, provider props,
 * and internal store instance structure.
 *
 * Architecture: Single unified proxy containing:
 * - state: User data (tracked by valtio)
 * - _concerns: Computed concern values (tracked by valtio)
 * - _internal: Graphs, registrations, processing queue (NOT tracked - wrapped in ref())
 * - config: Store configuration
 */

import type Graph from 'graphology'
import type { ReactNode } from 'react'

import type { ConcernType } from '../concerns/types'
import type { ArrayOfChanges, GenericMeta } from '../types'

/**
 * Configuration options for store creation
 */
export interface StoreConfig {
  /**
   * Path in the state object where errors will be stored
   * @default "_errors"
   */
  errorStorePath?: string

  /**
   * Maximum iterations for change processing loop
   * Prevents infinite loops in side-effects
   * @default 100
   */
  maxIterations?: number
}

/**
 * Props for the Provider component
 */
export interface ProviderProps<DATA extends object> {
  /**
   * Initial state data
   */
  initialState: DATA

  /**
   * Optional path for error storage
   * @default "_errors"
   */
  errorStorePath?: string

  /**
   * Child components to wrap
   */
  children: ReactNode
}

/**
 * Aggregation rule for computing derived values from multiple sources
 */
export interface AggregationRule<DATA extends object = object> {
  id: string
  targetPath: string
  sourcePaths: string[]
  aggregate: (state: DATA, sourcePaths: string[]) => unknown
}

/**
 * Listener function that reacts to changes
 * Returns additional changes to apply, or undefined
 */
export type OnStateListener<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> = (
  change: ArrayOfChanges<DATA, META>[number],
  state: DATA,
) => ArrayOfChanges<DATA, META> | undefined

/**
 * Pre-computed graphs for side-effect processing
 * Uses graphology for efficient graph operations
 */
export interface SideEffectGraphs<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * Sync paths graph (undirected)
   * Nodes are paths, edges connect synced paths
   * Use graphology-components for connected components
   */
  sync: Graph

  /**
   * Flip paths graph (undirected)
   * Nodes are paths, edges connect flipped paths
   */
  flip: Graph

  /**
   * Aggregations graph (directed)
   * Edges: source path → target path
   * Target nodes have 'rules' attribute: AggregationRule[]
   * Query: outNeighbors(changedPath) to find affected targets
   */
  aggregations: Graph

  /**
   * Listeners: path → array of listener functions
   */
  listeners: Map<string, OnStateListener<DATA, META>[]>
}

/**
 * Registration tracking for cleanup
 */
export interface Registrations {
  /**
   * Concern types: path → array of concern types registered
   */
  concerns: Map<string, ConcernType[]>

  /**
   * Effect cleanup functions from valtio-reactive
   */
  effectCleanups: Set<() => void>

  /**
   * Side-effect cleanup functions (for unregistration)
   */
  sideEffectCleanups: Map<string, () => void>
}

/**
 * Processing state for change batching
 */
export interface ProcessingState<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * Current queue of changes to process
   */
  queue: ArrayOfChanges<DATA, META>

  /**
   * Reentrancy guard - true when processing is in progress
   */
  isProcessing: boolean
}

/**
 * Internal store state (NOT tracked by valtio)
 * Wrapped in ref() to prevent proxy tracking
 */
export interface InternalState<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * Pre-computed graphs for side-effect processing
   */
  graphs: SideEffectGraphs<DATA, META>

  /**
   * Registration tracking for cleanup
   */
  registrations: Registrations

  /**
   * Processing state for change batching
   */
  processing: ProcessingState<DATA, META>
}

/**
 * Computed concern values
 * Structure: { [path]: { [concernName]: value } }
 */
export type ConcernValues = Record<string, Record<string, unknown>>

/**
 * Store instance structure
 *
 * Container for independent proxies:
 * - state: User data proxy (tracked)
 * - _concerns: Computed values proxy (tracked)
 * - _internal: Graphs + processing (NOT tracked via ref())
 * - config: Store configuration
 *
 * The StoreInstance itself is a plain object, NOT a proxy, to ensure
 * that state and _concerns remain independent for dependency tracking.
 */
export interface StoreInstance<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * The valtio proxy state
   * Application state - single source of truth
   * User actions WRITE to this, effects READ from this
   */
  state: DATA

  /**
   * Computed concern values (tracked by valtio)
   * Effects WRITE to this, UI components READ from this
   * Structure: { [path]: { [concernName]: value } }
   */
  _concerns: ConcernValues

  /**
   * Internal state (NOT tracked - wrapped in ref())
   * Contains graphs, registrations, and processing queue
   */
  _internal: InternalState<DATA, META>

  /**
   * Store configuration
   */
  config: Required<StoreConfig>
}
