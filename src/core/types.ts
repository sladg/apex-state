/**
 * Core Store Types
 *
 * Foundational type definitions for the store instance.
 * These types are used throughout the library.
 */

import type { ReactNode } from 'react'

import type { ConcernType } from '../concerns/types'
import type {
  ArrayOfChanges,
  DeepKey,
  DeepRequired,
  GenericMeta,
} from '../types'
import type { Timing } from '../utils/timing'
import type { FlipGraph, SyncGraph } from './graphTypes'

/**
 * Debug configuration for development tooling
 */
export interface DebugConfig {
  /** Enable timing measurement for concerns and listeners */
  timing?: boolean
  /** Threshold in milliseconds for slow operation warnings (default: 5ms) */
  timingThreshold?: number
  /** Enable tracking of processChanges calls and applied changes for testing/debugging */
  track?: boolean
}

/**
 * A single recorded processChanges invocation
 */
export interface DebugTrackEntry {
  /** Input changes passed to processChanges as [path, value, meta] tuples */
  input: [string, unknown, unknown][]
  /** Changes actually applied to state proxy */
  applied: { path: string; value: unknown }[]
  /** Changes applied to _concerns proxy */
  appliedConcerns: { path: string; value: unknown }[]
  /** Timestamp of the call */
  timestamp: number
}

/**
 * Debug tracking data exposed on StoreInstance when debug.track is enabled.
 * Provides an append-only log of all processChanges calls and their effects.
 */
export interface DebugTrack {
  /** All recorded processChanges calls (append-only) */
  calls: DebugTrackEntry[]
  /** Reset all tracking data */
  clear: () => void
}

export interface StoreConfig {
  /** Error storage path (default: "_errors") */
  errorStorePath?: string
  /** Max iterations for change processing (default: 100) */
  maxIterations?: number
  /** Debug configuration for development tooling */
  debug?: DebugConfig
  /** Use legacy TypeScript implementation instead of WASM (default: false) */
  useLegacyImplementation?: boolean
}

export interface ProviderProps<DATA extends object> {
  initialState: DATA
  children: ReactNode
}

export interface Aggregation {
  id?: string // Optional: for debugging only
  targetPath: string
  sourcePaths: string[]
}

/** Reacts to scoped changes - receives relative paths and scoped state. Only fires for NESTED paths, not the path itself. */
export type OnStateListener<
  DATA extends object = object,
  SUB_STATE = DATA,
  META extends GenericMeta = GenericMeta,
> = (
  changes: ArrayOfChanges<SUB_STATE, META>,
  state: SUB_STATE,
) => ArrayOfChanges<DATA, META> | undefined

/**
 * Listener registration with path (what to watch) and scope (how to present data)
 *
 * @example
 * ```typescript
 * // Watch user.profile.name, get full state
 * {
 *   path: 'user.profile.name',
 *   scope: null,
 *   fn: (changes, state) => {
 *     // changes: [['user.profile.name', 'Alice', {}]] - FULL path
 *     // state: full DATA object
 *   }
 * }
 *
 * // Watch user.profile.*, get scoped state
 * {
 *   path: 'user.profile',
 *   scope: 'user.profile',
 *   fn: (changes, state) => {
 *     // changes: [['name', 'Alice', {}]] - RELATIVE to scope
 *     // state: user.profile object
 *   }
 * }
 *
 * // Watch deep path, get parent scope
 * {
 *   path: 'p.123.g.abc.data.strike',
 *   scope: 'p.123.g.abc',
 *   fn: (changes, state) => {
 *     // changes: [['data.strike', value, {}]] - relative to scope
 *     // state: p.123.g.abc object
 *   }
 * }
 * ```
 */
export interface ListenerRegistration<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  /**
   * Path to watch - only changes under this path will trigger the listener
   * null = watch all top-level paths
   */
  path: DeepKey<DATA> | null

  /**
   * Scope for state and changes presentation
   * - If null: state is full DATA, changes use FULL paths
   * - If set: state is value at scope, changes use paths RELATIVE to scope
   *
   * Note: Changes are filtered based on `path`, even when scope is null
   */
  scope: DeepKey<DATA> | null

  fn: OnStateListener<DATA, any, META>
}

export interface ListenerHandlerRef {
  scope: string | null
  fn: (...args: unknown[]) => unknown
}

export interface SideEffectGraphs<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  sync: SyncGraph
  flip: FlipGraph
  listeners: Map<string, ListenerRegistration<DATA, META>[]>
  sortedListenerPaths: string[]
  /** O(1) lookup: subscriber_id -> handler ref. Populated by registerListener. */
  listenerHandlers: Map<number, ListenerHandlerRef>
}

export interface Registrations {
  concerns: Map<string, ConcernType[]>
  effectCleanups: Set<() => void>
  sideEffectCleanups: Map<string, () => void>
  aggregations: Map<string, Aggregation[]>
}

export interface ProcessingState<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  queue: ArrayOfChanges<DATA, META>
}

/** Internal store state (NOT tracked - wrapped in ref()) */
export interface InternalState<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  graphs: SideEffectGraphs<DATA, META>
  registrations: Registrations
  processing: ProcessingState<DATA, META>
  timing: Timing
  config: DeepRequired<StoreConfig>
}

export type ConcernValues = Record<string, Record<string, unknown>>

/** Two-proxy pattern: state and _concerns are independent to prevent infinite loops */
export interface StoreInstance<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
> {
  state: DATA
  _concerns: ConcernValues
  _internal: InternalState<DATA, META>
  /** Debug tracking data, only populated when debug.track is enabled */
  _debug: DebugTrack | null
}
