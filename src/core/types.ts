/**
 * Core Store Types
 *
 * Foundational type definitions for the store instance.
 * These types are used throughout the library.
 */

import type { ReactNode } from 'react'

import type { ConcernType } from '../concerns/types'
import type { DevToolsNotifier } from '../store/devtools'
import type {
  ArrayOfChanges,
  DeepKey,
  DeepRequired,
  DefaultDepth,
  GenericMeta,
} from '../types'
import type { ApexLogger } from '../utils/log'
import type { WasmPipeline } from '../wasm/bridge'

/**
 * Debug configuration for development tooling
 */
export interface DebugConfig {
  /** Enable console logging for pipeline runs and registrations */
  log?: boolean
  /** Enable timing warnings for slow listeners */
  timing?: boolean
  /** Threshold in milliseconds for slow listener warnings (default: 5ms) */
  timingThreshold?: number
  /** Enable tracking of processChanges calls and applied changes for testing/debugging */
  track?: boolean
  /** Connect to Redux DevTools Extension for state inspection */
  devtools?: boolean
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
  /** Human-readable store name for DevTools (default: "store"). Auto-incremented ID is appended. */
  name?: string
  /** Error storage path (default: "_errors") */
  errorStorePath?: string
  /** Max iterations for change processing (default: 100) */
  maxIterations?: number
  /** Debug configuration for development tooling */
  debug?: DebugConfig
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

/** Reacts to scoped changes - receives relative paths and scoped state. Only fires for NESTED paths, not the path itself.
 * Both input changes and return changes use paths relative to scope (or full paths when scope is null).
 *
 * When SUB_STATE is `any` (as in ListenerRegistration's default), the return type falls back to
 * ArrayOfChanges<DATA> to avoid DeepKey<any> resolving to never.
 */
type IsAnyState<T> = 0 extends 1 & T ? true : false

export type OnStateListener<
  DATA extends object = object,
  SUB_STATE = DATA,
  META extends GenericMeta = GenericMeta,
> = (
  changes: ArrayOfChanges<SUB_STATE, META>,
  state: SUB_STATE,
) => IsAnyState<SUB_STATE> extends true
  ? ArrayOfChanges<DATA, META> | undefined
  : ArrayOfChanges<SUB_STATE, META> | undefined

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
 *
 * // Watch ALL changes (always fires), get full state
 * {
 *   path: null,
 *   scope: null,
 *   fn: (changes, state) => {
 *     // changes: [['user.profile.name', 'Alice', {}], ['count', 5, {}]] - FULL paths, all depths
 *     // state: full DATA object
 *   }
 * }
 * ```
 */
export interface ListenerRegistration<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
  Depth extends number = DefaultDepth,
> {
  /**
   * Path to watch - only changes under this path will trigger the listener
   * null = watch all paths (receives every change)
   */
  path: DeepKey<DATA, Depth> | null

  /**
   * Scope for state and changes presentation
   * - If omitted/undefined: defaults to `path` (scoped state matching the watched path)
   * - If null: state is full DATA, changes use FULL paths
   * - If set: state is value at scope, changes use paths RELATIVE to scope
   *
   * Note: Changes are filtered based on `path`, even when scope is null
   */
  scope?: DeepKey<DATA, Depth> | null

  fn: OnStateListener<DATA, any, META>
}

export interface ListenerHandlerRef {
  scope: string | null
  fn: (...args: any[]) => unknown
  name: string
}

export interface Registrations {
  concerns: Map<string, ConcernType[]>
  effectCleanups: Set<() => void>
  sideEffectCleanups: Map<string, () => void>
  /** O(1) lookup: subscriber_id -> handler ref. Populated by registerListener. */
  listenerHandlers: Map<number, ListenerHandlerRef>
}

/** Internal store state (NOT tracked - wrapped in ref()) */
export interface InternalState {
  registrations: Registrations
  config: DeepRequired<StoreConfig>
  logger: ApexLogger
  /** DevTools — pipeline notifier + proxy inspection. Null when devtools disabled. */
  devtools: DevToolsNotifier | null
  /** Per-store WASM pipeline instance (null after destroy). */
  pipeline: WasmPipeline | null
}

export type ConcernValues = Record<string, Record<string, unknown>>

/** Two-proxy pattern: state and _concerns are independent to prevent infinite loops */
export interface StoreInstance<DATA extends object> {
  state: DATA
  _concerns: ConcernValues
  _internal: InternalState
  /** Debug tracking data, only populated when debug.track is enabled */
  _debug: DebugTrack | null
}
