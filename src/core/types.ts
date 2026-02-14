/**
 * Core Store Types
 *
 * Foundational type definitions for the store instance.
 * These types are used throughout the library.
 */

import type { ReactNode } from 'react'

import type { ConcernValues, InternalState } from '../_internal'
import type { ArrayOfChanges, DeepKey, DeepValue, GenericMeta } from '../types'
import type { SideEffects } from '../types/sideEffects'

/**
 * Debug configuration for development tooling
 */
export interface DebugConfig {
  /** Enable timing measurement for concerns and listeners */
  timing?: boolean
  /** Threshold in milliseconds for slow operation warnings (default: 5ms) */
  timingThreshold?: number
}

export interface StoreConfig {
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
/** path + scope: fn receives scoped state and changes with paths relative to scope */
type PathWithScope<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> = {
  [SCOPE in DeepKey<DATA>]: {
    path: DeepKey<DATA>
    scope: SCOPE
    fn: OnStateListener<DATA, DeepValue<DATA, SCOPE>, META>
  }
}[DeepKey<DATA>]

/** path + null scope: fn receives full DATA and changes with full paths */
type PathWithNullScope<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> = {
  [PATH in DeepKey<DATA>]: {
    path: PATH
    scope: null
    fn: OnStateListener<DATA, DATA, META>
  }
}[DeepKey<DATA>]

/** path only (no scope): fn receives full DATA and changes with full paths */
type PathOnly<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> = {
  [PATH in DeepKey<DATA>]: {
    path: PATH
    scope?: undefined
    fn: OnStateListener<DATA, DATA, META>
  }
}[DeepKey<DATA>]

/** root listener: watches all top-level paths, fn receives full DATA */
interface RootListener<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  path: null
  scope?: undefined
  fn: OnStateListener<DATA, DATA, META>
}

/**
 * Type-safe listener registration. Scope determines fn signature:
 * - PathWithScope: path + scope → fn typed by scope's DeepValue
 * - PathWithNullScope: path + scope:null → fn gets full DATA
 * - PathOnly: path only → fn gets full DATA
 * - RootListener: path:null → fn gets full DATA
 */
export type ListenerRegistration<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> =
  | PathWithScope<DATA, META>
  | PathWithNullScope<DATA, META>
  | PathOnly<DATA, META>
  | RootListener<DATA, META>

/** Extract the Listener type from a store created by createGenericStore */
export type InferListener<T> = T extends {
  useSideEffects: (
    id: string,
    effects: SideEffects<infer DATA, infer META>,
  ) => void
}
  ? DATA extends object
    ? ListenerRegistration<DATA, META>
    : never
  : never

/** Two-proxy pattern: state and _concerns are independent to prevent infinite loops */
export interface StoreInstance<
  DATA extends object,
  META extends GenericMeta = GenericMeta,
> {
  state: DATA
  _concerns: ConcernValues
  _internal: InternalState<DATA, META>
}
