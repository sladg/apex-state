/**
 * Internal Core Types
 *
 * Types used only within the library internals (pipeline, side effects, store setup).
 * Not part of the public API.
 */

import type { ConcernType } from '~/concerns/types'
import type { Aggregation, StoreConfig } from '~/core/types'
import type { DeepRequired, GenericMeta } from '~/types'
import type { TopicRouter } from '~/utils/topicRouter'

import type { ChangeTuple } from '../types/changes'
import type { Timing } from '../utils/timing'
import type { FlipGraph, SyncGraph } from './graphTypes'

/**
 * Internal listener registration with plain strings.
 * Structurally compatible with ListenerRegistration<DATA, META> to allow
 * passing type-safe registrations to internal functions without casting.
 *
 * Key design:
 * - path: string | null (accepts both string literals from DeepKey and plain strings)
 * - scope: optional string | null | undefined (accepts all scope variants)
 * - fn: (...args: any[]) => any (accepts any function signature)
 *
 * This allows:
 * ```ts
 * const typeSafe: ListenerRegistration<DATA, META> = { ... }
 * const internal: ListenerRegistrationInternal = typeSafe // ✓ works
 * ```
 */
export interface ListenerRegistrationInternal<
  _DATA extends object = object,
  _META extends GenericMeta = GenericMeta,
> {
  path: string | null
  scope?: string | null | undefined
  fn: (...args: any[]) => any
}

export interface SideEffectGraphs<
  _DATA extends object = object,
  _META extends GenericMeta = GenericMeta,
> {
  sync: SyncGraph
  flip: FlipGraph
  topicRouter: TopicRouter
}

export interface Registrations {
  concerns: Map<string, ConcernType[]>
  effectCleanups: Set<() => void>
  sideEffectCleanups: Map<string, () => void>
  aggregations: Map<string, Aggregation[]>
}

export interface ProcessingState {
  queue: ChangeTuple
}

/** Internal store state (NOT tracked - wrapped in ref()) */
export interface InternalState<
  DATA extends object = object,
  META extends GenericMeta = GenericMeta,
> {
  graphs: SideEffectGraphs<DATA, META>
  registrations: Registrations
  processing: ProcessingState
  timing: Timing
  config: DeepRequired<StoreConfig>
}

export type ConcernValues = Record<string, Record<string, unknown>>
