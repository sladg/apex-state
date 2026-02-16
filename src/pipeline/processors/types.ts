/**
 * Pipeline Processor Types
 */

import type { ArrayOfChanges, GenericMeta } from '../../types'
import type { AnyChange } from '../normalize-changes.types'

// =============================================================================
// Listener Types
// =============================================================================

type ListenerFn = (
  changes: AnyChange[],
  state: unknown,
) => AnyChange[] | undefined

export interface ListenerRegistrationInternal {
  scope: string | null
  fn: ListenerFn
}

export interface ProcessListenerArgs<
  DATA extends object,
  META extends GenericMeta,
> {
  registration: ListenerRegistrationInternal
  relevantChanges: AnyChange[]
  currentState: DATA
  queue: ArrayOfChanges<DATA, META>
}
