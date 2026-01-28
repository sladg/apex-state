/**
 * Listener types for state change subscriptions
 *
 * Defines listener function signatures for global and scoped listeners.
 * Listeners receive state changes and current state snapshots.
 */

import type { DeepKey, DeepValue, ArrayOfChanges, GenericMeta } from '../../types'

/**
 * Listener function for state changes.
 *
 * If key is null: receives all changes and full state (global listener)
 * If key is a path: receives changes for that scope and scoped state
 *
 * @example
 * ```typescript
 * // Global listener
 * const globalListener: OnStateChangesListenerFunction<AppState, Meta, null> = {
 *   key: null,
 *   fn: (changes, state) => {
 *     console.log('All changes:', changes)
 *     console.log('Full state:', state)
 *   }
 * }
 *
 * // Scoped listener
 * const userListener: OnStateChangesListenerFunction<AppState, Meta, 'user'> = {
 *   key: 'user',
 *   fn: (changes, userState) => {
 *     console.log('User changes:', changes)
 *     console.log('User state:', userState)
 *   }
 * }
 * ```
 */
export type OnStateChangesListenerFunction<
  DATA extends object,
  META extends GenericMeta,
  Key extends DeepKey<DATA> | null = null
> = {
  key: Key
  fn: Key extends null
    ? (changes: ArrayOfChanges<DATA, META>, currentState: DATA) => void
    : Key extends DeepKey<DATA>
      ? (
          changes: ArrayOfChanges<DeepValue<DATA, Key>, META>,
          currentState: DeepValue<DATA, Key>
        ) => void
      : never
}

/**
 * Internal listener storage with ID tracking
 */
export interface RegisteredListener<
  DATA extends object,
  META extends GenericMeta
> {
  id: string
  listener: OnStateChangesListenerFunction<DATA, META, any>
}
