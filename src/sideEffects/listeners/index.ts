/**
 * Listeners Side-Effect Module
 *
 * Public exports for state change listener functionality.
 */

export { createListenersRegistry } from './registry'
export type { ListenersRegistry } from './registry'
export { breakdownChanges } from './breakdown'
export type {
  OnStateChangesListenerFunction,
  RegisteredListener,
} from './types'
