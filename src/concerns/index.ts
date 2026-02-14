/**
 * Concerns system exports
 *
 * Reactive validation, conditional logic, and UI hints with automatic dependency tracking.
 */

export { defaultConcerns, findConcern } from './registry'
export type {
  BaseConcernProps,
  BoolLogic,
  ConcernRegistration,
  ConcernType,
} from './types'

// Pre-built concerns
export * as prebuilts from './prebuilts'
