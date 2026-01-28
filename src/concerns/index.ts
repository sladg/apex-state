/**
 * Concerns system exports
 *
 * Reactive validation, conditional logic, and UI hints with automatic dependency tracking.
 */

export type { ConcernType, ConcernRegistration, BaseConcernProps, BoolLogic } from './types'
export { defaultConcerns, findConcern } from './registry'
export { registerConcernEffects } from './registration'

// Pre-built concerns
export * as prebuilts from './prebuilts'
export { prebuilts as defaultPrebuilts } from './prebuilts'
