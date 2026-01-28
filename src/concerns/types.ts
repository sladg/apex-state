/**
 * Type definitions for the concerns system
 *
 * Concerns provide reactive validation, conditional logic, and UI hints
 * that automatically track dependencies through valtio-reactive.
 */

import type { DeepKey, DeepValue } from '../types'

/**
 * Base properties passed to all concern evaluators
 */
export type BaseConcernProps<STATE, PATH extends string> = {
  /** The full state proxy (for cross-field access) */
  state: STATE
  /** The path being evaluated */
  path: PATH
  /** The value at the path */
  value: any
}

/**
 * Base concern type definition
 *
 * EXTRA_PROPS: Additional properties specific to this concern (e.g., { schema: ZodSchema })
 * RETURN_TYPE: The type returned by evaluate() (e.g., boolean, string)
 */
export type ConcernType<EXTRA_PROPS = Record<string, any>, RETURN_TYPE = any> = {
  /** Unique name for this concern */
  name: string
  /** Human-readable description */
  description: string
  /**
   * Evaluation function - runs inside effect() for automatic dependency tracking
   * Any state properties accessed here will be tracked automatically
   */
  evaluate: (props: BaseConcernProps<any, string> & EXTRA_PROPS) => RETURN_TYPE
}

/**
 * Registration entry for a concern at a specific path
 */
export type ConcernRegistration = {
  /** Unique ID for this registration group */
  id: string
  /** The path this concern is registered at */
  path: string
  /** The concern name */
  concernName: string
  /** The concern definition */
  concern: ConcernType
  /** Configuration passed to evaluate() */
  config: any
  /** Cleanup function from effect() */
  dispose: () => void
}

/**
 * Boolean logic DSL for conditional concerns
 */
export type BoolLogic<STATE> =
  | { IS_EQUAL: [DeepKey<STATE>, any] }
  | { EXISTS: DeepKey<STATE> }
  | { IS_EMPTY: DeepKey<STATE> }
  | { AND: BoolLogic<STATE>[] }
  | { OR: BoolLogic<STATE>[] }
  | { NOT: BoolLogic<STATE> }
  | { GT: [DeepKey<STATE>, number] }
  | { LT: [DeepKey<STATE>, number] }
  | { GTE: [DeepKey<STATE>, number] }
  | { LTE: [DeepKey<STATE>, number] }
  | { IN: [DeepKey<STATE>, any[]] }
