/**
 * Type definitions for the concerns system
 *
 * Concerns provide reactive validation, conditional logic, and UI hints
 * that automatically track dependencies through valtio-reactive.
 */

// Re-export BoolLogic types for backwards compatibility
export type { BoolLogic, ComparableValue } from '../types'

/**
 * Base properties passed to all concern evaluators
 */
export interface BaseConcernProps<STATE, PATH extends string> {
  /** The full state proxy (for cross-field access) */
  state: STATE
  /** The path being evaluated */
  path: PATH
  /** The value at the path */
  value: unknown
}

/**
 * Base concern type definition
 *
 * EXTRA_PROPS: Additional properties specific to this concern (e.g., { schema: ZodSchema })
 * RETURN_TYPE: The type returned by evaluate() (e.g., boolean, string)
 */
export interface ConcernType<
  EXTRA_PROPS = Record<string, unknown>,
  RETURN_TYPE = unknown,
> {
  /** Unique name for this concern */
  name: string
  /** Human-readable description */
  description: string
  /**
   * Evaluation function - runs inside effect() for automatic dependency tracking
   * Any state properties accessed here will be tracked automatically
   */
  evaluate: (
    props: BaseConcernProps<Record<string, unknown>, string> & EXTRA_PROPS,
  ) => RETURN_TYPE
}

/**
 * Registration entry for a concern at a specific path
 */
export interface ConcernRegistration {
  /** Unique ID for this registration group */
  id: string
  /** The path this concern is registered at */
  path: string
  /** The concern name */
  concernName: string
  /** The concern definition */
  concern: ConcernType
  /** Configuration passed to evaluate() */
  config: Record<string, unknown>
  /** Cleanup function from effect() */
  dispose: () => void
}
