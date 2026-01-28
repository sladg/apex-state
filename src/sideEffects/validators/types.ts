/**
 * Types for Zod validation side-effect
 *
 * Supports multiple validators per path with error storage in state.
 */

import type { DeepKey } from '../../types'
import type { z } from 'zod'

/**
 * Configuration for a single validator
 */
export interface ValidatorConfig<DATA> {
  /**
   * Unique identifier for this validator
   */
  id: string

  /**
   * Path to validate (null = validate entire state)
   */
  scope: DeepKey<DATA> | null

  /**
   * Zod schema to validate against
   */
  schema: z.ZodSchema

  /**
   * Path where errors should be stored (relative to error store root)
   * Can be any string path, not limited to existing data paths
   */
  errorPath: DeepKey<DATA> | string
}

/**
 * Error stored in state for a specific validator
 */
export interface StoredError {
  /**
   * Validator ID that produced this error
   */
  id: string

  /**
   * Error message from Zod validation
   */
  message: string
}

/**
 * Type for error storage structure
 * Maps paths to arrays of errors from different validators
 */
export type ErrorStore<DATA> = {
  [K in DeepKey<DATA>]?: StoredError[]
}
