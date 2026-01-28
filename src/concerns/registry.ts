/**
 * Concern registry utilities
 *
 * Utilities for looking up and managing concerns.
 */

import type { ConcernType } from './types'
import { prebuilts } from './prebuilts'

/**
 * Concern lookup by name
 *
 * @param name The concern name to look up
 * @param concerns Optional array of concerns to search (defaults to prebuilts)
 * @returns The concern definition, or undefined if not found
 */
export const findConcern = (
  name: string,
  concerns: readonly any[] = prebuilts
): ConcernType | undefined => {
  return concerns.find(c => c.name === name) as ConcernType | undefined
}

/**
 * Default concerns provided by apex-state
 */
export const defaultConcerns = prebuilts
