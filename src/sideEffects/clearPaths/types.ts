/**
 * Clear Paths type definitions
 *
 * Configuration types for the clear paths side-effect.
 */

import type { DeepKey } from '../../types'

/**
 * Configuration for a single clear path rule
 */
export interface ClearPathConfig<DATA> {
  /**
   * Unique identifier for this clear rule
   */
  id: string

  /**
   * Path that triggers the clear action
   */
  triggerPath: DeepKey<DATA>

  /**
   * Paths to clear (set to undefined) when trigger changes
   */
  clearPaths: DeepKey<DATA>[]

  /**
   * If true, nested changes within triggerPath also trigger clearing
   * If false, only direct changes to triggerPath trigger clearing
   * @default false
   */
  clearOnNested?: boolean
}
