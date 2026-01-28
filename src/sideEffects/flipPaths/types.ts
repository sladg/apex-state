/**
 * Types for flip paths side-effect
 *
 * Flip paths keep two paths with opposite boolean/enum values synchronized.
 */

import type { DeepKey } from '../../types'

/**
 * Configuration for a single flip path pair
 */
export interface FlipPathPair<DATA> {
  id: string
  path1: DeepKey<DATA>
  path2: DeepKey<DATA>
}

/**
 * Configuration for flip paths side-effect
 */
export interface FlipPathConfig<DATA> {
  pairs: Array<FlipPathPair<DATA>>
}
