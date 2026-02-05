/**
 * Graph Type Definitions
 *
 * Type aliases for the PathGroups data structure used in sync and flip operations.
 * These aliases maintain backward compatibility with the previous graphology-based API.
 */

import type { PathGroups } from './pathGroups'

/**
 * Sync graph: Paths that must have synchronized values
 * Type alias for PathGroups - maintains backward compatibility
 */
export type SyncGraph = PathGroups

/**
 * Flip graph: Paths with inverted boolean values
 * Type alias for PathGroups - maintains backward compatibility
 */
export type FlipGraph = PathGroups

// Re-export PathGroups type
export type { PathGroups } from './pathGroups'
