/**
 * Graph Type Definitions
 *
 * Type aliases for the Graph data structure used in sync and flip operations.
 * These aliases maintain backward compatibility with the previous graphology-based API.
 */

import type { Graph } from '~/utils/graph'

/**
 * Sync graph: Paths that must have synchronized values
 * Type alias for Graph - maintains backward compatibility
 */
export type SyncGraph = Graph

/**
 * Flip graph: Paths with inverted boolean values
 * Type alias for Graph - maintains backward compatibility
 */
export type FlipGraph = Graph

// Re-export Graph type
export type { Graph } from '~/utils/graph'
