/**
 * Strongly-Typed Graph Definitions
 *
 * Each graph in the store has specific node and edge attributes.
 * These type definitions provide compile-time safety for graph operations.
 */

import type Graph from 'graphology'

/** Type representing no attributes */
type Empty = Record<string, never>

/**
 * Sync graph: Undirected graph of paths that must have synchronized values
 *
 * Nodes: path identifiers (strings)
 * Node attributes: none
 * Edge attributes: { key: string } for deduplication
 */
export type SyncGraph = Graph<
  Empty, // no node attributes
  { key: string }, // edges have a key
  Empty // no graph attributes
>

/**
 * Flip graph: Undirected graph of paths with inverted boolean values
 *
 * Nodes: path identifiers (strings)
 * Node attributes: none
 * Edge attributes: { key: string } for consistency with sync graph
 */
export type FlipGraph = Graph<
  Empty, // no node attributes
  { key: string }, // edges have a key
  Empty // no graph attributes
>
