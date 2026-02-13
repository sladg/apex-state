/**
 * Types for Change Normalization
 */

import type { GenericMeta } from '../types'
import type { ArrayOfChanges__internal } from '../types/changes'

// =============================================================================
// Normalization Types
// =============================================================================

/**
 * Match mode for change normalization
 * - 'all': Match exact, parent, and child changes (for sync/flip paths)
 * - 'children-only': Only match child changes (for listeners)
 */
export type MatchMode = 'all' | 'children-only'

export interface NormalizeChangesGroupedArgs {
  changes: ArrayOfChanges__internal
  /** Groups of connected paths - each group is processed as a unit */
  pathGroups: string[][]
  /** Default: 'all' */
  matchMode?: MatchMode
}

/** Normalized change with relative path for applying to neighbors */
export interface NormalizedChange {
  /** Relative path suffix (null for exact/parent match, 'd.e' for child match) */
  relativePath: string | null
  value: unknown
  meta: GenericMeta
}

/** Result for grouped normalization */
interface GroupedChangeMatch {
  /** The registered path that matched */
  matchedPath: string
  /** Relative path to append to neighbor paths (null for exact/parent match) */
  relativePath: string | null
  value: unknown
  meta: GenericMeta
  /** All paths in this connected group */
  connectedPaths: string[]
}

export type NormalizedChangesGrouped = GroupedChangeMatch[]

export interface NormalizeChangeArgs {
  changePath: string
  changeValue: unknown
  changeMeta: GenericMeta
  registeredPath: string
  matchMode: MatchMode
}
