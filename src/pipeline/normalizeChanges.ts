/**
 * Change Normalization Helper
 *
 * Normalizes ArrayOfChanges for registered paths by handling:
 * - Exact match: change path equals registered path
 * - Parent change: change path is ancestor of registered path (extracts nested value)
 * - Child change: change path is descendant of registered path (preserves relative path)
 */

import _get from 'lodash/get'

import type {
  NormalizeChangeArgs,
  NormalizeChangesArgs,
  NormalizeChangesGroupedArgs,
  NormalizedChange,
  NormalizedChangesGrouped,
  NormalizedChangesMap,
} from './normalizeChanges.types'

export type {
  AnyChange,
  GroupedChangeMatch,
  MatchMode,
  NormalizeChangesArgs,
  NormalizeChangesGroupedArgs,
  NormalizedChange,
  NormalizedChangesGrouped,
  NormalizedChangesMap,
} from './normalizeChanges.types'

// =============================================================================
// Core normalization logic
// =============================================================================

/**
 * Normalize a single change for a single registered path
 * Returns null if the change is not relevant to the registered path
 */
const normalizeChange = (
  props: NormalizeChangeArgs,
): NormalizedChange | null => {
  const { changePath, changeValue, changeMeta, registeredPath, matchMode } =
    props

  // Case 1: Exact match (skipped in 'children-only' mode)
  // e.g., changePath='a.b.c', registeredPath='a.b.c'
  if (matchMode === 'all' && changePath === registeredPath) {
    return { relativePath: null, value: changeValue, meta: changeMeta }
  }

  // Case 2: Parent change (skipped in 'children-only' mode)
  // e.g., changePath='a.b', registeredPath='a.b.c.d'
  // Extract nested value, relativePath is empty (applies to base neighbor path)
  if (matchMode === 'all' && registeredPath.startsWith(changePath + '.')) {
    if (changeValue === null || typeof changeValue !== 'object') {
      return null
    }

    // Extract nested value: 'a.b.c.d' - 'a.b.' = 'c.d'
    const nestedPath = registeredPath.slice(changePath.length + 1)
    const extractedValue = _get(changeValue, nestedPath)

    if (extractedValue === undefined) {
      return null
    }

    return { relativePath: null, value: extractedValue, meta: changeMeta }
  }

  // Case 3: Child change (always matched in both modes)
  // e.g., changePath='a.b.c.d.e', registeredPath='a.b.c'
  // Preserve relative path to append to neighbor paths
  if (changePath.startsWith(registeredPath + '.')) {
    // Relative path: 'a.b.c.d.e' - 'a.b.c.' = 'd.e'
    const relativePath = changePath.slice(registeredPath.length + 1)
    return { relativePath, value: changeValue, meta: changeMeta }
  }

  // No match
  return null
}

// =============================================================================
// Public API
// =============================================================================

/**
 * Normalize changes for a set of registered paths
 *
 * Returns normalized changes with relative paths that can be applied to neighbor paths.
 *
 * @example
 * // matchMode: 'all' (default) - exact, parent, and child matches
 * changes: [['user.name', 'Alice', {}]]
 * registeredPaths: ['user.name']
 * result: Map {
 *   'user.name' => [{ relativePath: null, value: 'Alice', meta: {} }]
 * }
 *
 * @example
 * // matchMode: 'children-only' - only child changes (for listeners)
 * changes: [
 *   ['user.profile', 'ignored', {}],           // exact match - SKIPPED
 *   ['user.profile.name', 'Alice', {}],        // child match - INCLUDED
 *   ['user.profile.settings.theme', 'dark', {}] // child match - INCLUDED
 * ]
 * registeredPaths: ['user.profile']
 * matchMode: 'children-only'
 * result: Map {
 *   'user.profile' => [
 *     { relativePath: 'name', value: 'Alice', meta: {} },
 *     { relativePath: 'settings.theme', value: 'dark', meta: {} }
 *   ]
 * }
 *
 * @example
 * // Parent change with matchMode: 'all'
 * changes: [['user', { name: 'Alice', profile: { age: 30 } }, {}]]
 * registeredPaths: ['user.name', 'user.profile.age']
 * result: Map {
 *   'user.name' => [{ relativePath: null, value: 'Alice', meta: {} }],
 *   'user.profile.age' => [{ relativePath: null, value: 30, meta: {} }]
 * }
 */
export const normalizeChangesForPaths = (
  props: NormalizeChangesArgs,
): NormalizedChangesMap => {
  const result: NormalizedChangesMap = new Map()
  const matchMode = props.matchMode ?? 'all'

  // Initialize empty arrays for all registered paths
  for (const registeredPath of props.registeredPaths) {
    result.set(registeredPath, [])
  }

  for (const change of props.changes) {
    const [changePath, changeValue, changeMeta] = change

    for (const registeredPath of props.registeredPaths) {
      const normalized = normalizeChange({
        changePath,
        changeValue,
        changeMeta,
        registeredPath,
        matchMode,
      })

      if (normalized) {
        result.get(registeredPath)!.push(normalized)
      }
    }
  }

  return result
}

/**
 * Normalize changes for grouped/connected paths (e.g., sync path components)
 *
 * When a change matches ANY path in a group, it's recorded once for the whole group.
 * The relativePath can be used to apply the change to all neighbor paths.
 *
 * @example
 * // Sync paths: ['a.b.c', 'path.synced', 'wow.path'] are connected
 * // Exact match on one path
 * changes: [['a.b.c', 'newValue', {}]]
 * pathGroups: [['a.b.c', 'path.synced', 'wow.path']]
 * result: [{
 *   matchedPath: 'a.b.c',
 *   relativePath: null,
 *   value: 'newValue',
 *   meta: {},
 *   connectedPaths: ['a.b.c', 'path.synced', 'wow.path']
 * }]
 * // Apply: 'path.synced' = 'newValue', 'wow.path' = 'newValue'
 *
 * @example
 * // Child change - relativePath is preserved
 * changes: [['a.b.c.deep.nested', 42, {}]]
 * pathGroups: [['a.b.c', 'path.synced', 'wow.path']]
 * result: [{
 *   matchedPath: 'a.b.c',
 *   relativePath: 'deep.nested',
 *   value: 42,
 *   meta: {},
 *   connectedPaths: ['a.b.c', 'path.synced', 'wow.path']
 * }]
 * // Apply: 'path.synced.deep.nested' = 42, 'wow.path.deep.nested' = 42
 */
export const normalizeChangesForGroups = (
  props: NormalizeChangesGroupedArgs,
): NormalizedChangesGrouped => {
  const result: NormalizedChangesGrouped = []
  const matchMode = props.matchMode ?? 'all'

  for (const change of props.changes) {
    const [changePath, changeValue, changeMeta] = change

    for (const group of props.pathGroups) {
      // Find first matching path in this group
      let match: { path: string; normalized: NormalizedChange } | null = null

      for (const registeredPath of group) {
        const normalized = normalizeChange({
          changePath,
          changeValue,
          changeMeta,
          registeredPath,
          matchMode,
        })

        if (normalized) {
          match = { path: registeredPath, normalized }
          break // Stop at first match in group
        }
      }

      // If any path in group matched, record it once for the whole group
      if (match) {
        result.push({
          matchedPath: match.path,
          relativePath: match.normalized.relativePath,
          value: match.normalized.value,
          meta: match.normalized.meta,
          connectedPaths: group,
        })
      }
    }
  }

  return result
}
