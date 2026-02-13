/**
 * Pipeline Processor Types
 */

import type { ArrayOfChanges__internal } from '../../types/changes'

// =============================================================================
// Listener Types
// =============================================================================

export type ListenerFn = (
  changes: ArrayOfChanges__internal,
  state: unknown,
) => ArrayOfChanges__internal | undefined

/** Pre-computed routing edge from a group to a downstream group */
export interface ListenerRoute {
  target: string
  prefix: string
  prefixLen: number
  depth: number
}

/** Listener node metadata */
export interface ListenerNode {
  scope: string | null
  groupPath: string
}

/** Group-level metadata for change filtering */
export interface GroupMeta {
  prefix: string
  prefixLen: number
  depth: number
}

/**
 * Pre-computed listener routing structure.
 *
 * Groups are keyed by listener path. A group at path `P` matches any change
 * whose path starts with `P.` (nested children). Groups are sorted deepest-first
 * so that the most specific listeners fire before broader ones.
 *
 * During processing, changes are routed to groups via an ancestor walk —
 * only groups on the change's path are checked (O(1) Map lookup per ancestor level).
 * Groups on unrelated subtrees ("cold groups") are never touched.
 *
 * @example
 * ```typescript
 * // After registering three listeners:
 * //   registerListener(store, { path: 'app.users.u1.profile', scope: 'app.users.u1.profile', fn: validateProfile })
 * //   registerListener(store, { path: 'app.users',            scope: 'app.users',            fn: auditUsers })
 * //   registerListener(store, { path: null,                    scope: null,                    fn: rootLogger })
 * //
 * // The graph looks like:
 * {
 *   order: ['app.users.u1.profile', 'app.users', ''],
 *   //       depth 4 (first)         depth 2      depth 0 (last)
 *
 *   groupMeta: Map {
 *     'app.users.u1.profile' → { prefix: 'app.users.u1.profile.', prefixLen: 25, depth: 4 },
 *     'app.users'            → { prefix: 'app.users.',            prefixLen: 10, depth: 2 },
 *     ''                     → { prefix: '',                      prefixLen: 0,  depth: 0 },
 *   },
 *
 *   groupMembers: Map {
 *     'app.users.u1.profile' → ['app.users.u1.profile_validateProfile'],
 *     'app.users'            → ['app.users_auditUsers'],
 *     ''                     → ['__rootLogger'],
 *   },
 *
 *   nodes: Map {
 *     'app.users.u1.profile_validateProfile' → { scope: 'app.users.u1.profile', groupPath: 'app.users.u1.profile' },
 *     'app.users_auditUsers'                 → { scope: 'app.users',            groupPath: 'app.users' },
 *     '__rootLogger'                         → { scope: null,                    groupPath: '' },
 *   },
 *
 *   edges: Map {
 *     // Each group points to ALL downstream (shallower) groups
 *     'app.users.u1.profile' → [
 *       { target: 'app.users', prefix: 'app.users.', prefixLen: 10, depth: 2 },
 *       { target: '',          prefix: '',            prefixLen: 0,  depth: 0 },
 *     ],
 *     'app.users' → [
 *       { target: '', prefix: '', prefixLen: 0, depth: 0 },
 *     ],
 *     '' → [],  // root has no downstream
 *   },
 *
 *   fns: Map {
 *     'app.users.u1.profile_validateProfile' → [wrapped validateProfile fn],
 *     'app.users_auditUsers'                 → [wrapped auditUsers fn],
 *     '__rootLogger'                         → [wrapped rootLogger fn],
 *   },
 * }
 *
 * // Processing change ['app.users.u1.profile.name', 'Alice', {}]:
 * //
 * // 1. Seed (ancestor walk):
 * //    'app.users.u1.profile' → pending: [['name', 'Alice', {}]]
 * //    'app.users.u1'         → no group (skipped)
 * //    'app.users'            → pending: [['u1.profile.name', 'Alice', {}]]
 * //    'app'                  → no group (skipped)
 * //    root ''                → skipped (change has dots, not top-level)
 * //
 * // 2. Iterate deepest-first:
 * //    'app.users.u1.profile' → validateProfile fires with ['name', 'Alice', {}]
 * //    'app.users'            → auditUsers fires with ['u1.profile.name', 'Alice', {}]
 * //    ''                     → pending empty, rootLogger does NOT fire (cold)
 * ```
 */
export interface ListenerGraph {
  /** Execution order: group paths sorted deepest-first */
  order: string[]

  /** Group path → filtering/routing metadata */
  groupMeta: Map<string, GroupMeta>

  /** Group path → ordered listener IDs */
  groupMembers: Map<string, string[]>

  /** Listener ID → node metadata */
  nodes: Map<string, ListenerNode>

  /** Group path → pre-computed downstream routes */
  edges: Map<string, ListenerRoute[]>

  /** Listener ID → wrapped function */
  fns: Map<string, ListenerFn>
}
