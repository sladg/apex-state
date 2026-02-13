/**
 * Listener Processor
 *
 * Processes pre-computed listener graph with routing optimization.
 * Groups are sorted deepest-first at registration time.
 * Changes from listeners are routed to downstream groups via pre-computed edges.
 */

/* eslint-disable sonarjs/elseif-without-else, sonarjs/cognitive-complexity */

import type { StoreInstance } from '../../core/types'
import type { GenericMeta } from '../../types'
import type { ArrayOfChanges__internal } from '../../types/changes'
import { dot } from '../../utils/dot'

/**
 * Processes listener graph by routing changes through pre-computed edges.
 *
 * Algorithm:
 * 1. **Seed** — Walk each change's path upward (via `lastIndexOf('.')`) to find
 *    matching groups. Only groups on the change's ancestor chain receive it.
 *    Cold groups (different subtree) are never touched. O(changes × path_depth).
 *
 * 2. **Iterate** — Process groups deepest-first (pre-sorted at registration).
 *    Each group's pending changes are passed to its listeners sequentially.
 *
 * 3. **Within-group accumulation** — When a listener produces changes, they are
 *    relativized and appended to `groupRelevant` so subsequent listeners in the
 *    same group see them.
 *
 * 4. **Edge routing** — Produced changes are routed to downstream (shallower)
 *    groups via pre-computed edges, relativized per target prefix.
 *
 * @param changes - Initial changes that triggered processing
 * @param store - Store instance containing the listener graph and processing queue
 * @param currentState - Current snapshot of the state tree
 *
 * @example
 * ```typescript
 * // Given listeners registered at:
 * //   'app.users.u1.profile' (depth 4)
 * //   'app.users'            (depth 2)
 * //   ''                     (root, depth 0)
 * //
 * // And a change: ['app.users.u1.profile.name', 'Alice', {}]
 * //
 * // Seeding routes the change to:
 * //   'app.users.u1.profile' → ['name', 'Alice', {}]        (relativized)
 * //   'app.users'            → ['u1.profile.name', 'Alice', {}]
 * //   root ('')              → skipped (change has dots, not top-level)
 * //
 * // Processing order: depth 4 → depth 2 → depth 0
 * // If depth-4 listener produces ['app.users.u1.age', 30, {}]:
 * //   → queued for applyBatch
 * //   → routed to 'app.users' via edge: ['u1.age', 30, {}]
 * //   → NOT routed to root (has dots, not top-level)
 * ```
 */
export const processListeners = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  changes: ArrayOfChanges__internal<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): void => {
  const graph = store._internal.graphs.listenerGraph
  const { queue } = store._internal.processing

  // Seed: route each initial change to matching groups by walking up ancestors.
  // O(changes × path_depth) — cold groups are never touched.
  const pendingByGroup = new Map<string, ArrayOfChanges__internal>()
  for (const groupPath of graph.order) pendingByGroup.set(groupPath, [])

  for (const change of changes) {
    const changePath = change[0]

    // Walk up ancestor paths by finding '.' positions from right to left
    let pos = changePath.length
    while (pos > 0) {
      pos = changePath.lastIndexOf('.', pos - 1)
      if (pos === -1) break
      const ancestor = changePath.slice(0, pos)
      const pending = pendingByGroup.get(ancestor)
      if (pending) {
        const meta = graph.groupMeta.get(ancestor)!
        pending.push([changePath.slice(meta.prefixLen), change[1], change[2]])
      }
    }

    // Root group: only top-level changes (no dots)
    if (!changePath.includes('.')) {
      const rootPending = pendingByGroup.get('')
      if (rootPending) {
        rootPending.push(change)
      }
    }
  }

  // Iterate groups deepest-first
  for (const groupPath of graph.order) {
    const meta = graph.groupMeta.get(groupPath)!
    const memberIds = graph.groupMembers.get(groupPath)!
    const edges = graph.edges.get(groupPath)!
    const pending = pendingByGroup.get(groupPath)!

    // Skip if no relevant changes
    if (pending.length === 0) continue

    // Use pending directly as the working set (within-group accumulation via mutation)
    const groupRelevant = pending

    // Process each entry in the group sequentially
    for (const id of memberIds) {
      const fn = graph.fns.get(id)
      if (!fn) continue

      const node = graph.nodes.get(id)!

      // Resolve scoped state
      const scopedState =
        node.scope === null
          ? currentState
          : dot.get__unsafe(currentState, node.scope)

      // Call listener with relative changes — returns absolute changes
      const produced = fn(groupRelevant, scopedState)
      if (!produced?.length) continue

      // Process produced changes
      for (const [path, value, changeMeta] of produced) {
        const enrichedMeta = { isListenerChange: true, ...changeMeta }

        // Push to queue for applyBatch
        queue.push([path, value, enrichedMeta])

        // Append relativized change to groupRelevant for within-group accumulation
        if (meta.depth === 0 && !path.includes('.')) {
          // Root group: change is already relative (top-level)
          groupRelevant.push([path, value, enrichedMeta])
        } else if (path.startsWith(meta.prefix)) {
          // Non-root: relativize for within-group visibility
          groupRelevant.push([path.slice(meta.prefixLen), value, enrichedMeta])
        }
        // else: Change is outside this group's scope — skip within-group accumulation

        // ROUTE: Send to downstream groups via pre-computed edges
        for (const edge of edges) {
          if (edge.depth === 0 && !path.includes('.')) {
            pendingByGroup.get(edge.target)!.push([path, value, enrichedMeta])
          } else if (path.startsWith(edge.prefix)) {
            pendingByGroup
              .get(edge.target)!
              .push([path.slice(edge.prefixLen), value, enrichedMeta])
          }
        }
      }
    }
  }
}
