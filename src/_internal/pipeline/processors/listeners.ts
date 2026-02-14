/**
 * Listener Processor
 *
 * Processes pre-computed topic router with routing optimization.
 * Topics are sorted deepest-first at registration time.
 * Changes from listeners are routed to downstream topics via pre-computed routes.
 */

/* eslint-disable sonarjs/elseif-without-else, sonarjs/cognitive-complexity */

import type { StoreInstance } from '~/core/types'
import type { GenericMeta } from '~/types'
import { dot } from '~/utils/dot'

import type { ChangeTuple } from '../../types/changes'

/**
 * Processes topic router by routing changes through pre-computed routes.
 *
 * Algorithm:
 * 1. **Seed** — Walk each change's path upward (via `lastIndexOf('.')`) to find
 *    matching topics. Only topics on the change's ancestor chain receive it.
 *    Cold topics (different subtree) are never touched. O(changes × path_depth).
 *
 * 2. **Iterate** — Process topics deepest-first (pre-sorted at registration).
 *    Each topic's pending changes are passed to its subscribers sequentially.
 *
 * 3. **Within-topic accumulation** — When a subscriber produces changes, they are
 *    relativized and appended to `topicRelevant` so subsequent subscribers in the
 *    same topic see them.
 *
 * 4. **Route dispatching** — Produced changes are routed to downstream (shallower)
 *    topics via pre-computed routes, relativized per target prefix.
 *
 * @param changes - Initial changes that triggered processing
 * @param store - Store instance containing the topic router and processing queue
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
 * // If depth-4 subscriber produces ['app.users.u1.age', 30, {}]:
 * //   → queued for applyBatch
 * //   → routed to 'app.users' via route: ['u1.age', 30, {}]
 * //   → NOT routed to root (has dots, not top-level)
 * ```
 */
export const processListeners = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  changes: ChangeTuple<DATA, META>,
  store: StoreInstance<DATA, META>,
  currentState: DATA,
): void => {
  const router = store._internal.graphs.topicRouter
  const { queue } = store._internal.processing

  // Seed: route each initial change to matching topics by walking up ancestors.
  // O(changes × path_depth) — cold topics are never touched.
  const pendingByTopic = new Map<string, ChangeTuple>()
  for (const topic of router.topics) pendingByTopic.set(topic, [])

  for (const change of changes) {
    const changePath = change[0]

    // Walk up ancestor paths by finding '.' positions from right to left
    let pos = changePath.length
    while (pos > 0) {
      pos = changePath.lastIndexOf('.', pos - 1)
      if (pos === -1) break
      const ancestor = changePath.slice(0, pos)
      const pending = pendingByTopic.get(ancestor)
      if (pending) {
        const meta = router.topicMeta.get(ancestor)!
        pending.push([changePath.slice(meta.prefixLen), change[1], change[2]])
      }
    }

    // Root topic: only top-level changes (no dots)
    if (!changePath.includes('.')) {
      const rootPending = pendingByTopic.get('')
      if (rootPending) {
        rootPending.push(change)
      }
    }
  }

  // Iterate topics deepest-first
  for (const topic of router.topics) {
    const meta = router.topicMeta.get(topic)!
    const subscriberIds = router.subscribers.get(topic)!
    const topicRoutes = router.routes.get(topic)!
    const pending = pendingByTopic.get(topic)!

    // Skip if no relevant changes
    if (pending.length === 0) continue

    // Use pending directly as the working set (within-topic accumulation via mutation)
    const topicRelevant = pending

    // Process each subscriber in the topic sequentially
    for (const id of subscriberIds) {
      const fn = router.handlers.get(id)
      if (!fn) continue

      const subscriber = router.subscriberMeta.get(id)!

      // Resolve scoped state
      const scopedState =
        subscriber.scope === null
          ? currentState
          : dot.get__unsafe(currentState, subscriber.scope)

      // Call subscriber with relative changes — returns absolute changes
      const produced = fn(topicRelevant, scopedState)
      if (!produced?.length) continue

      // Process produced changes
      for (const [path, value, changeMeta] of produced) {
        const enrichedMeta = { isListenerChange: true, ...changeMeta }

        // Push to queue for applyBatch
        queue.push([path, value, enrichedMeta])

        // Append relativized change to topicRelevant for within-topic accumulation
        if (meta.depth === 0 && !path.includes('.')) {
          // Root topic: change is already relative (top-level)
          topicRelevant.push([path, value, enrichedMeta])
        } else if (path.startsWith(meta.prefix)) {
          // Non-root: relativize for within-topic visibility
          topicRelevant.push([path.slice(meta.prefixLen), value, enrichedMeta])
        }
        // else: Change is outside this topic's scope — skip within-topic accumulation

        // ROUTE: Send to downstream topics via pre-computed routes
        for (const route of topicRoutes) {
          if (route.depth === 0 && !path.includes('.')) {
            pendingByTopic.get(route.target)!.push([path, value, enrichedMeta])
          } else if (path.startsWith(route.prefix)) {
            pendingByTopic
              .get(route.target)!
              .push([path.slice(route.prefixLen), value, enrichedMeta])
          }
        }
      }
    }
  }
}
