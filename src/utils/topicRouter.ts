/**
 * TopicRouter Data Structure
 *
 * Pre-computed topic-based routing structure for listener change dispatch.
 * Topics are keyed by listener path. A topic at path `P` matches any change
 * whose path starts with `P.` (nested children). Topics are sorted deepest-first
 * so that the most specific listeners fire before broader ones.
 *
 * Operations:
 * - createTopicRouter(): O(1) — empty router
 * - addTopic(router, path): O(T log T) — insert + re-sort + recompute routes
 * - removeTopic(router, path): O(T) — remove + recompute routes
 * - addSubscriber(router, id, scope, topic): O(1)
 * - removeSubscriber(router, id, topic): O(S) where S = subscribers in topic
 */

import type { GenericMeta } from '~/types/meta'

// =============================================================================
// Types
// =============================================================================

/** Internal change tuple: [path, value, metadata][] */
type ChangeTuple = [string, unknown, GenericMeta][]

export type HandlerFn = (
  changes: ChangeTuple,
  state: unknown,
) => ChangeTuple | undefined

/** Pre-computed routing edge from a topic to a downstream topic */
export interface Route {
  target: string
  prefix: string
  prefixLen: number
  depth: number
}

/** Subscriber metadata */
export interface SubscriberMeta {
  scope: string | null
  topic: string
}

/** Topic-level metadata for change filtering */
export interface TopicMeta {
  prefix: string
  prefixLen: number
  depth: number
}

/**
 * Pre-computed topic-based routing structure.
 *
 * During processing, changes are routed to topics via an ancestor walk —
 * only topics on the change's path are checked (O(1) Map lookup per ancestor level).
 * Topics on unrelated subtrees ("cold topics") are never touched.
 *
 * @example
 * ```typescript
 * // After registering three listeners:
 * //   registerListener(store, { path: 'app.users.u1.profile', scope: 'app.users.u1.profile', fn: validateProfile })
 * //   registerListener(store, { path: 'app.users',            scope: 'app.users',            fn: auditUsers })
 * //   registerListener(store, { path: null,                    scope: null,                    fn: rootLogger })
 * //
 * // The router looks like:
 * {
 *   topics: ['app.users.u1.profile', 'app.users', ''],
 *   //       depth 4 (first)         depth 2      depth 0 (last)
 *
 *   topicMeta: Map {
 *     'app.users.u1.profile' → { prefix: 'app.users.u1.profile.', prefixLen: 25, depth: 4 },
 *     'app.users'            → { prefix: 'app.users.',            prefixLen: 10, depth: 2 },
 *     ''                     → { prefix: '',                      prefixLen: 0,  depth: 0 },
 *   },
 *
 *   subscribers: Map {
 *     'app.users.u1.profile' → ['app.users.u1.profile_validateProfile'],
 *     'app.users'            → ['app.users_auditUsers'],
 *     ''                     → ['__rootLogger'],
 *   },
 *
 *   subscriberMeta: Map {
 *     'app.users.u1.profile_validateProfile' → { scope: 'app.users.u1.profile', topic: 'app.users.u1.profile' },
 *     'app.users_auditUsers'                 → { scope: 'app.users',            topic: 'app.users' },
 *     '__rootLogger'                         → { scope: null,                    topic: '' },
 *   },
 *
 *   routes: Map {
 *     // Each topic points to ALL downstream (shallower) topics
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
 *   handlers: Map {
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
 * //    'app.users.u1'         → no topic (skipped)
 * //    'app.users'            → pending: [['u1.profile.name', 'Alice', {}]]
 * //    'app'                  → no topic (skipped)
 * //    root ''                → skipped (change has dots, not top-level)
 * //
 * // 2. Iterate deepest-first:
 * //    'app.users.u1.profile' → validateProfile fires with ['name', 'Alice', {}]
 * //    'app.users'            → auditUsers fires with ['u1.profile.name', 'Alice', {}]
 * //    ''                     → pending empty, rootLogger does NOT fire (cold)
 * ```
 */
export interface TopicRouter {
  /** Execution order: topic paths sorted deepest-first */
  topics: string[]

  /** Topic path → filtering/routing metadata */
  topicMeta: Map<string, TopicMeta>

  /** Topic path → ordered subscriber IDs */
  subscribers: Map<string, string[]>

  /** Subscriber ID → subscriber metadata */
  subscriberMeta: Map<string, SubscriberMeta>

  /** Topic path → pre-computed downstream routes */
  routes: Map<string, Route[]>

  /** Subscriber ID → wrapped handler function */
  handlers: Map<string, HandlerFn>
}

// =============================================================================
// Private helpers
// =============================================================================

/** Count dot-separated segments in a path. Empty string → 0. */
const getPathDepth = (path: string): number => {
  if (!path) return 0
  return path.split('.').length
}

/**
 * Recompute all downstream routes based on current topic order.
 * For each topic at index i, routes point to ALL topics at index > i.
 */
const recomputeRoutes = (router: TopicRouter): void => {
  router.routes.clear()
  for (const [i, topic] of router.topics.entries()) {
    const downstream: Route[] = router.topics.slice(i + 1).map((targetPath) => {
      const meta = router.topicMeta.get(targetPath)!
      return {
        target: targetPath,
        prefix: meta.prefix,
        prefixLen: meta.prefixLen,
        depth: meta.depth,
      }
    })
    router.routes.set(topic, downstream)
  }
}

// =============================================================================
// Public API
// =============================================================================

/**
 * Creates a new empty TopicRouter instance.
 */
export const createTopicRouter = (): TopicRouter => ({
  topics: [],
  topicMeta: new Map(),
  subscribers: new Map(),
  subscriberMeta: new Map(),
  routes: new Map(),
  handlers: new Map(),
})

/**
 * Add a topic for the given path if it doesn't exist.
 * Re-sorts order and recomputes routes.
 */
export const addTopic = (router: TopicRouter, path: string): void => {
  if (router.topicMeta.has(path)) return

  const depth = getPathDepth(path)
  router.topicMeta.set(path, {
    prefix: depth === 0 ? '' : path + '.',
    prefixLen: depth === 0 ? 0 : path.length + 1,
    depth,
  })
  router.subscribers.set(path, [])
  router.topics.push(path)

  // Sort by depth DESC (stable sort preserves insertion order within same depth)
  router.topics.sort((a, b) => {
    const metaA = router.topicMeta.get(a)!
    const metaB = router.topicMeta.get(b)!
    return metaB.depth - metaA.depth
  })
  recomputeRoutes(router)
}

/**
 * Add topics for multiple paths at once, deferring sort + route recomputation
 * to after all topics are inserted. O(T log T) once instead of per-path.
 */
export const addTopics = (router: TopicRouter, paths: string[]): void => {
  let added = false
  for (const path of paths) {
    if (router.topicMeta.has(path)) continue

    const depth = getPathDepth(path)
    router.topicMeta.set(path, {
      prefix: depth === 0 ? '' : path + '.',
      prefixLen: depth === 0 ? 0 : path.length + 1,
      depth,
    })
    router.subscribers.set(path, [])
    router.topics.push(path)
    added = true
  }

  if (added) {
    router.topics.sort((a, b) => {
      const metaA = router.topicMeta.get(a)!
      const metaB = router.topicMeta.get(b)!
      return metaB.depth - metaA.depth
    })
    recomputeRoutes(router)
  }
}

/**
 * Remove a topic entirely (only if it has no subscribers).
 * Recomputes routes after removal.
 */
export const removeTopic = (router: TopicRouter, path: string): void => {
  router.topicMeta.delete(path)
  router.subscribers.delete(path)
  const orderIndex = router.topics.indexOf(path)
  if (orderIndex !== -1) {
    router.topics.splice(orderIndex, 1)
  }
  recomputeRoutes(router)
}

/**
 * Add a subscriber to a topic.
 */
export const addSubscriber = (
  router: TopicRouter,
  id: string,
  scope: string | null,
  topic: string,
): void => {
  router.subscriberMeta.set(id, { scope, topic })
  router.subscribers.get(topic)!.push(id)
}

/**
 * Remove a subscriber from its topic.
 * Returns true if the topic is now empty (caller should remove it).
 */
export const removeSubscriber = (
  router: TopicRouter,
  id: string,
  topic: string,
): boolean => {
  router.subscriberMeta.delete(id)

  const subscriberIds = router.subscribers.get(topic)
  if (subscriberIds) {
    const idx = subscriberIds.indexOf(id)
    if (idx !== -1) subscriberIds.splice(idx, 1)
    return subscriberIds.length === 0
  }
  return false
}
