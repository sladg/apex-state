import type { ListenerRegistrationInternal } from '~/_internal'
import type { StoreInstance } from '~/core/types'
import type { GenericMeta } from '~/types'
import { generateListenerId, guard } from '~/utils/guards'
import type { HandlerFn } from '~/utils/topicRouter'
import {
  addSubscriber,
  addTopic,
  addTopics,
  removeSubscriber,
  removeTopic,
} from '~/utils/topicRouter'

/**
 * Internal registration logic shared by both single and batch variants.
 * Works with ListenerRegistrationInternal to avoid DeepKey resolution overhead.
 */
const prepareRegistration = (
  store: StoreInstance<object>,
  registration: ListenerRegistrationInternal,
  router: { handlers: Map<string, HandlerFn> },
): {
  path: string
  id: string
  wrappedFn: HandlerFn
  scope: string | null
} => {
  guard.listenerScope(registration.path, registration.scope)

  const path = registration.path ?? ''
  const id = generateListenerId(path, registration.fn, router.handlers)

  const wrappedFn: HandlerFn = (changes, state) =>
    store._internal.timing.run(
      'listeners',
      () => registration.fn(changes, state),
      {
        path,
        name: 'listener',
      },
    )

  return { path, id, wrappedFn, scope: registration.scope ?? null }
}

/**
 * Batch version of registerListener. Adds all topics first (single sort + route
 * recomputation), then registers all subscribers. Returns a single combined cleanup.
 *
 * PERF: For N listeners sharing M unique topics, this does O(M log M) work once
 * instead of O(M × M log M) from N individual addTopic calls.
 */
export const registerListenersBatch = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registrations: ListenerRegistrationInternal<DATA, META>[],
): (() => void) => {
  const router = store._internal.graphs.topicRouter

  // Phase 1: Validate all registrations and collect topic paths
  const topicPaths: string[] = []
  const prepared: ReturnType<typeof prepareRegistration>[] = []

  for (const registration of registrations) {
    const entry = prepareRegistration(store, registration, router)
    topicPaths.push(entry.path)
    prepared.push(entry)
  }

  // Phase 2: Add all topics at once (single sort + route recomputation)
  addTopics(router, topicPaths)

  // Phase 3: Register all subscribers and handlers
  const cleanupEntries: { id: string; path: string }[] = []
  for (const { path, id, wrappedFn, scope } of prepared) {
    router.handlers.set(id, wrappedFn)
    addSubscriber(router, id, scope, path)
    cleanupEntries.push({ id, path })
  }

  // Return combined cleanup
  return () => {
    for (const { id, path } of cleanupEntries) {
      const isEmpty = removeSubscriber(router, id, path)
      if (isEmpty) removeTopic(router, path)
      router.handlers.delete(id)
    }
  }
}

export const registerListener = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: StoreInstance<DATA, META>,
  registration: ListenerRegistrationInternal<DATA, META>,
): (() => void) => {
  const router = store._internal.graphs.topicRouter
  const { path, id, wrappedFn, scope } = prepareRegistration(
    store,
    registration,
    router,
  )

  // Ensure topic exists for this path
  addTopic(router, path)

  // Store wrapped handler and subscriber metadata
  router.handlers.set(id, wrappedFn)
  addSubscriber(router, id, scope, path)

  // Return cleanup function
  return () => {
    const isEmpty = removeSubscriber(router, id, path)
    if (isEmpty) removeTopic(router, path)
    router.handlers.delete(id)
  }
}
