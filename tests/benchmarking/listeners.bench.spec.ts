/**
 * Listener Processing Strategy Benchmark
 *
 * Compares approaches for processListeners with change accumulation:
 * 1. Flat loop (current): pre-computed FlatListener[], single loop, push spillage
 * 2. Old nested loop: Map + sortedListenerPaths, nested loop, queue tracking
 * 3. Reduce (immutable): reduce with spread accumulator
 * 4. Reduce (mutable acc): reduce with push accumulator
 * 5. Graph-based: TopicRouter with pre-computed routes and local pendingByTopic
 */

import { bench, describe } from 'vitest'

import type { ListenerRegistrationInternal } from '~/_internal/core/types'
import type { ChangeTuple } from '~/_internal/types/changes'
import { getPathDepth } from '~/_internal/utils/pathUtils'
import { dot } from '~/utils/dot'
import type { HandlerFn, Route, TopicRouter } from '~/utils/topicRouter'

import type { TestState } from '../mocks/types'

// ============================================================================
// Types
// ============================================================================

type AnyChange = ChangeTuple[number]

/** Legacy flat listener structure for backward-compat benchmarks */
interface FlatListener {
  path: string
  prefix: string
  prefixLen: number
  isRoot: boolean
  scope: string | null
  fn: HandlerFn
  depth: number
}

// ============================================================================
// Shared helpers
// ============================================================================

const filterAndRelativize = (
  changes: AnyChange[],
  listenerPath: string,
): AnyChange[] => {
  const result: AnyChange[] = []
  if (listenerPath === '') {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }
  const prefix = listenerPath + '.'
  for (const change of changes) {
    if (change[0].startsWith(prefix)) {
      result.push([change[0].slice(prefix.length), change[1], change[2]])
    }
  }
  return result
}

const getRelevantChanges = (
  changes: AnyChange[],
  prefix: string,
  prefixLen: number,
  isRoot: boolean,
): AnyChange[] => {
  const result: AnyChange[] = []
  if (isRoot) {
    for (const change of changes) {
      if (!change[0].includes('.')) {
        result.push(change)
      }
    }
    return result
  }
  for (const change of changes) {
    if (change[0].startsWith(prefix)) {
      result.push([change[0].slice(prefixLen), change[1], change[2]])
    }
  }
  return result
}

const callListener = (
  registration: ListenerRegistrationInternal<TestState>,
  relevantChanges: AnyChange[],
  currentState: TestState,
): AnyChange[] => {
  if (relevantChanges.length === 0) return []
  const scope = (registration.scope as string) ?? ''
  const scopedState =
    scope === '' ? currentState : dot.get__unsafe(currentState, scope)
  const fn = registration.fn as unknown as HandlerFn
  const result = fn(relevantChanges, scopedState)
  if (!result || result.length === 0) return []
  return result.map(([path, value, meta]) => [
    path,
    value,
    { isListenerChange: true, ...meta },
  ])
}

const callFlatListener = (
  fn: HandlerFn,
  scope: string | null,
  relevantChanges: AnyChange[],
  currentState: TestState,
): AnyChange[] => {
  if (relevantChanges.length === 0) return []
  const scopedState =
    scope === null ? currentState : dot.get__unsafe(currentState, scope)
  const result = fn(relevantChanges, scopedState)
  if (!result || result.length === 0) return []
  return result.map(([path, value, meta]) => [
    path,
    value,
    { isListenerChange: true, ...meta },
  ])
}

// ============================================================================
// Strategy A: Flat loop (current implementation)
// ============================================================================

const processListeners_flat = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: TestState,
): AnyChange[] => {
  const allChanges = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )
  const produced: AnyChange[] = []

  for (const listener of flatListeners) {
    const relevant = getRelevantChanges(
      allChanges,
      listener.prefix,
      listener.prefixLen,
      listener.isRoot,
    )
    if (relevant.length === 0) continue

    const newChanges = callFlatListener(
      listener.fn,
      listener.scope,
      relevant,
      currentState,
    )
    if (newChanges.length > 0) {
      produced.push(...newChanges)
      allChanges.push(...newChanges)
    }
  }

  return produced
}

// ============================================================================
// Strategy B: Old nested loop (Map + sortedListenerPaths)
// ============================================================================

const processListeners_nested = (
  changes: AnyChange[],
  listeners: Map<string, ListenerRegistrationInternal<TestState>[]>,
  sortedListenerPaths: string[],
  currentState: TestState,
): AnyChange[] => {
  const sortedChanges = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )
  const produced: AnyChange[] = []

  for (const listenerPath of sortedListenerPaths) {
    const pathListeners = listeners.get(listenerPath)!

    let relevantChanges = filterAndRelativize(sortedChanges, listenerPath)

    for (const registration of pathListeners) {
      const newChanges = callListener(
        registration,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        produced.push(...newChanges)
        sortedChanges.push(...newChanges)
        relevantChanges = filterAndRelativize(sortedChanges, listenerPath)
      }
    }
  }

  return produced
}

// ============================================================================
// Strategy C: reduce (immutable)
// ============================================================================

const processListeners_reduce = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: BenchState,
): AnyChange[] => {
  const initialSorted = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )

  const result = flatListeners.reduce<{
    allChanges: AnyChange[]
    produced: AnyChange[]
  }>(
    (acc, listener) => {
      const relevantChanges = getRelevantChanges(
        acc.allChanges,
        listener.prefix,
        listener.prefixLen,
        listener.isRoot,
      )
      const newChanges = callFlatListener(
        listener.fn,
        listener.scope,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        return {
          allChanges: [...acc.allChanges, ...newChanges],
          produced: [...acc.produced, ...newChanges],
        }
      }
      return acc
    },
    { allChanges: initialSorted, produced: [] },
  )

  return result.produced
}

// ============================================================================
// Strategy D: reduce (mutable accumulator)
// ============================================================================

const processListeners_reduceMut = (
  changes: AnyChange[],
  flatListeners: FlatListener[],
  currentState: BenchState,
): AnyChange[] => {
  const initialSorted = [...changes].sort(
    (a, b) => getPathDepth(b[0]) - getPathDepth(a[0]),
  )

  const result = flatListeners.reduce<{
    allChanges: AnyChange[]
    produced: AnyChange[]
  }>(
    (acc, listener) => {
      const relevantChanges = getRelevantChanges(
        acc.allChanges,
        listener.prefix,
        listener.prefixLen,
        listener.isRoot,
      )
      const newChanges = callFlatListener(
        listener.fn,
        listener.scope,
        relevantChanges,
        currentState,
      )
      if (newChanges.length > 0) {
        acc.allChanges.push(...newChanges)
        acc.produced.push(...newChanges)
      }
      return acc
    },
    { allChanges: initialSorted, produced: [] },
  )

  return result.produced
}

// ============================================================================
// Strategy E: router-based (TopicRouter with pre-computed routes)
// ============================================================================

/* eslint-disable sonarjs/elseif-without-else, sonarjs/cognitive-complexity */
const processListeners_graph = (
  changes: AnyChange[],
  router: TopicRouter,
  currentState: BenchState,
): AnyChange[] => {
  const produced: AnyChange[] = []

  // Seed: route each change to matching topics by walking up ancestors
  const pendingByTopic = new Map<string, AnyChange[]>()
  for (const topic of router.topics) pendingByTopic.set(topic, [])

  for (const change of changes) {
    const changePath = change[0]
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
    if (!changePath.includes('.')) {
      const rootPending = pendingByTopic.get('')
      if (rootPending) rootPending.push(change)
    }
  }

  for (const topic of router.topics) {
    const meta = router.topicMeta.get(topic)!
    const subscriberIds = router.subscribers.get(topic)!
    const topicRoutes = router.routes.get(topic)!
    const pending = pendingByTopic.get(topic)!

    if (pending.length === 0) continue
    const topicRelevant = pending

    for (const id of subscriberIds) {
      const fn = router.handlers.get(id)
      if (!fn) continue
      const subscriber = router.subscriberMeta.get(id)!
      const scopedState =
        subscriber.scope === null
          ? currentState
          : dot.get__unsafe(currentState, subscriber.scope)
      const result = fn(topicRelevant, scopedState)
      if (!result || result.length === 0) continue

      for (const [path, value, changeMeta] of result) {
        const enrichedMeta = { isListenerChange: true, ...changeMeta }
        const change: AnyChange = [path, value, enrichedMeta]

        produced.push(change)

        // Within-topic accumulation
        if (meta.depth === 0 && !path.includes('.')) {
          topicRelevant.push([path, value, enrichedMeta])
        } else if (path.startsWith(meta.prefix)) {
          topicRelevant.push([path.slice(meta.prefixLen), value, enrichedMeta])
        }

        // Route to downstream topics via pre-computed routes
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

  return produced
}

// ============================================================================
// Test fixture factory
// ============================================================================

const createListenerFn = (
  producesChanges: boolean,
  isFirst: boolean,
): HandlerFn =>
  function listenerFn(_changes: AnyChange[]) {
    if (producesChanges && isFirst) {
      return [['_marker', Date.now(), {}] as AnyChange]
    }
    return undefined
  } as HandlerFn

/* eslint-disable sonarjs/cognitive-complexity */
const createFixture = (config: {
  listenerPaths: string[]
  listenersPerPath: number
  producesChanges: boolean
  changeCount: number
}) => {
  const state: BenchState = { root: {} }
  const listeners = new Map<
    string,
    ListenerRegistrationInternal<BenchState>[]
  >()
  const flatListeners: FlatListener[] = []

  // Build the router
  const router: TopicRouter = {
    topics: [],
    topicMeta: new Map(),
    subscribers: new Map(),
    subscriberMeta: new Map(),
    routes: new Map(),
    handlers: new Map(),
  }

  for (const path of config.listenerPaths) {
    const regs: ListenerRegistrationInternal<BenchState>[] = []
    const depth = getPathDepth(path)
    const isRoot = depth === 0
    const prefix = isRoot ? '' : path + '.'
    const prefixLen = isRoot ? 0 : path.length + 1

    // Set up topic in router
    router.topicMeta.set(path, { prefix, prefixLen, depth })
    router.subscribers.set(path, [])
    router.topics.push(path)

    for (let i = 0; i < config.listenersPerPath; i++) {
      const fn = createListenerFn(config.producesChanges, i === 0)
      const id = isRoot ? `__${fn.name}_${i}` : `${path}_${fn.name}_${i}`

      regs.push({
        path: isRoot ? null : path,
        scope: isRoot ? null : path,
        fn,
      })

      flatListeners.push({
        path,
        prefix,
        prefixLen,
        isRoot,
        scope: isRoot ? null : path,
        fn,
        depth,
      })

      // Add to router
      router.subscriberMeta.set(id, {
        scope: isRoot ? null : path,
        topic: path,
      })
      router.subscribers.get(path)!.push(id)
      router.handlers.set(id, fn)
    }

    listeners.set(path, regs)
  }

  // Sort: deepest first (matching real behavior)
  const sortedListenerPaths = Array.from(listeners.keys()).sort(
    (a, b) => getPathDepth(b) - getPathDepth(a),
  )

  // Sort flatListeners the same way
  flatListeners.sort((a, b) => b.depth - a.depth)

  // Sort router topics and compute routes
  router.topics.sort((a, b) => {
    const metaA = router.topicMeta.get(a)!
    const metaB = router.topicMeta.get(b)!
    return metaB.depth - metaA.depth
  })

  // Compute routes
  for (let i = 0; i < router.topics.length; i++) {
    const downstream: Route[] = []
    for (let j = i + 1; j < router.topics.length; j++) {
      const targetPath = router.topics[j]!
      const targetMeta = router.topicMeta.get(targetPath)!
      downstream.push({
        target: targetPath,
        prefix: targetMeta.prefix,
        prefixLen: targetMeta.prefixLen,
        depth: targetMeta.depth,
      })
    }
    router.routes.set(router.topics[i]!, downstream)
  }

  const changes: AnyChange[] = Array.from(
    { length: config.changeCount },
    (_, i) => {
      const path = config.listenerPaths[i % config.listenerPaths.length]!
      const childPath = path === '' ? `field${i}` : `${path}.field${i}`
      return [childPath, `value${i}`, {}] as AnyChange
    },
  )

  return {
    state,
    listeners,
    sortedListenerPaths,
    flatListeners,
    router,
    changes,
  }
}

// ============================================================================
// Benchmarks
// ============================================================================

describe('processListeners strategies', () => {
  // --- Small: 3 paths, 1 listener each, no produced changes ---
  describe('small (3 paths, 1 listener, no spillage)', () => {
    const fixture = createFixture({
      listenerPaths: ['a.b.c', 'a.b', ''],
      listenersPerPath: 1,
      producesChanges: false,
      changeCount: 3,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (TopicRouter)', () => {
      processListeners_graph(fixture.changes, fixture.router, fixture.state)
    })
  })

  // --- Medium: 5 paths, 3 listeners each, with spillage ---
  describe('medium (5 paths, 3 listeners, with spillage)', () => {
    const fixture = createFixture({
      listenerPaths: ['a.b.c.d', 'a.b.c', 'a.b', 'a', ''],
      listenersPerPath: 3,
      producesChanges: true,
      changeCount: 10,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (TopicRouter)', () => {
      processListeners_graph(fixture.changes, fixture.router, fixture.state)
    })
  })

  // --- Large: 10 paths, 5 listeners each, with spillage, 50 changes ---
  describe('large (10 paths, 5 listeners, 50 changes, spillage)', () => {
    const fixture = createFixture({
      listenerPaths: [
        'store.products.item.variant.option',
        'store.products.item.variant',
        'store.products.item',
        'store.products',
        'store.cart.items.detail',
        'store.cart.items',
        'store.cart',
        'store.user.profile',
        'store.user',
        '',
      ],
      listenersPerPath: 5,
      producesChanges: true,
      changeCount: 50,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (TopicRouter)', () => {
      processListeners_graph(fixture.changes, fixture.router, fixture.state)
    })
  })

  // --- Realistic: hashmap with 50 listeners, most not triggered ---
  // Models: store.users.[userId].profile.settings.[settingId]
  // 5 changes on one hashmap key, 15 triggered (5 nested + 10 root), 35 cold
  describe('realistic: hashmap 50 listeners, 15 triggered, 35 cold', () => {
    // Triggered paths: the subtree where changes land
    const triggeredNested = [
      'app.users.u1.profile.settings.s1', // deepest: exact match
      'app.users.u1.profile.settings', // level 5
      'app.users.u1.profile', // level 4
      'app.users.u1', // level 3
      'app.users', // level 2
    ]
    // Cold paths: other hashmap keys that won't match
    const coldPaths = [
      'app.users.u2.profile.settings.s1',
      'app.users.u2.profile.settings',
      'app.users.u2.profile',
      'app.users.u2',
      'app.users.u3.profile.settings.s2',
      'app.users.u3.profile.settings',
      'app.users.u3.profile',
      'app.users.u3',
      'app.users.u4.profile.settings.s3',
      'app.users.u4.profile.settings',
      'app.users.u4.profile',
      'app.users.u4',
      'app.products.p1.variants.v1',
      'app.products.p1.variants',
      'app.products.p1',
      'app.products.p2.variants.v2',
      'app.products.p2.variants',
      'app.products.p2',
      'app.products',
      'app.cart.items.i1',
      'app.cart.items.i2',
      'app.cart.items',
      'app.cart',
      'app.settings.theme',
      'app.settings',
    ]

    // Custom fixture: 5 nested (1 each) + root (10) + 25 cold (1 each)
    const state: BenchState = {
      app: { users: { u1: { profile: { settings: { s1: {} } } } } },
    }
    const listeners = new Map<
      string,
      ListenerRegistrationInternal<BenchState>[]
    >()
    const flatListeners: FlatListener[] = []
    const router: TopicRouter = {
      topics: [],
      topicMeta: new Map(),
      subscribers: new Map(),
      subscriberMeta: new Map(),
      routes: new Map(),
      handlers: new Map(),
    }

    const allPaths = [...triggeredNested, '', ...coldPaths]
    const listenersPerPathMap: Record<string, number> = { '': 10 }
    for (const p of triggeredNested) listenersPerPathMap[p] = 1
    for (const p of coldPaths) listenersPerPathMap[p] = 1

    for (const path of allPaths) {
      const isRoot = path === ''
      const prefix = isRoot ? '' : path + '.'
      const prefixLen = isRoot ? 0 : path.length + 1
      const depth = getPathDepth(path)
      const count = listenersPerPathMap[path] ?? 1

      router.topicMeta.set(path, { prefix, prefixLen, depth })
      router.subscribers.set(path, [])
      router.topics.push(path)

      const regs: ListenerRegistrationInternal<BenchState>[] = []
      for (let i = 0; i < count; i++) {
        const fn = createListenerFn(false, false)
        const id = isRoot ? `__${fn.name}_${i}` : `${path}_${fn.name}_${i}`
        regs.push({
          path: isRoot ? null : path,
          scope: isRoot ? null : path,
          fn: fn,
        })
        flatListeners.push({
          path,
          prefix,
          prefixLen,
          isRoot,
          scope: isRoot ? null : path,
          fn,
          depth,
        })
        router.subscriberMeta.set(id, {
          scope: isRoot ? null : path,
          topic: path,
        })
        router.handlers.set(id, fn)
        router.subscribers.get(path)!.push(id)
      }
      listeners.set(path, regs)
    }

    const sortedListenerPaths = Array.from(listeners.keys()).sort(
      (a, b) => getPathDepth(b) - getPathDepth(a),
    )
    flatListeners.sort((a, b) => b.depth - a.depth)
    router.topics.sort((a, b) => {
      const metaA = router.topicMeta.get(a)!
      const metaB = router.topicMeta.get(b)!
      return metaB.depth - metaA.depth
    })
    for (let i = 0; i < router.topics.length; i++) {
      const downstream: Route[] = []
      for (let j = i + 1; j < router.topics.length; j++) {
        const targetPath = router.topics[j]!
        const targetMeta = router.topicMeta.get(targetPath)!
        downstream.push({
          target: targetPath,
          prefix: targetMeta.prefix,
          prefixLen: targetMeta.prefixLen,
          depth: targetMeta.depth,
        })
      }
      router.routes.set(router.topics[i]!, downstream)
    }

    // 5 changes all under the triggered subtree
    const changes: AnyChange[] = [
      ['app.users.u1.profile.settings.s1.name', 'Alice', {}],
      ['app.users.u1.profile.settings.s1.value', 42, {}],
      ['app.users.u1.profile.settings.s1.enabled', true, {}],
      ['app.users.u1.profile.settings.s1.priority', 'high', {}],
      ['app.users.u1.profile.settings.s1.updated', Date.now(), {}],
    ]

    bench('flat loop', () => {
      processListeners_flat(changes, flatListeners, state)
    })

    bench('nested loop (old)', () => {
      processListeners_nested(changes, listeners, sortedListenerPaths, state)
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(changes, flatListeners, state)
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(changes, flatListeners, state)
    })

    bench('graph (TopicRouter)', () => {
      processListeners_graph(changes, router, state)
    })
  })

  // --- Stress: many same-path listeners (accumulation-heavy) ---
  describe('stress: 20 same-path listeners with accumulation', () => {
    const fixture = createFixture({
      listenerPaths: ['data.items'],
      listenersPerPath: 20,
      producesChanges: true,
      changeCount: 5,
    })

    bench('flat loop (current)', () => {
      processListeners_flat(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('nested loop (old)', () => {
      processListeners_nested(
        fixture.changes,
        fixture.listeners,
        fixture.sortedListenerPaths,
        fixture.state,
      )
    })

    bench('reduce (immutable)', () => {
      processListeners_reduce(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('reduce (mutable acc)', () => {
      processListeners_reduceMut(
        fixture.changes,
        fixture.flatListeners,
        fixture.state,
      )
    })

    bench('graph (TopicRouter)', () => {
      processListeners_graph(fixture.changes, fixture.router, fixture.state)
    })
  })
})
