/**
 * @sladg/apex-state/testing
 *
 * Lightweight mock of the main module for consumer tests.
 * No real pipeline, no WASM, no concerns — just bare-bones
 * reactive state with call tracking.
 *
 * Consumer setup:
 *   __mocks__/@sladg/apex-state.ts:
 *     export * from '@sladg/apex-state/testing'
 *
 * Then in tests:
 *   vi.mock('@sladg/apex-state')
 *   import { __mocked } from '@sladg/apex-state/testing'
 *
 *   // Inspect state, calls, effects
 *   expect(__mocked.state.calls).toHaveLength(1)
 *   // Programmatically set values
 *   __mocked.set('user.email', 'hello@example.com')
 *   // Reset between tests
 *   __mocked.reset()
 */

import React from 'react'

import { act } from '@testing-library/react'
import { proxy, snapshot, useSnapshot } from 'valtio'

import type { ConcernType } from '../concerns'
import { defaultConcerns } from '../concerns/registry'
import type { StoreConfig } from '../core/types'
import type { GenericStoreApi } from '../store/create-store'
import type { DeepKey, DeepValue, GenericMeta } from '../types'
import { deepClone } from '../utils/deep-clone'
import { dot } from '../utils/dot'

// ---------------------------------------------------------------------------
// Re-export all values except createGenericStore
// Keep in sync with src/index.ts exports
// Types are NOT re-exported — TypeScript resolves them from the real module's
// .d.ts even when vi.mock replaces the runtime module.
// ---------------------------------------------------------------------------
export {
  _,
  applyChangesToObject,
  deepClone,
  defaultConcerns,
  dot,
  evaluateBoolLogic,
  extractPlaceholders,
  findConcern,
  hashKey,
  interpolateTemplate,
  is,
  prebuilts,
  registerFlipPair,
  registerListenerLegacy,
  registerSideEffects,
  registerSyncPairsBatch,
  useBufferedField,
  useKeyboardSelect,
  useThrottledField,
  useTransformedField,
} from '../index'

// ---------------------------------------------------------------------------
// Mock internal state
// ---------------------------------------------------------------------------

interface MockCall {
  path: string
  value: unknown
  meta?: unknown
}

interface MockEffect {
  id: string
  type: 'concerns' | 'sideEffects'
  registration: unknown
}

const __state = proxy({
  /** Current store state (reactive valtio proxy) */
  value: {} as Record<string, unknown>,
  /** Log of all setValue / setChanges calls */
  calls: [] as MockCall[],
  /** Log of all useConcerns / useSideEffects registrations */
  effects: [] as MockEffect[],
})

// ---------------------------------------------------------------------------
// Mock control API — import from '@sladg/apex-state/testing'
// ---------------------------------------------------------------------------

/** Typed chainable returned by `__mocked.set<T>()` — path+value pairs are type-checked */
interface TypedMock<T extends object> {
  set: <P extends DeepKey<T>>(path: P, value: DeepValue<T, P>) => TypedMock<T>
  state: typeof __state
  getState: () => T
  flush: () => Promise<void>
  reset: () => void
}

const __flush = async () => {
  await act(async () => {
    // valtio propagates proxy notifications via microtasks
  })
}

const __reset = () => {
  const keys = Object.keys(__state.value)
  keys.forEach((key) => {
    Reflect.deleteProperty(__state.value, key)
  })
  __state.calls.length = 0
  __state.effects.length = 0
}

const __typedMock = <T extends object>(): TypedMock<T> => {
  const api: TypedMock<T> = {
    set: (path, value) => {
      dot.set__unsafe(__state.value, path as string, deepClone(value))
      return api
    },
    state: __state,
    getState: () => snapshot(__state.value) as T,
    flush: __flush,
    reset: __reset,
  }
  return api
}

/**
 * Mock control object for test assertions and state manipulation.
 *
 * @example
 * ```ts
 * import { __mocked } from '@sladg/apex-state/testing'
 *
 * beforeEach(() => __mocked.reset())
 *
 * // First .set<T>() seeds data and returns typed chainable
 * __mocked.set<MyState>({ email: '' })
 *   .set('email', 'a@b.com')   // DeepKey<MyState> — autocomplete + type-checked
 *   .set('name', 'Alice')
 *
 * // Or just establish the type without seeding
 * __mocked.set<MyState>()
 *   .set('email', 'a@b.com')
 *
 * // Assertions
 * expect(__mocked.state.calls).toHaveLength(1)
 * expect(__mocked.getState()).toEqual({ ... })
 * ```
 */
export const __mocked = {
  /** Reactive proxy — access .value, .calls, .effects for assertions */
  state: __state,

  /**
   * Seed mock state and return typed chainable.
   * - `set<T>()` — establish type, no seeding
   * - `set<T>(data)` — merge data into state, establish type
   * Subsequent `.set(path, value)` calls are type-safe against T.
   */
  set: <T extends object>(data?: Partial<T>): TypedMock<T> => {
    if (data) {
      __state.value = deepClone(data)
    }
    return __typedMock<T>()
  },

  /** Get an immutable snapshot of the current state */
  getState: () => snapshot(__state.value),

  /** Flush pending valtio-triggered React renders */
  flush: __flush,

  /** Reset all mock state between tests */
  reset: __reset,
}

// ---------------------------------------------------------------------------
// Mock createGenericStore
// ---------------------------------------------------------------------------

/**
 * Mock version of createGenericStore.
 *
 * Returns the same API shape as the real module but backed by a shared
 * module-level valtio proxy (__mocked.state). No pipeline, no WASM,
 * no concerns evaluation — just reactive state with call tracking.
 */
export const createGenericStore = <
  DATA extends object,
  META extends GenericMeta = GenericMeta,
  CONCERNS extends readonly ConcernType<string, any, any>[] =
    typeof defaultConcerns,
>(
  _config?: StoreConfig,
): GenericStoreApi<DATA, META, CONCERNS> => {
  // Provider: seeds mock state from initialState
  const Provider = ({
    initialState,
    children,
  }: {
    initialState: Record<string, unknown>
    children?: React.ReactNode
  }) => {
    __state.value = deepClone(initialState)
    return React.createElement(React.Fragment, null, children)
  }
  Provider.displayName = 'MockStoreProvider'

  // Hooks use useSnapshot for automatic React re-rendering via valtio
  const useStore = (path: string) => {
    const snap = useSnapshot(__state)
    const value = dot.get__unsafe(snap.value as Record<string, unknown>, path)
    const setValue = (newValue: unknown, meta?: unknown) => {
      __state.calls.push({ path, value: newValue, meta })
      dot.set__unsafe(__state.value, path, deepClone(newValue))
    }
    return [value, setValue] as const
  }

  const useFieldStore = (path: string) => {
    const [value, setValue] = useStore(path)
    return { value, setValue }
  }

  const useJitStore = () => {
    const snap = useSnapshot(__state)
    return {
      proxyValue: snap.value as Record<string, unknown>,
      setChanges: (changes: [string, unknown, unknown?][]) => {
        for (const [path, value, meta] of changes) {
          __state.calls.push({ path, value, meta })
          dot.set__unsafe(__state.value, path, deepClone(value))
        }
      },
      getState: () => snapshot(__state.value) as Record<string, unknown>,
    }
  }

  const useSideEffects = (id: string, effects: unknown) => {
    __state.effects.push({ id, type: 'sideEffects', registration: effects })
  }

  const useConcerns = (id: string, registration: unknown) => {
    __state.effects.push({ id, type: 'concerns', registration })
  }

  const withConcerns = () => ({
    useFieldStore,
  })

  const api = {
    Provider,
    useStore,
    useFieldStore,
    useJitStore,
    useSideEffects,
    useConcerns,
    withConcerns,
  }

  // Compile-time drift guard: if the real API adds or removes a method,
  // one of these two lines will fail with a type error.
  type _Real = keyof GenericStoreApi<DATA, META, CONCERNS>
  type _Mock = keyof typeof api
  type _NoMissing = _Real extends _Mock ? true : never
  type _NoExtra = _Mock extends _Real ? true : never
  void (0 as unknown as _NoMissing & _NoExtra)

  return api as unknown as GenericStoreApi<DATA, META, CONCERNS>
}
