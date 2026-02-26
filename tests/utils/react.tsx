/**
 * React Test Utilities
 *
 * Shared utilities for React component testing with apex-state stores.
 * Provides Provider wrapping, render helpers, setValue, and flush utilities.
 *
 * See TESTING_PATTERNS.md for usage guide.
 */

import React, { type ReactElement } from 'react'

import { act, fireEvent as tlFireEvent, render } from '@testing-library/react'
import { proxy, ref, useSnapshot } from 'valtio'
import { expect } from 'vitest'

import type { ConcernType } from '~/concerns'
import { defaultConcerns } from '~/concerns'
import { registerConcernEffects } from '~/concerns/registration.wasm-impl'
import { useStoreContext } from '~/core/context'
import { DEFAULT_STORE_CONFIG } from '~/core/defaults'
import type { StoreConfig, StoreInstance } from '~/core/types'
import { processChangesWasm as processChanges } from '~/pipeline/process-changes.wasm-impl'
import { registerSideEffects } from '~/sideEffects/registration.wasm-impl'
import { createGenericStore } from '~/store/create-store'
import { createInternalState } from '~/store/provider'
import type {
  ArrayOfChanges,
  ConcernRegistrationMap,
  DeepKey,
  DeepRequired,
  DeepValue,
  GenericMeta,
} from '~/types'
import type { SideEffects } from '~/types/side-effects'
import { deepClone } from '~/utils/deep-clone'
import { deepMerge } from '~/utils/deep-merge'
import { createWasmPipeline } from '~/wasm/bridge'
import { isWasmLoaded } from '~/wasm/lifecycle'

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type GenericStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
> = ReturnType<typeof createGenericStore<T, META>>

/** Options for mountStore (new API) */
interface MountStoreOptions<
  T extends object,
  META extends GenericMeta = GenericMeta,
> {
  /** Concern registrations (passed to store.useConcerns) */
  concerns?: ConcernRegistrationMap<T>
  /** ID for concerns registration (default: 'test') */
  concernsId?: string
  /** Side effects (passed to store.useSideEffects) */
  sideEffects?: SideEffects<T, META>
  /** ID for side effects registration (default: 'test') */
  sideEffectsId?: string
  /** data-testid for default root container */
  testId?: string
  /** Custom render function receiving snapshot state */
  customRender?: (state: T) => React.ReactNode
}

/** Options for mountStore (old API — backward compat for v1 tests) */
interface MountStoreOptionsLegacy<T extends object> {
  concerns?: ConcernRegistrationMap<T>
  concernsId?: string
}

/** Return type for mountStore */
interface MountStoreResult<
  T extends object,
  META extends GenericMeta = GenericMeta,
> {
  storeInstance: StoreInstance<T>
  setValue: <P extends DeepKey<T>>(
    path: P,
    value: DeepValue<T, P>,
    meta?: META,
  ) => void
  /** Batch change API — mirrors useJitStore().setChanges */
  setChanges: (changes: ArrayOfChanges<T, META>) => void
}

// ---------------------------------------------------------------------------
// Dual-mode test configuration
// ---------------------------------------------------------------------------

/**
 * Mode configurations for testing (WASM only).
 *
 * @example
 * ```typescript
 * import { MODES } from '../utils/react'
 *
 * describe.each(MODES)('[$name] my feature', ({ config }) => {
 *   it('works', () => {
 *     const store = createGenericStore<State>(config)
 *     // ...
 *   })
 * })
 * ```
 */
export const MODES: readonly { name: string; config: StoreConfig }[] = [
  { name: 'WASM', config: {} },
]

// ---------------------------------------------------------------------------
// mountStore
// ---------------------------------------------------------------------------

// Overload: Old API with component (backward compat for v1 tests)
export function mountStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  component: ReactElement,
  store: GenericStore<T, META>,
  initialState: T,
  options?: MountStoreOptionsLegacy<T>,
): MountStoreResult<T, META>

// Overload: New API without component
export function mountStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: GenericStore<T, META>,
  initialState: T,
  options?: MountStoreOptions<T, META>,
): MountStoreResult<T, META>

/**
 * Render a store with Provider, capturing storeInstance and providing setValue.
 *
 * Supports two signatures:
 * 1. New API: mountStore(store, initialState, options?)
 * 2. Old API: mountStore(<Component />, store, initialState, options?)
 */
export function mountStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  componentOrStore: ReactElement | GenericStore<T, META>,
  storeOrInitialState: GenericStore<T, META> | T,
  initialStateOrOptions?: T | MountStoreOptions<T, META>,
  legacyOptions?: MountStoreOptionsLegacy<T>,
): MountStoreResult<T, META> {
  // Detect which API is being used
  const isOldAPI = React.isValidElement(componentOrStore)

  let component: ReactElement | null
  let store: GenericStore<T, META>
  let initialState: T
  let options: MountStoreOptions<T, META>

  if (isOldAPI) {
    component = componentOrStore as ReactElement
    store = storeOrInitialState as GenericStore<T, META>
    initialState = initialStateOrOptions as T
    options = (legacyOptions ?? {}) as MountStoreOptions<T, META>
  } else {
    component = null
    store = componentOrStore as GenericStore<T, META>
    initialState = storeOrInitialState as T
    options = (initialStateOrOptions ?? {}) as MountStoreOptions<T, META>
  }

  let storeInstance: StoreInstance<T> | null = null

  // Wrapper to capture store instance and register concerns/side-effects
  const WrapperComponent = ({ children }: { children: React.ReactNode }) => {
    storeInstance = useStoreContext<T>()

    if (options.concerns) {
      store.useConcerns(options.concernsId ?? 'test', options.concerns)
    }
    if (options.sideEffects) {
      store.useSideEffects(options.sideEffectsId ?? 'test', options.sideEffects)
    }

    return <>{children}</>
  }

  // Choose content
  let content: React.ReactNode

  if (component) {
    content = component
  } else if (options.customRender) {
    const CustomRenderComponent = () => {
      const snap = useSnapshot(storeInstance!.state) as T
      return (
        <div data-testid={options.testId ?? 'test-root'}>
          {options.customRender!(snap)}
        </div>
      )
    }
    content = <CustomRenderComponent />
  } else {
    content = <div data-testid={options.testId ?? 'test-root'}>Test</div>
  }

  // Pre-initialize WASM pipeline before render so Provider's synchronous
  // path works. Without this, Provider returns null (blocking on async WASM load)
  // and storeInstance is never captured.
  // Note: Provider.useMemo will create its own pipeline. This pre-init is not needed
  // anymore since Provider handles pipeline creation internally via createWasmPipeline().
  // The Provider's initPipeline() call handles this now.

  render(
    React.createElement(store.Provider, {
      initialState: deepClone(initialState),
      children: <WrapperComponent>{content}</WrapperComponent>,
    }),
  )

  // Guard: if storeInstance is still null, Provider didn't render children.
  // This happens when WASM mode is enabled but WASM hasn't been loaded yet.
  if (!storeInstance) {
    throw new Error(
      '[mountStore] storeInstance is null after render. ' +
        'If using WASM mode, ensure WASM is loaded before calling mountStore ' +
        '(call initWasm() in beforeAll/beforeEach).',
    )
  }

  // Helper to set values — matches store's setValue implementation.
  // Calls processChanges directly (same pattern as production code).
  const setValue = <P extends DeepKey<T>>(
    path: P,
    value: DeepValue<T, P>,
    meta?: META,
  ) => {
    act(() => {
      const changes: ArrayOfChanges<T, META> = [
        [path, value, (meta || {}) as META],
      ]

      processChanges(storeInstance!, changes)
    })
  }

  // Batch change API — mirrors useJitStore().setChanges exactly.
  const setChanges = (changes: ArrayOfChanges<T, META>) => {
    act(() => {
      processChanges(storeInstance!, changes)
    })
  }

  return {
    storeInstance: storeInstance!,
    setValue,
    setChanges,
  }
}

// ---------------------------------------------------------------------------
// fireEvent — wrapped in act()
// ---------------------------------------------------------------------------

/**
 * Re-export fireEvent from testing-library, wrapped in act().
 *
 * Uses React's testing-library which properly integrates with React's
 * synthetic event system.
 */
export const fireEvent = {
  change: (element: Element, data: { target: { value: string | boolean } }) => {
    act(() => {
      tlFireEvent.change(element, data)
    })
  },
  click: (element: Element) => {
    act(() => {
      tlFireEvent.click(element)
    })
  },
  blur: (element: Element) => {
    act(() => {
      tlFireEvent.blur(element)
    })
  },
  focus: (element: Element) => {
    act(() => {
      tlFireEvent.focus(element)
    })
  },
}

// ---------------------------------------------------------------------------
// Flush utilities
// ---------------------------------------------------------------------------

/**
 * Flush all pending updates (microtasks, macrotasks, valtio changes, React renders)
 *
 * This flushes:
 * - All pending microtasks (Promises, queueMicrotask, valtio subscribers)
 * - All pending macrotasks (setTimeout, setInterval)
 * - Multiple rounds to catch cascading effects
 *
 * Timing: Uses minimal timeout (20ms) to handle async validators like
 * setTimeout(resolve, 10) while keeping test overhead reasonable.
 */
export const flushEffects = async () => {
  await act(async () => {
    await Promise.resolve()
    await new Promise<void>((resolve) => {
      setTimeout(resolve, 20)
    })
    await Promise.resolve()
  })
}

/**
 * Flush only synchronous effects (no setTimeout wait)
 *
 * Use this for tests where you only need React renders and synchronous
 * valtio effects, not async validators.
 *
 * @example
 * ```typescript
 * setValue('field', 'value')
 * await flushSync()
 * expect(storeInstance.state.field).toBe('value')
 * ```
 */
export const flushSync = async () => {
  await act(async () => {
    await Promise.resolve()
    await Promise.resolve()
  })
}

// ---------------------------------------------------------------------------
// Shadow state parity assertion
// ---------------------------------------------------------------------------

/**
 * Recursively snapshot a valtio proxy to a plain object.
 * Skips getter properties (which are not part of reactive state data).
 */
const snapshotToPlain = (obj: unknown): unknown => {
  if (obj === null || obj === undefined || typeof obj !== 'object') return obj
  if (Array.isArray(obj)) return obj.map(snapshotToPlain)
  const result: Record<string, unknown> = {}
  for (const key of Object.keys(obj as Record<string, unknown>)) {
    const desc = Object.getOwnPropertyDescriptor(obj, key)
    if (desc && desc.get && !desc.set) continue // skip read-only getters
    result[key] = snapshotToPlain((obj as Record<string, unknown>)[key])
  }
  return result
}

/**
 * Assert that valtio proxy state matches WASM shadow state.
 *
 * - Valtio: recursively walk proxy, skip getters
 * - Shadow: `pipeline.shadowDump()` uses `fastParse` which converts
 *   `__APEX_UNDEFINED__` sentinels back to `undefined` via `createFastJson`
 *
 * Both sides use `undefined` consistently because `shadowInit` encodes via
 * `fastStringify` (undefined → sentinel) and `shadowDump` decodes via
 * `fastParse` (sentinel → undefined).
 */
export const expectShadowMatch = <T extends object>(
  storeInstance: StoreInstance<T>,
) => {
  const pipeline = storeInstance._internal.pipeline
  if (!pipeline) throw new Error('No pipeline on store instance')

  const valtioSnapshot = snapshotToPlain(storeInstance.state)
  const shadowSnapshot = pipeline.shadowDump()

  expect(shadowSnapshot).toEqual(valtioSnapshot)
}

// ---------------------------------------------------------------------------
// createTestStore — React-free store construction for benchmarks
// ---------------------------------------------------------------------------

/** Options for createTestStore */
interface CreateTestStoreOptions<
  T extends object,
  META extends GenericMeta = GenericMeta,
> {
  concerns?: ConcernRegistrationMap<T>
  sideEffects?: SideEffects<T, META>
  sideEffectsId?: string
}

/**
 * Create a store instance WITHOUT React rendering.
 *
 * Constructs the store instance directly (proxy, _concerns, _internal),
 * initializes WASM shadow state, and registers concerns/side effects.
 * Use this for benchmarks where React render + act() causes vitest bench NaN.
 *
 * @example
 * ```typescript
 * const { storeInstance, processChanges } = createTestStore<State>(
 *   {},
 *   buildInitialState(),
 *   { concerns: myConcerns, sideEffects: myEffects },
 * )
 * // Use processChanges(storeInstance, changes) directly
 * ```
 */
export const createTestStore = <
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  config: StoreConfig,
  initialState: T,
  options?: CreateTestStoreOptions<T, META>,
): {
  storeInstance: StoreInstance<T>
  processChanges: (
    store: StoreInstance<T>,
    changes: ArrayOfChanges<T, META>,
  ) => void
} => {
  const resolvedConfig = deepMerge(
    DEFAULT_STORE_CONFIG,
    config,
  ) as DeepRequired<StoreConfig>

  // Initialize WASM pipeline
  const internal = createInternalState(resolvedConfig)
  if (!isWasmLoaded()) {
    throw new Error(
      '[createTestStore] WASM is not loaded. ' +
        'Call initWasm() in beforeAll/beforeEach first.',
    )
  }
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(initialState as Record<string, unknown>)
  internal.pipeline = pipeline

  const storeInstance: StoreInstance<T> = {
    state: proxy(initialState),
    _concerns: proxy({} as Record<string, Record<string, unknown>>),
    _internal: ref(internal),
    _debug: null,
  }

  // Register concerns
  if (options?.concerns) {
    registerConcernEffects(
      storeInstance,
      options.concerns,
      defaultConcerns as readonly ConcernType<any, any, any>[],
    )
  }

  // Register side effects
  if (options?.sideEffects) {
    registerSideEffects(
      storeInstance,
      options.sideEffectsId ?? 'test',
      options.sideEffects,
    )
  }

  return {
    storeInstance,
    processChanges: processChanges as (
      store: StoreInstance<T>,
      changes: ArrayOfChanges<T, META>,
    ) => void,
  }
}
