/**
 * React Test Utilities
 *
 * Shared utilities for React component testing with apex-state stores.
 * Eliminates duplication of Provider wrapping, render helpers, and test store creation.
 */

import React, { ReactElement } from 'react'

import {
  act,
  fireEvent as tlFireEvent,
  render,
  type RenderResult,
} from '@testing-library/react'
import { useSnapshot } from 'valtio'
import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'

import { useStoreContext } from '../../src/core/context'
import type { StoreInstance } from '../../src/core/types'
import { createGenericStore } from '../../src/store/createStore'
import type { DeepKey, GenericMeta } from '../../src/types'
import { dot } from '../../src/utils/dot'

// Type helpers for store
type GenericStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
> = ReturnType<typeof createGenericStore<T, META>>

// Store with attached default state (returned by createStore)
type StoreWithDefault<
  T extends object,
  META extends GenericMeta = GenericMeta,
> = GenericStore<T, META> & { defaultState: T }

// Type for concerns registration - inferred from store
type ConcernsRegistration<T extends object> = Partial<
  Record<DeepKey<T>, Record<string, any>>
>

interface ConcernType {
  name: string
  evaluate: (props: any) => any
}

interface ConcernRegistration {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: any
  dispose: () => void
}

/**
 * Create a minimal test store with concerns support
 *
 * This replicates the createTestStore pattern found across concerns unit tests.
 * Provides proxy data, concerns registry, and evaluation cache.
 *
 * @example
 * ```typescript
 * const store = createTestStore({ count: 0 })
 * store.useConcerns('test', {
 *   count: { validationState: { schema: z.number().min(0) } }
 * })
 * ```
 */
export const createTestStore = <T extends object>(initialData: T) => {
  const dataProxy = proxy<T>(initialData)
  const concernsRegistry = new Map<string, ConcernRegistration[]>()
  const evaluationCache = new Map<string, unknown>()

  const useConcerns = (
    id: string,
    registration: Record<string, any>,
    concerns: Record<string, ConcernType>,
  ) => {
    const disposeCallbacks: (() => void)[] = []

    Object.entries(registration).forEach(([path, concernConfigs]) => {
      if (!concernConfigs) return

      Object.entries(concernConfigs).forEach(([concernName, config]) => {
        if (!config) return

        const concern = concerns[concernName as keyof typeof concerns]
        if (!concern) return

        const concernKey = `${id}:${path}:${concernName}`

        const dispose = effect(() => {
          const value = dot.get__unsafe(dataProxy, path)

          const result = concern.evaluate({
            state: dataProxy,
            path,
            value,
            ...config,
          })

          evaluationCache.set(concernKey, result)
        })

        const reg: ConcernRegistration = {
          id,
          path,
          concernName,
          concern,
          config,
          dispose,
        }

        const pathRegs = concernsRegistry.get(path) || []
        pathRegs.push(reg)
        concernsRegistry.set(path, pathRegs)

        disposeCallbacks.push(dispose)
      })
    })

    return () => {
      disposeCallbacks.forEach((dispose) => dispose())
      concernsRegistry.forEach((regs, path) => {
        const filtered = regs.filter((r) => r.id !== id)
        if (filtered.length === 0) {
          concernsRegistry.delete(path)
        } else {
          concernsRegistry.set(path, filtered)
        }
      })
    }
  }

  const getFieldConcerns = <K extends string = string>(path: string) => {
    const result: Record<K, unknown> = {} as Record<K, unknown>
    const registrations = concernsRegistry.get(path) || []

    registrations.forEach(({ id, path: regPath, concernName }) => {
      const key = `${id}:${regPath}:${concernName}`
      result[concernName as K] = evaluationCache.get(key)
    })

    return result
  }

  return {
    proxy: dataProxy,
    useConcerns,
    getFieldConcerns,
  }
}

/**
 * Create a store with attached default state for use with renderWithStore.
 *
 * Wraps createGenericStore and attaches `defaultState` so that
 * renderWithStore can use it automatically when no explicit initialState is passed.
 *
 * @example
 * ```typescript
 * const store = createStore<TestState>(testStateFixtures.formEmpty)
 * renderWithStore(<FormComponent />, store) // uses store.defaultState
 * renderWithStore(<FormComponent />, store, overrideState) // uses override
 * ```
 */
export const createStore = <
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  defaultState: T,
  config?: Parameters<typeof createGenericStore<T, META>>[0],
): StoreWithDefault<T, META> => {
  const store = createGenericStore<T, META>(config)
  return Object.assign(store, { defaultState })
}

// Overload: Old API with component, initialState from store.defaultState
export function renderWithStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  component: ReactElement,
  store: StoreWithDefault<T, META>,
  options?: {
    concerns?: ConcernsRegistration<T>
    concernsId?: string
  },
): RenderResult & { storeInstance: StoreInstance<T, META> }

// Overload: Old API with component (backward compatible)
export function renderWithStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  component: ReactElement,
  store: GenericStore<T, META>,
  initialState: T,
  options?: {
    concerns?: ConcernsRegistration<T>
    concernsId?: string
  },
): RenderResult & { storeInstance: StoreInstance<T, META> }

// Overload: New API without component (simpler)
export function renderWithStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  store: GenericStore<T, META>,
  initialState: T,
  options?: {
    concerns?: ConcernsRegistration<T>
    concernsId?: string
    testId?: string
    customRender?: (state: T) => React.ReactNode
  },
): RenderResult & { storeInstance: StoreInstance<T, META> }

/**
 * Render store with Provider
 *
 * Supports two signatures:
 * 1. Old API: renderWithStore(<Component />, store, initialState, options)
 * 2. New API: renderWithStore(store, initialState, options)
 *
 * @example
 * ```typescript
 * // New API - no component needed!
 * const { storeInstance } = renderWithStore(
 *   store,
 *   { email: '' },
 *   { concerns: { email: { validationState: { schema: z.string().email() } } } }
 * )
 *
 * // Old API - still supported
 * const { storeInstance } = renderWithStore(
 *   <MyComponent />,
 *   store,
 *   { email: '' },
 *   { concerns: { email: { validationState: { schema: z.string().email() } } } }
 * )
 * ```
 */
export function renderWithStore<
  T extends object,
  META extends GenericMeta = GenericMeta,
>(
  componentOrStore: ReactElement | GenericStore<T, META>,
  storeOrInitialState: GenericStore<T, META> | T,
  initialStateOrOptions?: T | any,
  optionsOrUndefined?: any,
): RenderResult & { storeInstance: StoreInstance<T, META> } {
  // Detect which API is being used
  const isOldAPI = React.isValidElement(componentOrStore)

  let component: ReactElement | null
  let store: GenericStore<T, META>
  let initialState: T
  let options: any

  if (isOldAPI) {
    // Old API: (component, store, ...)
    component = componentOrStore as ReactElement
    store = storeOrInitialState as GenericStore<T, META>

    // Check if initialState was provided or should use store.defaultState
    if (
      initialStateOrOptions === undefined ||
      (initialStateOrOptions &&
        typeof initialStateOrOptions === 'object' &&
        ('concerns' in initialStateOrOptions ||
          'concernsId' in initialStateOrOptions))
    ) {
      // No initialState provided, or third arg is options — use store.defaultState
      // structuredClone prevents valtio's proxy() from mutating the shared fixture
      initialState = structuredClone(
        (store as StoreWithDefault<T, META>).defaultState,
      )
      options = initialStateOrOptions
    } else {
      // Old API: (component, store, initialState, options)
      initialState = initialStateOrOptions as T
      options = optionsOrUndefined
    }
  } else {
    // New API: (store, initialState, options)
    component = null
    store = componentOrStore as GenericStore<T, META>
    initialState = storeOrInitialState as T
    options = initialStateOrOptions
  }
  let storeInstance: StoreInstance<T, META> | null = null

  // Wrapper to capture store instance and register concerns
  function WrapperComponent({ children }: { children: React.ReactNode }) {
    storeInstance = useStoreContext<T, META>()

    // Register concerns if provided
    if (options?.concerns) {
      store.useConcerns(options.concernsId || 'test', options.concerns)
    }

    return <>{children}</>
  }

  // Choose content based on API
  let content: React.ReactNode

  if (component) {
    // Old API: use provided component
    content = component
  } else if (options?.customRender) {
    // New API with custom render
    function CustomRenderComponent() {
      const snap = useSnapshot(storeInstance!.state) as T
      return (
        <div data-testid={options.testId || 'test-root'}>
          {options.customRender!(snap)}
        </div>
      )
    }
    content = <CustomRenderComponent />
  } else {
    // New API: default simple container
    content = <div data-testid={options?.testId || 'test-root'}>Test</div>
  }

  const renderResult = render(
    React.createElement(store.Provider, {
      initialState,
      children: <WrapperComponent>{content}</WrapperComponent>,
    }),
  )

  return {
    ...renderResult,
    storeInstance: storeInstance!,
  }
}

/**
 * Re-export fireEvent from testing-library
 *
 * Uses React's testing-library which properly integrates with React's synthetic event system.
 * Previously this was a custom implementation that manually dispatched DOM events,
 * but that doesn't trigger React's onChange handlers correctly.
 */
export const fireEvent = {
  change: (element: Element, data: any) => {
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

/**
 * Flush all pending updates (microtasks, macrotasks, valtio changes, React renders)
 *
 * Extracted from tests/setup.ts for explicit reuse in tests.
 * Used as a replacement for waitFor() for better performance and determinism.
 *
 * This flushes:
 * - All pending microtasks (Promises, queueMicrotask, valtio subscribers)
 * - All pending macrotasks (setTimeout, setInterval)
 * - Multiple rounds to catch cascading effects
 *
 * All async updates are wrapped in act() to prevent React warnings.
 *
 * Timing: Uses minimal timeout (20ms) to handle async validators like
 * setTimeout(resolve, 10) while keeping test overhead reasonable.
 */
export const flushEffects = async () => {
  await act(async () => {
    // First flush microtasks
    await Promise.resolve()

    // Then allow macrotasks (setTimeout calls) to execute
    // 20ms is enough for setTimeout(resolve, 10) + overhead
    await new Promise<void>((resolve) => {
      setTimeout(resolve, 20)
    })

    // Final microtask flush
    await Promise.resolve()
  })
}

/**
 * Flush only synchronous effects (no setTimeout wait)
 *
 * Use this for performance tests where you only need React renders
 * and synchronous valtio effects, not async validators.
 *
 * This flushes:
 * - All pending microtasks (Promises, queueMicrotask, valtio subscribers)
 * - React renders and effects
 *
 * Does NOT wait for:
 * - setTimeout/setInterval callbacks (async validators)
 *
 * @example
 * ```typescript
 * benchmark.start('render')
 * store.value = 123
 * await flushSync()  // Only ~0-2ms overhead instead of 20ms
 * const duration = benchmark.end('render')
 * ```
 */
export const flushSync = async () => {
  await act(async () => {
    // Flush microtasks only - no setTimeout wait
    await Promise.resolve()
    await Promise.resolve()
  })
}

/**
 * Validate a field and update the errors object.
 *
 * Replaces the repeated spread-set-delete pattern:
 *   const errors = { ...errorsField.value }
 *   if (!isValid) errors[name] = [msg]; else delete errors[name];
 *   errorsField.setValue(errors)
 */
export const validateField = (
  errorsField: { value: Record<string, any[]>; setValue: (v: any) => void },
  fieldName: string,
  isValid: boolean,
  errorMessage: string,
) => {
  const { [fieldName]: _, ...rest } = errorsField.value
  errorsField.setValue(
    isValid ? rest : { ...rest, [fieldName]: [errorMessage] },
  )
}

/**
 * Common assertions for form validation tests
 */
export const assertions = {
  /**
   * Assert field value matches expected
   */
  fieldValue: (
    element: HTMLInputElement | HTMLSelectElement,
    expected: string,
  ): boolean => {
    return element.value === expected
  },

  /**
   * Assert checkbox state
   */
  checkboxState: (element: HTMLInputElement, expected: boolean): boolean => {
    return element.checked === expected
  },

  /**
   * Assert element is visible (not null)
   */
  isVisible: (element: HTMLElement | null): boolean => {
    return element !== null
  },

  /**
   * Assert element is hidden (null)
   */
  isHidden: (element: HTMLElement | null): boolean => {
    return element === null
  },

  /**
   * Assert element is disabled
   */
  isDisabled: (element: HTMLButtonElement | HTMLInputElement): boolean => {
    return element.disabled
  },

  /**
   * Assert element is enabled
   */
  isEnabled: (element: HTMLButtonElement | HTMLInputElement): boolean => {
    return !element.disabled
  },

  /**
   * Assert element is readonly
   */
  isReadOnly: (element: HTMLInputElement): boolean => {
    return element.readOnly
  },
}

/**
 * DOM query helpers
 */
export const domHelpers = {
  /**
   * Get all error messages in the document
   */
  getAllErrors: (root: HTMLElement | Document = document): string[] => {
    const errorElements = root.querySelectorAll('[data-testid*="error"]')
    return Array.from(errorElements).map((el) => el.textContent || '')
  },

  /**
   * Check if form has any validation errors
   */
  hasErrors: (root: HTMLElement | Document = document): boolean => {
    return root.querySelectorAll('[data-testid*="error"]').length > 0
  },

  /**
   * Get error count
   */
  getErrorCount: (root: HTMLElement | Document = document): number => {
    return root.querySelectorAll('[data-testid*="error"]').length
  },

  /**
   * Get field by testid
   */
  getField: (
    testId: string,
    root: HTMLElement | Document = document,
  ): HTMLInputElement => {
    return root.querySelector(`[data-testid="${testId}"]`) as HTMLInputElement
  },

  /**
   * Get button by testid
   */
  getButton: (
    testId: string,
    root: HTMLElement | Document = document,
  ): HTMLButtonElement => {
    return root.querySelector(`[data-testid="${testId}"]`) as HTMLButtonElement
  },
}
