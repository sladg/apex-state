/**
 * React Test Utilities
 *
 * Shared utilities for React component testing with apex-state stores.
 * Eliminates duplication of Provider wrapping, render helpers, and test store creation.
 */

import React from 'react'

import {
  act,
  fireEvent as tlFireEvent,
  render,
  type RenderResult,
} from '@testing-library/react'
import type { ReactElement } from 'react'
import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'

export interface ConcernType {
  name: string
  evaluate: (props: any) => any
}

export interface ConcernRegistration {
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

  const getDeepValue = (obj: any, path: string): any => {
    return path.split('.').reduce((acc, part) => acc?.[part], obj)
  }

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
          const value = getDeepValue(dataProxy, path)

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

  const getFieldConcerns = (path: string) => {
    const result: Record<string, any> = {}
    const registrations = concernsRegistry.get(path) || []

    registrations.forEach(({ id, path: regPath, concernName }) => {
      const key = `${id}:${regPath}:${concernName}`
      result[concernName] = evaluationCache.get(key)
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
 * Render a component with a store Provider
 *
 * Wraps component in store.Provider with initialState.
 * Common pattern across all integration tests.
 *
 * @example
 * ```typescript
 * import { screen } from '@testing-library/react'
 *
 * const store = createRegistrationFormStore()
 * renderWithStore(<FormComponent />, store, { email: '', password: '' })
 *
 * // Use screen.* instead of destructuring from render() return value
 * expect(screen.getByTestId('email').value).toBe('')
 * fireEvent.click(screen.getByText('Submit'))
 * ```
 */
export const renderWithStore = <T extends object>(
  component: ReactElement,
  store: {
    Provider: React.ComponentType<{
      initialState: T
      children: React.ReactNode
      errorStorePath?: string
    }>
  },
  initialState: T,
  options?: {
    errorStorePath?: string
  },
): RenderResult => {
  const providerProps: {
    initialState: T
    children: React.ReactNode
    errorStorePath?: string
  } = {
    initialState,
    children: component,
  }

  if (options?.errorStorePath) {
    providerProps.errorStorePath = options.errorStorePath
  }

  return render(React.createElement(store.Provider, providerProps))
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
