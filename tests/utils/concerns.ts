/**
 * Concern test utilities
 *
 * Reusable mock store creation for unit testing concerns without React.
 * Extracted from duplicated patterns in concerns unit tests.
 */

import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'

import { dot } from '../../src/utils/dot'

/**
 * Minimal concern type for testing
 */
export interface ConcernType<TProps = unknown, TResult = unknown> {
  name: string
  evaluate: (props: TProps) => TResult
}

/**
 * Concern registration entry (internal)
 */
interface ConcernRegistration<TConfig = unknown> {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: TConfig
  dispose: () => void
}

/**
 * Test store structure returned by createTestStore
 */
interface TestStore<T extends object> {
  proxy: T
  useConcerns: (id: string, registration: Record<string, any>) => () => void
  getFieldConcerns: <K extends string = string>(
    path: string,
  ) => Record<K, unknown>
}

/**
 * Create a test store with concern registration support
 *
 * This utility creates a minimal mock store for unit testing concerns
 * without requiring React or the full store implementation.
 *
 * @example
 * ```typescript
 * const store = createTestStore(
 *   { user: { name: 'John', age: 30 } },
 *   { validationState: myValidationConcern, tooltip: myTooltipConcern }
 * )
 *
 * // Register concerns
 * const dispose = store.useConcerns('test-id', {
 *   'user.age': {
 *     validationState: { schema: z.number().min(0) }
 *   }
 * })
 *
 * // Update state
 * store.proxy.user.age = 31
 * await waitForEffects()
 *
 * // Check concern results
 * const concerns = store.getFieldConcerns('user.age')
 * expect(concerns.validationState.isError).toBe(false)
 *
 * // Cleanup
 * dispose()
 * ```
 *
 * @param initialData - Initial state data
 * @param concerns - Map of concern name to concern implementation
 * @returns Test store with proxy, useConcerns, and getFieldConcerns
 */
export const createTestStore = <T extends object>(
  initialData: T,
  concerns: Record<string, ConcernType>,
): TestStore<T> => {
  const dataProxy = proxy<T>(initialData)
  const concernsRegistry = new Map<string, ConcernRegistration[]>()
  const evaluationCache = new Map<string, any>()

  const useConcerns = (id: string, registration: Record<string, any>) => {
    const disposeCallbacks: (() => void)[] = []

    Object.entries(registration).forEach(([path, concernConfigs]) => {
      if (!concernConfigs) return

      Object.entries(concernConfigs).forEach(([concernName, config]) => {
        if (!config) return

        const concern = concerns[concernName]
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
