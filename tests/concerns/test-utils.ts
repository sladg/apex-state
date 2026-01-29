/**
 * Test utilities for concerns benchmarking and tracking
 *
 * Provides performance measurement and evaluation tracking for P0 test suite
 */

import { proxy } from 'valtio/vanilla'
import { effect } from 'valtio-reactive'
import { vi } from 'vitest'

/**
 * Performance benchmarking harness with round-trip tracking
 */
export class PerformanceBenchmark {
  private marks = new Map<string, number>()
  private measurements: { name: string; duration: number }[] = []

  start(label: string) {
    this.marks.set(label, performance.now())
  }

  end(label: string): number {
    const startTime = this.marks.get(label)
    if (!startTime) throw new Error(`No start mark for ${label}`)

    const duration = performance.now() - startTime
    this.measurements.push({ name: label, duration })
    this.marks.delete(label)

    return duration
  }

  report() {
    const summary = {
      count: this.measurements.length,
      total: this.measurements.reduce((sum, m) => sum + m.duration, 0),
      average:
        this.measurements.reduce((sum, m) => sum + m.duration, 0) /
        this.measurements.length,
      max: Math.max(...this.measurements.map((m) => m.duration)),
      min: Math.min(...this.measurements.map((m) => m.duration)),
    }

    return { measurements: this.measurements, summary }
  }

  clear() {
    this.marks.clear()
    this.measurements = []
  }
}

/**
 * Wait for a concern value to change and return time elapsed
 * Measures: state change → effects execute → value available in store
 */
export async function waitForConcernValue<T>(
  getConcernValue: () => T,
  expectedValue?: T,
  timeout = 1000,
): Promise<number> {
  const startTime = performance.now()
  const initialValue = getConcernValue()

  return new Promise((resolve, reject) => {
    const pollInterval = setInterval(() => {
      const currentValue = getConcernValue()
      const elapsed = performance.now() - startTime

      // Check if value changed (or matches expected value)
      const valueMatches =
        expectedValue !== undefined
          ? currentValue === expectedValue
          : currentValue !== initialValue

      if (valueMatches) {
        clearInterval(pollInterval)
        resolve(elapsed)
      }

      if (elapsed > timeout) {
        clearInterval(pollInterval)
        reject(
          new Error(
            `Timeout waiting for concern value after ${timeout}ms. Got: ${JSON.stringify(currentValue)}`,
          ),
        )
      }
    }, 0) // Poll as fast as possible
  })
}

/**
 * Evaluation tracking log entry
 */
export interface EvaluationLogEntry {
  concern: string
  path: string
  timestamp: number
}

/**
 * Create tracked concern evaluator
 */
export function createEvaluationTracker() {
  const evaluationLog: EvaluationLogEntry[] = []

  return {
    log: evaluationLog,
    track: (concernName: string, path: string) => {
      evaluationLog.push({
        concern: concernName,
        path,
        timestamp: performance.now(),
      })
    },
    clear: () => {
      evaluationLog.length = 0
    },
    filter: (predicate: (entry: EvaluationLogEntry) => boolean) => {
      return evaluationLog.filter(predicate)
    },
  }
}

/**
 * Create spies for concern evaluators
 */
export function createConcernSpies() {
  return {
    validationState: vi.fn(),
    disabled: vi.fn(),
    tooltip: vi.fn(),
    computed: vi.fn(),
    clear: function () {
      this.validationState.mockClear()
      this.disabled.mockClear()
      this.tooltip.mockClear()
      this.computed.mockClear()
    },
  }
}

/**
 * Wait for effects to settle
 */
export async function waitForEffects(ms = 10): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, ms))
}

/**
 * Helper to get deep value from object
 */
export function getDeepValue(obj: any, path: string): any {
  return path.split('.').reduce((acc, part) => acc?.[part], obj)
}

/**
 * Helper to evaluate boolean logic conditions
 */
export type BoolLogic =
  | { IS_EQUAL: [string, any] }
  | { AND: BoolLogic[] }
  | { OR: BoolLogic[] }
  | { NOT: BoolLogic }

export function evaluateBoolLogic(logic: BoolLogic, state: any): boolean {
  if ('IS_EQUAL' in logic) {
    const [path, value] = logic.IS_EQUAL
    return getDeepValue(state, path) === value
  }
  if ('AND' in logic) {
    return logic.AND.every((l: any) => evaluateBoolLogic(l, state))
  }
  if ('OR' in logic) {
    return logic.OR.some((l: any) => evaluateBoolLogic(l, state))
  }
  if ('NOT' in logic) {
    return !evaluateBoolLogic(logic.NOT, state)
  }
  return false
}

/**
 * Render tracking for React tests
 */
export function createRenderTracker() {
  const renderLog: { timestamp: number; [key: string]: any }[] = []

  return {
    log: renderLog,
    track: (data: Record<string, any>) => {
      renderLog.push({
        timestamp: performance.now(),
        ...data,
      })
    },
    clear: () => {
      renderLog.length = 0
    },
  }
}

/**
 * Core concern type interface
 */
export interface ConcernType {
  name: string
  evaluate: (props: any) => any
}

/**
 * Concern registration entry
 */
export interface ConcernRegistration {
  id: string
  path: string
  concernName: string
  concern: ConcernType
  config: any
  dispose: () => void
}

/**
 * Test store factory using real store implementation pattern
 *
 * Creates a store instance directly without React Provider,
 * following the same pattern as src/concerns/registration.ts
 *
 * This ensures performance tests validate the real implementation
 */
export const createTestStore = <T extends Record<string, any>>(
  initialData: T,
  concerns: Record<string, ConcernType>,
) => {
  const dataProxy = proxy<T>(initialData)
  const concernsProxy = proxy<Record<string, Record<string, any>>>({})

  const useConcerns = (id: string, registration: Record<string, any>) => {
    const disposeCallbacks: (() => void)[] = []
    const resultCache = new Map<string, unknown>()
    const concernRefs = new Map<string, Record<string, unknown>>()

    // Pre-initialize all path objects BEFORE creating effects (matching real implementation)
    Object.keys(registration).forEach((path) => {
      if (!concernsProxy[path]) {
        concernsProxy[path] = {}
      }
      concernRefs.set(path, concernsProxy[path])
    })

    // Register each concern with effect tracking
    Object.entries(registration).forEach(([path, concernConfigs]) => {
      if (!concernConfigs) return

      const concernsAtPath = concernRefs.get(path)!

      Object.entries(concernConfigs).forEach(([concernName, config]) => {
        if (!config) return

        const concern = concerns[concernName as keyof typeof concerns]
        if (!concern) return

        const cacheKey = `${path}.${concernName}`

        // Wrap evaluation in effect for automatic dependency tracking
        const dispose = effect(() => {
          const value = getDeepValue(dataProxy, path)

          // Evaluate concern with provided config
          const result = concern.evaluate({
            state: dataProxy,
            path,
            value,
            ...config,
          })

          // Check cache to avoid unnecessary updates
          const prev = resultCache.get(cacheKey)
          if (prev !== result) {
            resultCache.set(cacheKey, result)
            // Write to pre-captured reference (prevents tracked reads)
            concernsAtPath[concernName] = result
          }
        })

        disposeCallbacks.push(dispose)
      })
    })

    // Return cleanup function
    return () => {
      disposeCallbacks.forEach((dispose) => dispose())
      resultCache.clear()
      concernRefs.clear()
      // Clear concern values
      Object.keys(registration).forEach((path) => {
        Object.keys(registration[path] || {}).forEach((concernName) => {
          if (concernsProxy[path]) {
            Reflect.deleteProperty(concernsProxy[path], concernName)
          }
        })
      })
    }
  }

  const getFieldConcerns = (path: string) => {
    return concernsProxy[path] || {}
  }

  return {
    proxy: dataProxy,
    useConcerns,
    getFieldConcerns,
  }
}
