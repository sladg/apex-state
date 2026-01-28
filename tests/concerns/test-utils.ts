/**
 * Test utilities for concerns benchmarking and tracking
 *
 * Provides performance measurement and evaluation tracking for P0 test suite
 */

import { vi } from 'vitest'

/**
 * Performance benchmarking harness
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
    zodValidation: vi.fn(),
    disabled: vi.fn(),
    tooltip: vi.fn(),
    computed: vi.fn(),
    clear: function () {
      this.zodValidation.mockClear()
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
