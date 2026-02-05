/**
 * Debug Timing Utilities
 *
 * Provides timing measurement for concerns and listeners
 * to detect slow operations during development.
 */

type TimingType = 'concerns' | 'listeners' | 'registration'

interface TimingMeta {
  path: string
  name: string
}

export interface TimingEvent extends TimingMeta {
  type: TimingType
  duration: number
  threshold: number
}

export interface TimingSummary {
  type: TimingType
  totalDuration: number
  operationCount: number
  slowOperations: TimingEvent[]
}

export type OnSlowOperation = (event: TimingEvent) => void
export type OnTimingSummary = (summary: TimingSummary) => void

const defaultOnSlowOperation: OnSlowOperation = (event) => {
  console.warn(
    `[apex-state] Slow ${event.type}: ${event.path}/${event.name} took ${event.duration.toFixed(2)}ms (threshold: ${event.threshold}ms)`,
  )
}

const defaultOnTimingSummary: OnTimingSummary = (summary) => {
  if (summary.slowOperations.length > 0 || summary.totalDuration > 16) {
    console.warn(
      `[apex-state] ${summary.type}: ${summary.operationCount} ops in ${summary.totalDuration.toFixed(2)}ms (${summary.slowOperations.length} slow)`,
    )
  }
}

export interface Timing {
  run: <T>(type: TimingType, fn: () => T, meta: TimingMeta) => T
  reportBatch: (type: TimingType) => void
}

interface TimingConfig {
  timing: boolean
  timingThreshold: number
  onSlowOperation?: OnSlowOperation
  onSummary?: OnTimingSummary
}

interface TypeState {
  totalDuration: number
  operationCount: number
  slowOperations: TimingEvent[]
  warnedOperations: Set<string>
}

const createTypeState = (): TypeState => ({
  totalDuration: 0,
  operationCount: 0,
  slowOperations: [],
  warnedOperations: new Set(),
})

/**
 * Create a timing instance for the store.
 * If timing is disabled, all methods are no-ops.
 */
export const createTiming = (options: TimingConfig): Timing => {
  const {
    timing,
    timingThreshold,
    onSlowOperation = defaultOnSlowOperation,
    onSummary = defaultOnTimingSummary,
  } = options

  // Disabled: no-op implementation
  if (!timing) {
    return {
      run: (_type, fn) => fn(),
      reportBatch: () => {
        // Do nothing
      },
    }
  }

  const state: Record<TimingType, TypeState> = {
    concerns: createTypeState(),
    listeners: createTypeState(),
    registration: createTypeState(),
  }

  return {
    run: <T>(type: TimingType, fn: () => T, meta: TimingMeta): T => {
      const start = performance.now()
      const result = fn()
      const duration = performance.now() - start

      const typeState = state[type]
      typeState.totalDuration += duration
      typeState.operationCount++

      if (duration > timingThreshold) {
        const event: TimingEvent = {
          ...meta,
          type,
          duration,
          threshold: timingThreshold,
        }
        typeState.slowOperations.push(event)

        const key = `${type}:${meta.path}:${meta.name}`
        if (!typeState.warnedOperations.has(key)) {
          typeState.warnedOperations.add(key)
          onSlowOperation(event)
        }
      }

      return result
    },

    reportBatch: (type: TimingType) => {
      const typeState = state[type]
      onSummary({
        type,
        totalDuration: typeState.totalDuration,
        operationCount: typeState.operationCount,
        slowOperations: typeState.slowOperations,
      })

      // Reset batch counters but keep warned set to avoid spam
      typeState.totalDuration = 0
      typeState.operationCount = 0
      typeState.slowOperations = []
    },
  }
}
