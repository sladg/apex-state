import { useCallback, useEffect, useRef } from 'react'

/**
 * Minimal field interface for throttling
 * Supports setValue with optional additional arguments (e.g., meta)
 */
export interface ThrottleFieldInput<T, Args extends unknown[] = unknown[]> {
  value: T
  setValue: (v: T, ...args: Args) => void
}

/**
 * Throttle configuration
 */
export interface ThrottleConfig {
  /** Minimum milliseconds between setValue calls to the underlying field */
  ms: number
}

/**
 * Internal state for throttle tracking
 */
interface ThrottleState<T, Args extends unknown[]> {
  timeoutId: ReturnType<typeof setTimeout> | null
  lastFlushTime: number
  pendingValue: T | undefined
  pendingArgs: Args | undefined
  hasPending: boolean
}

/**
 * Adds throttling to any field hook.
 * First setValue executes immediately, subsequent calls are rate-limited.
 * Last value wins when multiple calls occur within the throttle window.
 * Preserves the full setValue signature including additional arguments like meta.
 *
 * @param field - Field hook with { value, setValue, ...rest }
 * @param config - Throttle configuration { ms }
 * @returns Field with throttled setValue, plus any passthrough props
 *
 * @example
 * ```typescript
 * const field = useFieldStore('spotPrice')
 * const throttled = useThrottledField(field, { ms: 100 })
 *
 * // Rapid updates from WebSocket
 * throttled.setValue(1.234)  // Immediate
 * throttled.setValue(1.235)  // Buffered
 * throttled.setValue(1.236)  // Buffered (replaces 1.235)
 * // After 100ms: 1.236 is applied
 * ```
 *
 * @example
 * ```typescript
 * // With meta argument
 * const field = useFieldStore('price')
 * const throttled = useThrottledField(field, { ms: 100 })
 * throttled.setValue(42, { source: 'websocket' })
 * ```
 *
 * @example
 * ```typescript
 * // Composable with other wrappers
 * const field = useFieldStore('price')
 * const transformed = useTransformedField(field, {
 *   to: (cents) => cents / 100,
 *   from: (dollars) => Math.round(dollars * 100)
 * })
 * const throttled = useThrottledField(transformed, { ms: 50 })
 * ```
 */
export const useThrottledField = <
  T,
  Args extends unknown[] = unknown[],
  TField extends ThrottleFieldInput<T, Args> = ThrottleFieldInput<T, Args>,
>(
  field: TField,
  config: ThrottleConfig,
): TField => {
  const { setValue: originalSetValue, ...rest } = field
  const { ms } = config

  const throttleRef = useRef<ThrottleState<T, Args>>({
    timeoutId: null,
    lastFlushTime: 0,
    pendingValue: undefined,
    pendingArgs: undefined,
    hasPending: false,
  })

  const setValue = useCallback(
    (newValue: T, ...args: Args) => {
      const now = Date.now()
      const ref = throttleRef.current
      const timeSinceLastFlush = now - ref.lastFlushTime

      // If enough time has passed, flush immediately
      if (timeSinceLastFlush >= ms) {
        ref.lastFlushTime = now
        originalSetValue(newValue, ...args)
        return
      }

      // Buffer the value and args (last value wins)
      ref.pendingValue = newValue
      ref.pendingArgs = args
      ref.hasPending = true

      // Schedule flush if not already scheduled
      if (!ref.timeoutId) {
        const remainingTime = ms - timeSinceLastFlush
        ref.timeoutId = setTimeout(() => {
          ref.timeoutId = null
          ref.lastFlushTime = Date.now()
          if (ref.hasPending) {
            originalSetValue(
              ref.pendingValue as T,
              ...(ref.pendingArgs as Args),
            )
            ref.hasPending = false
            ref.pendingValue = undefined
            ref.pendingArgs = undefined
          }
        }, remainingTime)
      }
    },
    [originalSetValue, ms],
  )

  // Cleanup timeout on unmount
  useEffect(() => {
    return () => {
      if (throttleRef.current.timeoutId) {
        clearTimeout(throttleRef.current.timeoutId)
      }
    }
  }, [])

  return { ...rest, setValue } as TField
}
