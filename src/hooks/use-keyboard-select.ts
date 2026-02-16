import { useCallback, useRef } from 'react'

/**
 * Option for keyboard selection
 */
export interface SelectOption<T> {
  value: T
  label: string
}

/**
 * Configuration for keyboard select behavior
 */
export interface KeyboardSelectConfig<T> {
  /** Available options to select from */
  options: SelectOption<T>[]
  /** Time window to accumulate keystrokes (ms). Default: 500 */
  debounceMs?: number
  /** Match from start of label only. Default: true */
  matchFromStart?: boolean
}

/**
 * Minimal field interface that useKeyboardSelect accepts
 */
export interface FieldInput<T> {
  value: T
  setValue: (v: T) => void
}

/**
 * Adds keyboard-driven selection to any field hook.
 * Typing letters auto-selects matching options.
 *
 * @param field - Field hook with { value, setValue, ...rest }
 * @param config - Options and behavior configuration
 * @returns Field with onKeyDown handler added
 *
 * @example
 * ```typescript
 * const field = useFieldStore('user.country')
 * const { onKeyDown, ...rest } = useKeyboardSelect(field, {
 *   options: [
 *     { value: 'us', label: 'United States' },
 *     { value: 'uk', label: 'United Kingdom' },
 *     { value: 'ca', label: 'Canada' },
 *   ]
 * })
 *
 * // User types "u" -> selects "United States"
 * // User types "un" quickly -> still "United States"
 * // User types "c" -> selects "Canada"
 *
 * <input onKeyDown={onKeyDown} {...rest} />
 * ```
 */
export const useKeyboardSelect = <T, TField extends FieldInput<T>>(
  field: TField,
  config: KeyboardSelectConfig<T>,
): TField & { onKeyDown: (e: React.KeyboardEvent) => void } => {
  const { setValue } = field
  const { options, debounceMs = 500, matchFromStart = true } = config

  // Accumulated search string
  const searchRef = useRef('')
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)

  const onKeyDown = useCallback(
    (e: React.KeyboardEvent) => {
      // Only handle printable characters
      if (e.key.length !== 1) return

      // Don't interfere with modifier keys
      if (e.ctrlKey || e.metaKey || e.altKey) return

      const char = e.key.toLowerCase()

      // Clear previous timeout
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current)
      }

      // Accumulate search string
      searchRef.current += char

      // Find matching option
      const search = searchRef.current
      const match = options.find((opt) => {
        const label = opt.label.toLowerCase()
        return matchFromStart
          ? label.startsWith(search)
          : label.includes(search)
      })

      if (match) {
        setValue(match.value)
      }

      // Reset search after debounce
      timeoutRef.current = setTimeout(() => {
        searchRef.current = ''
      }, debounceMs)
    },
    [options, setValue, debounceMs, matchFromStart],
  )

  return { ...field, onKeyDown }
}
