import { useCallback, useState } from 'react'

/**
 * Minimal field interface that useBufferedField accepts
 */
export interface FieldInput<T> {
  value: T
  setValue: (v: T) => void
}

/**
 * Extended interface with buffering capabilities
 */
export interface BufferedField<T> extends FieldInput<T> {
  commit: () => void
  cancel: () => void
  isDirty: boolean
}

/**
 * Adds buffered editing to any field hook.
 * Local changes are held until explicitly committed or cancelled.
 *
 * @param field - Field hook with { value, setValue }
 * @returns Buffered field with commit/cancel/isDirty
 *
 * @example
 * ```typescript
 * const field = useFieldStore('user.name')
 * const buffered = useBufferedField(field)
 *
 * // User types - updates local only
 * buffered.setValue('new value')
 *
 * // Enter/Tab - commit to store
 * buffered.commit()
 *
 * // Esc - revert to stored value
 * buffered.cancel()
 *
 * // Check if user has unsaved changes
 * if (buffered.isDirty) { ... }
 * ```
 */
export const useBufferedField = <T>(field: FieldInput<T>): BufferedField<T> => {
  const { value: storedValue, setValue: setStoredValue } = field

  // null = not editing, T = editing with this local value
  const [localValue, setLocalValue] = useState<T | null>(null)

  // Derive editing state from localValue
  const isEditing = localValue !== null
  const isDirty = isEditing && localValue !== storedValue

  // Update local value only (no store write)
  const setValue = useCallback((newValue: T) => {
    setLocalValue(newValue)
  }, [])

  // Commit local value to store
  const commit = useCallback(() => {
    if (localValue !== null) {
      setStoredValue(localValue)
    }
    setLocalValue(null)
  }, [localValue, setStoredValue])

  // Cancel and revert to stored value
  const cancel = useCallback(() => {
    setLocalValue(null)
  }, [])

  // Return current value: local if editing, otherwise stored
  const value = isEditing ? localValue : storedValue

  return { value, setValue, commit, cancel, isDirty }
}
