import { useCallback, useMemo } from 'react'

/**
 * Transform configuration for field values
 */
export interface TransformConfig<TStored, TDisplay> {
  /** Transform from stored value to display value */
  to: (stored: TStored) => TDisplay
  /** Transform from display value to stored value */
  from: (display: TDisplay) => TStored
}

/**
 * Minimal field interface that useTransformedField accepts
 */
export interface FieldInput<T> {
  value: T
  setValue: (v: T) => void
}

/**
 * Adds value transformation to any field hook.
 * Converts between storage format and display format.
 * Passes through any additional properties from the input field.
 *
 * @param field - Field hook with { value, setValue, ...rest }
 * @param config - Transform functions { to, from }
 * @returns Field with transformed types, plus any passthrough props
 *
 * @example
 * ```typescript
 * const field = useFieldStore('user.birthdate')
 * const formatted = useTransformedField(field, {
 *   to: (iso) => format(new Date(iso), 'MM/dd/yyyy'),
 *   from: (display) => parse(display, 'MM/dd/yyyy').toISOString()
 * })
 *
 * // formatted.value is "01/15/2024"
 * // formatted.setValue("01/20/2024") stores ISO string
 * ```
 *
 * @example
 * ```typescript
 * // Works with buffered fields - passes through commit/cancel/isDirty
 * const field = useFieldStore('price')
 * const buffered = useBufferedField(field)
 * const formatted = useTransformedField(buffered, {
 *   to: (cents) => (cents / 100).toFixed(2),
 *   from: (dollars) => Math.round(parseFloat(dollars) * 100)
 * })
 *
 * formatted.setValue("15.99")  // local only
 * formatted.commit()           // stores 1599
 * ```
 */
export const useTransformedField = <
  TStored,
  TDisplay,
  TField extends FieldInput<TStored>,
>(
  field: TField,
  config: TransformConfig<TStored, TDisplay>,
): Omit<TField, 'value' | 'setValue'> & FieldInput<TDisplay> => {
  const { value: storedValue, setValue: setStoredValue, ...rest } = field
  const { to, from } = config

  // Transform stored to display
  const value = useMemo(() => to(storedValue), [storedValue, to])

  // Transform display to stored on set
  const setValue = useCallback(
    (displayValue: TDisplay) => {
      const storedVal = from(displayValue)
      setStoredValue(storedVal)
    },
    [from, setStoredValue],
  )

  return { ...rest, value, setValue } as Omit<TField, 'value' | 'setValue'> &
    FieldInput<TDisplay>
}
