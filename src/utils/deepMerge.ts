import type { DeepPartial } from '../types'
import { is } from './is'

/**
 * Deep merge two objects, with source values overriding target values.
 * Only handles plain objects (not arrays, dates, etc.) - suitable for config merging.
 */
export const deepMerge = <T extends object>(
  target: T,
  source?: DeepPartial<T>,
): T => {
  if (!source) return target

  const result = { ...target }

  for (const key in source) {
    if (!Object.prototype.hasOwnProperty.call(source, key)) continue

    const sourceValue = source[key]
    const targetValue = target[key]

    if (is.undefined(sourceValue)) {
      continue
    }

    if (is.object(sourceValue) && is.object(targetValue)) {
      result[key] = deepMerge(
        targetValue,
        sourceValue as Partial<typeof targetValue>,
      ) as T[Extract<keyof T, string>]
    } else {
      result[key] = sourceValue as T[Extract<keyof T, string>]
    }
  }

  return result
}
