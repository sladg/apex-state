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

  // DeepPartial<T> has the same keys as T when T extends object.
  // Cast to a known-keyed type so TypeScript allows indexing both objects.
  const src = source as { [K in keyof T]?: unknown }

  for (const key in src) {
    if (!Object.prototype.hasOwnProperty.call(src, key)) continue

    const k = key as Extract<keyof T, string>
    const sourceValue = src[k]
    const targetValue = target[k]

    if (is.undefined(sourceValue)) {
      continue
    }

    if (is.object(sourceValue) && is.object(targetValue)) {
      result[k] = deepMerge(
        targetValue,
        sourceValue as DeepPartial<typeof targetValue>,
      ) as T[typeof k]
    } else {
      result[k] = sourceValue as T[typeof k]
    }
  }

  return result
}
