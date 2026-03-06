/**
 * Filters a concerns map to only include entries for the given keys.
 * Paths with no matching keys are omitted entirely.
 */
export const filterConcernsByKeys = (
  raw: Record<string, Record<string, unknown>>,
  keys: string[],
): Record<string, Record<string, unknown>> => {
  const result: Record<string, Record<string, unknown>> = {}
  for (const [path, pathConcerns] of Object.entries(raw)) {
    const filtered: Record<string, unknown> = {}
    for (const key of keys) {
      if (Object.prototype.hasOwnProperty.call(pathConcerns, key)) {
        filtered[key] = pathConcerns[key]
      }
    }
    if (Object.keys(filtered).length > 0) result[path] = filtered
  }
  return result
}
