/**
 * A placeholder maps a JS value (e.g. `undefined`) to a JSON-encoded string
 * sentinel (e.g. `'"__APEX_UNDEFINED__"'`) so values that cannot be represented
 * in JSON survive a round-trip through stringify → parse.
 */
export interface Placeholder {
  value: unknown
  /** The exact JSON-encoded string to use (include surrounding quotes for string sentinels). */
  encoded: string
}

/**
 * Create a symmetric fast stringify/parse pair with optional placeholder substitutions.
 *
 * Bypasses JSON.stringify / JSON.parse for primitives (number, boolean, null) — ~4.7x faster.
 * Placeholders are checked first via O(1) Map lookup before the fast-path logic.
 * See docs/BENCHMARK_JSON_SERIALIZATION.md for performance data.
 *
 * @example
 * const { stringify, parse } = createFastJson([
 *   { value: undefined, encoded: '"__UNDEFINED__"' },
 * ])
 */
export const createFastJson = (placeholders: Placeholder[] = []) => {
  const encodeMap = new Map<unknown, string>()
  const decodeMap = new Map<string, unknown>()

  for (const p of placeholders) {
    encodeMap.set(p.value, p.encoded)
    decodeMap.set(p.encoded, p.value)
  }

  const stringify = (value: unknown): string => {
    if (encodeMap.has(value)) return encodeMap.get(value)!
    if (typeof value === 'number' || typeof value === 'boolean')
      return String(value)
    if (value === null) return 'null'
    return JSON.stringify(value)
  }

  const parse = (json: string): unknown => {
    if (decodeMap.has(json)) return decodeMap.get(json)
    const c = json.charCodeAt(0)
    // Numbers: starts with 0-9 or minus sign
    if ((c >= 48 && c <= 57) || c === 45) return Number(json)
    if (json === 'true') return true
    if (json === 'false') return false
    if (json === 'null') return null
    return JSON.parse(json) as unknown
  }

  return { stringify, parse }
}
