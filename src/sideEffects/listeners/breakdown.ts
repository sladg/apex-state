/**
 * Smart Change Breakdown for Listener Triggering
 *
 * Breaks down object changes into nested path changes ONLY when listeners exist.
 * Uses lazy breakdown - only goes as deep as necessary based on registered listeners.
 *
 * Example:
 *   Change: ['user', { name: 'John', age: 30 }, {}]
 *   If listener exists for 'user.name' → adds ['user.name', 'John', {}]
 *   If NO listener for 'user.age' → does NOT add ['user.age', 30, {}]
 */

import type { ArrayOfChanges, DeepKey, GenericMeta } from '../../types'
import type { ListenersRegistry } from './registry'

/**
 * Smart breakdown of changes for listener triggering
 *
 * This function is ONLY used for determining which listeners to invoke.
 * It does NOT affect state updates - state is updated with original changes.
 *
 * Breakdown rules:
 * 1. Only break down if listeners exist at nested levels
 * 2. Break down to level + 1 if value is object
 * 3. Stop if value is primitive (cannot break down further)
 * 4. Recursively break down nested objects if needed
 *
 * @param changes - Original changes from setChanges
 * @param registry - Listener registry for path lookup
 * @param state - Current state (unused, kept for future optimization)
 * @returns Extended array of changes including virtual nested changes
 *
 * @example
 * ```typescript
 * const registry = createListenersRegistry<AppState, Meta>()
 * registry.register('name-listener', 'user.name', () => {})
 *
 * const changes: ArrayOfChanges<AppState, Meta> = [
 *   ['user', { name: 'Alice', age: 30 }, {}]
 * ]
 *
 * const broken = breakdownChanges(changes, registry, state)
 * // Result:
 * // [
 * //   ['user', { name: 'Alice', age: 30 }, {}],
 * //   ['user.name', 'Alice', {}]  // Only this because listener exists
 * // ]
 * // Note: 'user.age' NOT included because no listener exists
 * ```
 */
export function breakdownChanges<
  DATA extends object,
  META extends GenericMeta
>(
  changes: ArrayOfChanges<DATA, META>,
  registry: ListenersRegistry<DATA, META>,
  _state: DATA
): ArrayOfChanges<DATA, META> {
  const result: ArrayOfChanges<DATA, META> = [...changes]
  const processed = new Set<string>()

  // Process each change
  for (const [path, value, meta] of changes) {
    breakdownSingle(path, value, meta, result, registry, processed)
  }

  return result
}

/**
 * Recursively breaks down a single change
 */
function breakdownSingle<DATA extends object, META extends GenericMeta>(
  path: DeepKey<DATA>,
  value: any,
  meta: META,
  result: ArrayOfChanges<DATA, META>,
  registry: ListenersRegistry<DATA, META>,
  processed: Set<string>
): void {
  const pathStr = path as string

  // Avoid duplicate processing
  if (processed.has(pathStr)) return
  processed.add(pathStr)

  // Check if there are listeners for nested paths
  if (!registry.hasListenerForNestedPath(pathStr)) {
    return // No nested listeners, skip breakdown
  }

  // Only break down objects (not primitives, not arrays)
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    return
  }

  // Break down to level + 1
  for (const key of Object.keys(value)) {
    const nestedPath = `${pathStr}.${key}` as DeepKey<DATA>
    const nestedValue = value[key]

    // Only add if listener exists at this level OR deeper
    const hasListener = registry.hasListenerForPath(nestedPath as string)
    const hasNestedListener = registry.hasListenerForNestedPath(
      nestedPath as string
    )

    if (hasListener || hasNestedListener) {
      result.push([nestedPath, nestedValue, meta])

      // Recursively break down further if needed
      if (hasNestedListener) {
        breakdownSingle(nestedPath, nestedValue, meta, result, registry, processed)
      }
    }
  }
}
