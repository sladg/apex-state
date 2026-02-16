/**
 * ArrayOfChanges type
 *
 * Represents an array of changes with paths, values, and metadata.
 * Each change is a tuple of [path, value, metadata].
 *
 * @example
 * ```typescript
 * type User = {
 *   name: string
 *   age: number
 * }
 *
 * const changes: ArrayOfChanges<User, GenericMeta> = [
 *   ["name", "John", { sender: "user-123" }],
 *   ["age", 30, { isProgramaticChange: true }]
 * ]
 * ```
 */

import type { DeepKey } from './deep-key'
import type { DeepValue } from './deep-value'
import type { GenericMeta } from './meta'

/**
 * Represents an array of change tuples.
 * Each tuple contains:
 * - path: A valid deep key path for the data structure
 * - value: The value at that path (properly typed based on the path)
 * - meta: Metadata about the change
 */
export type ArrayOfChanges<DATA, META extends GenericMeta = GenericMeta> = {
  [K in DeepKey<DATA>]: [K, DeepValue<DATA, K>, META]
}[DeepKey<DATA>][]
