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

import type { Stage } from '../wasm/generated/types'
import type { DeepKey } from './deep-key'
import type { DeepValue } from './deep-value'
import type { GenericMeta } from './meta'

export type { Stage }

/**
 * Represents an array of change tuples.
 * Each tuple contains:
 * - path: A valid deep key path for the data structure
 * - value: The value at that path (properly typed based on the path)
 * - meta: Metadata about the change
 */
export type ArrayOfChanges<DATA, META extends GenericMeta = GenericMeta> = {
  [K in DeepKey<DATA>]: [K, DeepValue<DATA, K>, META] | [K, DeepValue<DATA, K>]
}[DeepKey<DATA>][]

/** A single state change in internal pipeline form. */
export interface Change {
  path: string
  value: unknown
  /** Meta object threaded through the pipeline. Lineage is merged in by the bridge on WASM→JS conversion. */
  meta: GenericMeta
}

// ---------------------------------------------------------------------------
// WASM boundary conversion helpers
// ---------------------------------------------------------------------------

/**
 * Convert a WASM Change array to pipeline change tuples, merging optional meta.
 * Paths from WASM are erased to `string` — the cast is required at the WASM boundary.
 */
const fromWasm = <DATA extends object, META extends GenericMeta = GenericMeta>(
  wasmChanges: Change[],
  meta?: META,
): ArrayOfChanges<DATA, META> =>
  wasmChanges.map((c) => [
    c.path,
    c.value,
    meta ?? ({} as META),
  ]) as unknown as ArrayOfChanges<DATA, META>

/**
 * Convert pipeline change tuples to WASM Change format.
 * Strips meta and casts paths to string — meta is JS-only and never crosses the WASM boundary.
 */
const toWasm = <DATA extends object, META extends GenericMeta>(
  tuples: ArrayOfChanges<DATA, META>,
): Change[] =>
  tuples.map(([path, value]) => ({
    path: path as string,
    value: value as unknown,
    meta: {},
  }))

/** WASM boundary conversion utilities. */
export const changes = { fromWasm, toWasm }
