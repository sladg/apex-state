/**
 * Pair WASM boundary conversion helpers
 *
 * Mirrors the pattern of `changes.ts` — converts typed pair arrays
 * to the flat string-tuple format expected by WASM registration calls.
 *
 * The optional BoolLogic condition (always the last element) is JSON-serialized
 * since WASM receives it as a string for serde deserialization.
 */

import type { AggregationPair, ComputationPair } from './paths-of-same-value'
import type {
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
} from './validated-pairs'

/**
 * Convert aggregation pairs to WASM format.
 * Serializes the optional BoolLogic condition to a JSON string.
 */
const aggregationToWasm = <DATA extends object>(
  input: AggregationPair<DATA>[] | ValidatedAggregationPairs<DATA>,
): ([string, string] | [string, string, string])[] =>
  (input as ([string, string] | [string, string, unknown])[]).map(
    ([target, source, condition]) =>
      condition !== undefined
        ? [target, source, JSON.stringify(condition)]
        : [target, source],
  ) as ([string, string] | [string, string, string])[]

/**
 * Convert computation pairs to WASM format.
 * Serializes the optional BoolLogic condition to a JSON string.
 */
const computationToWasm = <DATA extends object>(
  input: ComputationPair<DATA>[] | ValidatedComputationPairs<DATA>,
): ([string, string, string] | [string, string, string, string])[] =>
  (
    input as ([string, string, string] | [string, string, string, unknown])[]
  ).map(([op, target, source, condition]) =>
    condition !== undefined
      ? [op, target, source, JSON.stringify(condition)]
      : [op, target, source],
  ) as ([string, string, string] | [string, string, string, string])[]

/** WASM boundary conversion utilities for pair arrays. */
export const pairs = { aggregationToWasm, computationToWasm }
