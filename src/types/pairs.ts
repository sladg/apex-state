/**
 * Pair WASM boundary conversion helpers
 *
 * Mirrors the pattern of `changes.ts` — converts typed pair arrays
 * to the flat string-tuple format expected by WASM registration calls.
 *
 * The optional BoolLogic condition (always the last element) is JSON-serialized
 * since WASM receives it as a string for serde deserialization.
 */

import type {
  AggregationPair,
  ComputationPair,
  SyncPair,
} from './paths-of-same-value'
import type {
  ValidatedAggregationPairs,
  ValidatedComputationPairs,
  ValidatedSyncPairs,
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

/**
 * Split sync pairs into bidirectional and directed (oneWay) groups for WASM registration.
 * Pairs without `oneWay` go into bidirectional (existing sync graph).
 * Pairs with `oneWay: '[0]->[1]'` → [pair[0], pair[1]] directed edge.
 * Pairs with `oneWay: '[1]->[0]'` → [pair[1], pair[0]] directed edge (reversed).
 */
const syncToWasm = <DATA extends object>(
  input: SyncPair<DATA>[] | ValidatedSyncPairs<DATA>,
): { bidirectional: [string, string][]; directed: [string, string][] } => {
  const bidirectional: [string, string][] = []
  const directed: [string, string][] = []
  for (const pair of input as (
    | [string, string]
    | [string, string, { oneWay: '[0]->[1]' | '[1]->[0]' }]
  )[]) {
    if (pair.length === 3) {
      const dir = pair[2].oneWay
      directed.push(
        dir === '[0]->[1]' ? [pair[0], pair[1]] : [pair[1], pair[0]],
      )
    } else {
      bidirectional.push([pair[0], pair[1]])
    }
  }
  return { bidirectional, directed }
}

/** WASM boundary conversion utilities for pair arrays. */
export const pairs = { aggregationToWasm, computationToWasm, syncToWasm }
