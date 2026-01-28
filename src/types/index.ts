/**
 * Core type utilities for APEX state management
 *
 * This module exports advanced TypeScript type utilities for:
 * - Deep path access (DeepKey, DeepValue)
 * - Change tracking metadata (GenericMeta)
 * - Change arrays (ArrayOfChanges)
 * - Advanced type filters and mappings
 */

export type { DeepKey } from './deepKey'
export type { DeepValue } from './deepValue'
export type { GenericMeta } from './meta'
export type { ArrayOfChanges } from './changes'
export type { DeepKeyFiltered } from './deepKeyFiltered'
export type { PathsWithSameValueAs, SyncPair, FlipPair, AggregationPair } from './pathsOfSameValue'
export type { ExtractEvaluateReturn, EvaluatedConcerns } from './concerns'
export type { FieldTransformConfig } from './fieldTransform'
export type { ExtractPlaceholders, ValidatedTemplate } from './interpolation'
