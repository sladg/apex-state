/**
 * Utility functions
 *
 * Common utilities used across the codebase.
 */

export { evaluateBoolLogic } from './boolLogic'
export {
  deepEqual,
  deepGet,
  deepGetUnsafe,
  deepHas,
  deepSet,
} from './deepAccess'
export { detectGetters, extractGetters } from './deriveValues'
export { extractPlaceholders, interpolateTemplate } from './interpolation'
export { getPathDepth, getRelativePath } from './pathUtils'
