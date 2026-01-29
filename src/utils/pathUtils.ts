/**
 * Path utility functions
 *
 * Common path manipulation utilities used across the codebase.
 * Centralized here to avoid duplication.
 */

/**
 * Calculate the depth of a dot-notation path
 *
 * @example
 * getPathDepth('') // 0
 * getPathDepth('user') // 1
 * getPathDepth('user.profile.name') // 3
 */
export const getPathDepth = (path: string): number => {
  if (!path) return 0
  return path.split('.').length
}
