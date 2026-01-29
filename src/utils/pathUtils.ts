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

/**
 * Extract relative path from full path given a listener scope prefix
 *
 * @example
 * getRelativePath('user.profile.name', 'user.profile') // 'name'
 * getRelativePath('a.b.c.d', 'a.b') // 'c.d'
 */
export const getRelativePath = (
  changePath: string,
  listenerPath: string,
): string => {
  // Remove the listener path prefix + '.' to get relative path
  // e.g., 'user.profile.name' - 'user.profile.' = 'name'
  return changePath.slice(listenerPath.length + 1)
}
