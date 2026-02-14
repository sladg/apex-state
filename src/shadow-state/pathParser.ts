/**
 * Path Parser for Shadow State
 *
 * Parses path strings with dot notation (a.b.c) and bracket notation (a[0].b)
 * into arrays of path segments for tree traversal.
 *
 * Uses caching for performance similar to dot.ts utilities.
 *
 * @module shadow-state/pathParser
 */

// Cache parsed paths to avoid repeated string parsing overhead
const pathCache = new Map<string, string[]>()
const MAX_CACHE_SIZE = 1000

/**
 * Helper: Process bracket notation and extract content
 *
 * @param path - Full path string
 * @param startIndex - Index of opening bracket
 * @returns Tuple of [bracketContent, nextIndex]
 */
const processBracket = (path: string, startIndex: number): [string, number] => {
  let i = startIndex + 1 // Skip opening bracket
  let bracketContent = ''

  while (i < path.length && path[i] !== ']') {
    bracketContent += path[i]
    i++
  }

  // Return content and index after closing bracket
  return [bracketContent, i + 1]
}

/**
 * Helper: Parse path string into segments (uncached)
 *
 * @param path - The path string to parse
 * @returns Array of path segments
 */
const parsePathUncached = (path: string): string[] => {
  if (!path) {
    return []
  }

  const segments: string[] = []
  let current = ''
  let i = 0

  while (i < path.length) {
    const char = path[i]!

    if (char === '.') {
      // Dot separator - push current segment if not empty
      if (current) {
        segments.push(current)
        current = ''
      }
      i++
    } else if (char === '[') {
      // Opening bracket - push current segment if not empty
      if (current) {
        segments.push(current)
        current = ''
      }

      // Process bracket notation
      const [bracketContent, nextIndex] = processBracket(path, i)
      if (bracketContent) {
        segments.push(bracketContent)
      }
      i = nextIndex
    } else if (char === ']') {
      // Closing bracket without opening - skip it
      i++
    } else {
      // Regular character - add to current segment
      current += char
      i++
    }
  }

  // Push final segment if not empty
  if (current) {
    segments.push(current)
  }

  return segments
}

/**
 * Parses a path string into an array of segments.
 *
 * Supports both dot notation and bracket notation:
 * - Dot notation: "user.profile.name" → ["user", "profile", "name"]
 * - Bracket notation: "todos[0].title" → ["todos", "0", "title"]
 * - Mixed notation: "users[0].profile.tags[1]" → ["users", "0", "profile", "tags", "1"]
 *
 * Path segments are cached for performance. The cache is cleared when it
 * reaches MAX_CACHE_SIZE to prevent memory leaks.
 *
 * @param path - The path string to parse (e.g., "user.profile.name" or "todos[0]")
 * @returns Array of path segments
 *
 * @example
 * ```typescript
 * parsePath('user.profile.name')
 * // Returns: ['user', 'profile', 'name']
 *
 * parsePath('todos[0].title')
 * // Returns: ['todos', '0', 'title']
 *
 * parsePath('items[0][1].value')
 * // Returns: ['items', '0', '1', 'value']
 *
 * parsePath('')
 * // Returns: []
 * ```
 */
export const parsePath = (path: string): string[] => {
  // Check cache first
  const cached = pathCache.get(path)
  if (cached !== undefined) {
    return cached
  }

  // Parse and cache
  const segments = parsePathUncached(path)

  // Cache the result
  if (pathCache.size >= MAX_CACHE_SIZE) {
    pathCache.clear()
  }
  pathCache.set(path, segments)

  return segments
}

/**
 * Joins path segments into a dot-notation string.
 *
 * Converts an array of path segments back into a string path.
 * Numeric segments are converted to bracket notation.
 *
 * @param segments - Array of path segments
 * @returns Dot-notation path string
 *
 * @example
 * ```typescript
 * joinPath(['user', 'profile', 'name'])
 * // Returns: 'user.profile.name'
 *
 * joinPath(['todos', '0', 'title'])
 * // Returns: 'todos[0].title'
 *
 * joinPath([])
 * // Returns: ''
 * ```
 */
export const joinPath = (segments: string[]): string => {
  if (segments.length === 0) {
    return ''
  }

  let result = ''

  for (let i = 0; i < segments.length; i++) {
    const segment = segments[i]!
    const isNumeric = /^\d+$/.test(segment)

    if (i === 0) {
      // First segment - no prefix
      result = segment
    } else if (isNumeric) {
      // Numeric segment - use bracket notation
      result += `[${segment}]`
    } else {
      // String segment - use dot notation
      result += `.${segment}`
    }
  }

  return result
}

/**
 * Checks if a path string is valid.
 *
 * A valid path:
 * - Can be empty (root path)
 * - Contains only alphanumeric characters, dots, and brackets
 * - Has balanced brackets
 * - Doesn't have consecutive dots
 *
 * @param path - The path string to validate
 * @returns True if the path is valid
 *
 * @example
 * ```typescript
 * isValidPath('user.profile.name')  // true
 * isValidPath('todos[0].title')     // true
 * isValidPath('invalid..path')      // false (consecutive dots)
 * isValidPath('unclosed[0')         // false (unbalanced brackets)
 * ```
 */
export const isValidPath = (path: string): boolean => {
  if (!path) {
    return true // Empty path is valid (root)
  }

  // Check for consecutive dots
  if (path.includes('..')) {
    return false
  }

  // Check for balanced brackets
  let bracketDepth = 0
  for (const char of path) {
    if (char === '[') {
      bracketDepth++
    } else if (char === ']') {
      bracketDepth--
      if (bracketDepth < 0) {
        return false // Closing bracket without opening
      }
    } else {
      // Other characters are allowed
    }
  }

  if (bracketDepth !== 0) {
    return false // Unbalanced brackets
  }

  return true
}

/**
 * Clears the path parser cache.
 *
 * Useful for testing or when memory usage is a concern.
 * The cache will be rebuilt automatically as paths are parsed.
 *
 * @example
 * ```typescript
 * clearCache()
 * ```
 */
export const clearCache = (): void => {
  pathCache.clear()
}
