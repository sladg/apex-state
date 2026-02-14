/**
 * JS-side string interning utility
 * Maintains a Map<string, number> synchronized with WASM intern table
 */

/**
 * Path string → ID mapping
 * Populated at registration time to avoid repeated WASM calls
 */
const pathToId = new Map<string, number>()

/**
 * Intern a path string and cache the result
 * Lazy-loads WASM module on first use
 */
export const internPath = async (path: string): Promise<number> => {
  // Check cache first
  const cached = pathToId.get(path)
  if (cached !== undefined) {
    return cached
  }

  // Import WASM module and intern
  const wasm = await import('../../rust/pkg/apex_state_wasm.js')
  const id = wasm.intern(path)

  // Cache the result
  pathToId.set(path, id)

  return id
}

/**
 * Batch intern multiple paths for efficiency
 * Returns IDs in the same order as input paths
 */
export const internPaths = async (paths: string[]): Promise<number[]> => {
  const wasm = await import('../../rust/pkg/apex_state_wasm.js')

  const ids: number[] = []

  for (const path of paths) {
    // Use cache if available
    const cached = pathToId.get(path)
    if (cached !== undefined) {
      ids.push(cached)
    } else {
      // Intern new path
      const id = wasm.intern(path)
      pathToId.set(path, id)
      ids.push(id)
    }
  }

  return ids
}

/**
 * Get the cached ID for a path without interning
 * Returns undefined if path hasn't been interned yet
 */
export const getPathId = (path: string): number | undefined => {
  return pathToId.get(path)
}

/**
 * Clear the JS-side cache
 * IMPORTANT: Should be paired with WASM intern_clear() for tests
 */
export const clearInternCache = (): void => {
  pathToId.clear()
}

/**
 * Get the number of cached path IDs
 */
export const getInternCacheSize = (): number => {
  return pathToId.size
}
