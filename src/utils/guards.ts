/**
 * Runtime Guards and Validation
 *
 * Centralized validation guards for development-time checks.
 * Guards are always called at call sites — they self-gate internally.
 * Dev-only guards are tree-shaken in production builds.
 */

/**
 * Development mode flag.
 *
 * - Bundlers (Vite, Next.js, Webpack, esbuild) replace at build time
 * - typeof check prevents ReferenceError in edge runtimes
 * - Evaluates to `false` in production → dead-code eliminated
 */
export const __DEV__ =
  typeof process !== 'undefined' &&
  typeof process.env !== 'undefined' &&
  process.env.NODE_ENV !== 'production'

/**
 * Centralized guard object — always call, internally decides whether to act.
 *
 * - `anonymousFn`: Always runs (anonymous fns break ID generation)
 * - `duplicateId`: Dev-only (production uses counter fallback)
 * - `dynamicPath`: Always runs (dynamic paths are never valid at runtime)
 */
export const guard = {
  /**
   * Rejects anonymous listener functions.
   *
   * Always active — anonymous functions break ID generation regardless of env.
   *
   * @param fn - The listener function to validate
   * @param path - The path the listener is being registered at
   * @throws Error if function is anonymous or has no name
   *
   * @example
   * ```typescript
   * guard.anonymousFn(function validateAge() {}, 'user.age') // OK
   * guard.anonymousFn(() => {}, 'cart.items') // throws
   * ```
   */
  anonymousFn: (fn: (...args: unknown[]) => unknown, path: string): void => {
    if (!fn.name || fn.name === 'anonymous') {
      throw new Error(
        `Listener fn must be a named function. Got anonymous fn at path '${path}'.`,
      )
    }
  },

  /**
   * Rejects duplicate listener IDs in development mode.
   *
   * Dev-only — in production, callers use counter fallback for minified names.
   *
   * @param id - The listener ID to check
   * @param existingIds - Set of already-registered listener IDs
   * @throws Error if ID already exists (dev mode only)
   *
   * @example
   * ```typescript
   * const ids = new Set(['a.b.c_validateAge'])
   * guard.duplicateId('a.b.c_validateAge', ids) // throws in dev
   * ```
   */
  duplicateId: (
    id: string,
    existingIds: { has(key: string): boolean },
  ): void => {
    if (!__DEV__) return
    if (existingIds.has(id)) {
      throw new Error(
        `Duplicate listener ID: '${id}'. Each path+fn.name must be unique.`,
      )
    }
  },

  /**
   * Rejects paths with dynamic hash key notation [*].
   *
   * Always active — [*] is a type-level marker, never valid at runtime.
   *
   * @param path - The path to validate
   * @throws Error if path contains [*]
   *
   * @example
   * ```typescript
   * guard.dynamicPath('users.u1.posts.p1') // OK
   * guard.dynamicPath('users.[*].posts') // throws
   * ```
   */
  /**
   * Validates that scope is a parent/ancestor of path.
   *
   * Always active — invalid scope/path relationships break listener routing.
   *
   * @param path - The listener watch path
   * @param scope - The listener scope (determines what state fn receives)
   * @throws Error if scope is not a parent/ancestor of path
   *
   * @example
   * ```typescript
   * guard.listenerScope('a.b.c', 'a.b') // OK (parent)
   * guard.listenerScope('a.b.c', 'a.b.c') // OK (same)
   * guard.listenerScope('a.b.c', null) // OK (null scope)
   * guard.listenerScope('a.b.c', '1.2.3') // throws
   * ```
   */
  listenerScope: (
    path: string | null,
    scope: string | null | undefined,
  ): void => {
    // If either is null/undefined, validation passes
    if (path == null || scope == null) return

    // If scope === path, that's valid
    if (path === scope) return

    // Scope must be a prefix of path (parent/ancestor)
    if (!path.startsWith(scope + '.')) {
      throw new Error(
        `Invalid listener: scope '${scope}' must be a parent/ancestor of path '${path}', or one must be null`,
      )
    }
  },

  dynamicPath: <P extends string>(path: P): void => {
    const parts = path.split('.')
    for (const part of parts) {
      if (part === '[*]') {
        throw new Error(
          `Path contains [*] hash key which is not allowed in store operations. Use concrete paths only.`,
        )
      }
    }
  },
} as const

/**
 * Generates a unique listener ID from path and function name.
 *
 * Format:
 * - Normal: {path}_{fn.name}
 * - Root:   __{fn.name} (double underscore for empty path)
 * - Collision (prod only): {path}_{fn.name}_{counter}
 *
 * @param path - The path the listener is watching ('' for root)
 * @param fn - The listener function
 * @param existingIds - Set of already-registered listener IDs
 * @returns Unique listener ID
 * @throws Error if function is anonymous or ID is duplicate (dev mode only)
 *
 * @example
 * ```typescript
 * const ids = new Set<string>()
 *
 * generateListenerId('a.b.c', function validateAge() {}, ids)
 * // → 'a.b.c_validateAge'
 *
 * generateListenerId('', function rootAudit() {}, ids)
 * // → '__rootAudit'
 *
 * // Production mode with minified collision
 * ids.add('a.b_a')
 * generateListenerId('a.b', function a() {}, ids)
 * // → 'a.b_a_1'
 * ```
 */
export const generateListenerId = (
  path: string,
  fn: (...args: unknown[]) => unknown,
  existingIds: { has(key: string): boolean },
): string => {
  // Step 1: Validate function is named (always active)
  guard.anonymousFn(fn, path)

  // Step 2: Generate base ID
  const baseId = (path === '' ? '__' : path + '_') + fn.name

  // Step 3: Check uniqueness
  if (!existingIds.has(baseId)) {
    return baseId
  }

  // Step 4: Development mode - reject duplicates
  guard.duplicateId(baseId, existingIds)

  // Step 5: Production mode - append counter to disambiguate
  let counter = 1
  while (existingIds.has(baseId + '_' + counter)) {
    counter++
  }

  return baseId + '_' + counter
}
