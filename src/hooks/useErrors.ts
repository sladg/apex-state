/**
 * useErrors Hook
 *
 * Retrieves validation errors for a specific path.
 * Built on top of useStore for reactive updates.
 */

import type { DeepKey } from '../types'
import type { StoredError } from '../sideEffects/validators/types'
import { useStore } from './useStore'

/**
 * Hook to retrieve validation errors for a specific path.
 *
 * Returns array of error messages from all validators targeting this path.
 * Reactive: re-renders when errors change.
 *
 * @param path - The data path to get errors for
 * @param errorStorePath - Root path for error storage (default: '_errors')
 * @returns Array of error messages (empty array if no errors)
 *
 * @example
 * ```typescript
 * function EmailField() {
 *   const [email, setEmail] = store.useStore('user.email')
 *   const errors = store.useErrors('user.email')
 *
 *   return (
 *     <div>
 *       <input value={email} onChange={e => setEmail(e.target.value)} />
 *       {errors.map((err, i) => <div key={i} className="error">{err}</div>)}
 *     </div>
 *   )
 * }
 * ```
 */
export const useErrors = <DATA extends object>(
  path: DeepKey<DATA>,
  errorStorePath = '_errors'
): string[] => {
  // Build full error path
  const errorPath = `${errorStorePath}.${path}` as DeepKey<DATA>

  // Use useStore to get reactive errors
  const [errors] = useStore(errorPath)

  // Return empty array if no errors
  if (!errors || !Array.isArray(errors)) {
    return []
  }

  // Extract messages from StoredError objects
  return (errors as StoredError[]).map(e => e.message)
}
