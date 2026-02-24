/**
 * Lazy listener validators — O(N) base + O(K) per-listener validation
 *
 * These types power the `listeners()` helper function that scales to large state
 * types without hitting TS2589.
 *
 * Unlike the direct `ListenerRegistration<DATA>[]` type which resolves
 * `DeepKey<DATA>` for path/scope on every element, these validators only evaluate
 * `DeepValue` for the K listeners you actually write.
 *
 * Valid listener combinations:
 * 1. `{ path: string, fn }` — scope defaults to path → scoped state
 * 2. `{ path: string, scope: null, fn }` — explicit null scope → full DATA
 * 3. `{ path: string, scope: string, fn }` — scope must be prefix of path
 * 4. `{ path: null, fn }` — watch everything, scope defaults to null → full DATA
 * 5. `{ path: null, scope: null, fn }` — same as above, explicit
 *
 * **Inline fn restriction**: Fns with untyped (any) parameters are rejected.
 * Use `OnStateListener<DATA, ScopedState>` to explicitly type your fn.
 */

import type { OnStateListener } from '../core/types'
import type { DefaultDepth } from './deep-key'
import type { DeepValue } from './deep-value'
import type { GenericMeta } from './meta'

/**
 * True if Prefix is a dot-separated ancestor of Path (or equals Path).
 *
 * @example
 * IsPrefixOf<'user', 'user'>          → true   (equal)
 * IsPrefixOf<'user', 'user.name'>     → true   (parent)
 * IsPrefixOf<'user', 'username'>      → false  (different segment)
 * IsPrefixOf<'cart', 'user.name'>     → false  (unrelated)
 */
export type IsPrefixOf<
  Prefix extends string,
  Path extends string,
> = Path extends Prefix
  ? true
  : Path extends `${Prefix}.${string}`
    ? true
    : false

/**
 * Derive the sub-state type from scope.
 * - null scope → full DATA
 * - string scope → DeepValue<DATA, Scope>
 *
 * Uses `Scope & string` to satisfy DeepValue's string constraint
 * when Scope is inferred from a mapped type.
 */
type ScopedState<DATA extends object, Scope> = Scope extends null
  ? DATA
  : Scope extends string
    ? DeepValue<DATA, Scope & string>
    : DATA

/**
 * Extract the effective scope from a listener element.
 *
 * When 'scope' key exists with a non-undefined value, returns that value.
 * When 'scope' key is missing or undefined, returns the path as default.
 */
type EffectiveScope<T, P> = 'scope' extends keyof T
  ? Exclude<T[keyof T & 'scope'], undefined> extends never
    ? P // scope is undefined → default to path
    : Exclude<T[keyof T & 'scope'], undefined> // use explicit scope value
  : P // no scope key → default to path

/**
 * Detect if a function's first parameter is `any`.
 * Uses the `0 extends 1 & T` trick: only true when T is `any`.
 *
 * When true, the fn has untyped parameters (e.g., `(_changes, _state) => ...`
 * without explicit typing). We reject these to force users to use
 * `OnStateListener<DATA, ScopedState>` for type safety.
 */
type HasAnyFirstParam<F> = F extends (...args: infer A) => any
  ? A extends [infer P1, ...any[]]
    ? 0 extends 1 & P1
      ? true
      : false
    : false
  : false

/**
 * Error marker type for untyped listener fns.
 * Produces a clear error message when the user passes an untyped inline fn.
 */
type TypedFnRequired<
  DATA extends object,
  SUB_STATE,
  META extends GenericMeta,
> = OnStateListener<DATA, SUB_STATE, META> & {
  __error: 'Listener fn must be explicitly typed. Use OnStateListener<DATA, ScopedState> or type the parameters directly.'
}

/**
 * Returns the fn type for a listener element, rejecting untyped fns.
 *
 * When the user's fn has `any` params (implicit from inference), returns
 * a branded error type that produces a clear type error.
 * When the fn is explicitly typed (standalone or annotated), returns the
 * expected OnStateListener type for assignability checking.
 */
type ValidatedFn<DATA extends object, META extends GenericMeta, SUB_STATE, F> =
  HasAnyFirstParam<F> extends true
    ? TypedFnRequired<DATA, SUB_STATE, META>
    : OnStateListener<DATA, SUB_STATE, META>

/**
 * Validates a single listener element.
 *
 * When scope is omitted (undefined), it defaults to path:
 * - path is string → fn gets DeepValue<DATA, path>
 * - path is null → fn gets full DATA
 *
 * When scope is explicitly provided:
 * - scope: null → fn gets full DATA
 * - scope: string → must be prefix of path, fn gets DeepValue<DATA, scope>
 *
 * Rejects fns with untyped (any) parameters — forces explicit typing via
 * OnStateListener<DATA, ScopedState>.
 */
export type CheckListenerElement<
  DATA extends object,
  META extends GenericMeta,
  T,
> = T extends { path: infer P; fn: infer F }
  ? EffectiveScope<T, P> extends infer ES
    ? // Both path and effective scope are strings
      [P, ES] extends [string, string]
      ? IsPrefixOf<ES & string, P & string> extends true
        ? // Scope explicitly provided as string
          'scope' extends keyof T
          ? {
              path: P
              scope: ES
              fn: ValidatedFn<DATA, META, ScopedState<DATA, ES>, F>
            }
          : // Scope omitted — keep optional
            {
              path: P
              scope?: undefined
              fn: ValidatedFn<DATA, META, ScopedState<DATA, ES>, F>
            }
        : { path: P; scope: never; fn: OnStateListener<DATA, never, META> }
      : // Path is string, effective scope is null → fn gets full DATA
        [P, ES] extends [string, null]
        ? {
            path: P
            scope: null
            fn: ValidatedFn<DATA, META, DATA, F>
          }
        : // Path is null, effective scope is null → fn gets full DATA
          [P, ES] extends [null, null]
          ? // Scope explicitly provided?
            'scope' extends keyof T
            ? {
                path: null
                scope: null
                fn: ValidatedFn<DATA, META, DATA, F>
              }
            : {
                path: null
                scope?: undefined
                fn: ValidatedFn<DATA, META, DATA, F>
              }
          : T
    : T
  : T

/**
 * Validates an array of listener objects lazily — O(K) where K = listeners written.
 *
 * Per element:
 * 1. path must be ResolvableDeepKey<DATA> | null (enforced by function constraint)
 * 2. scope is optional — defaults to path when omitted
 * 3. When both non-null: scope must be prefix of path
 * 4. fn must be explicitly typed (rejects untyped `any` params)
 * 5. fn receives correctly-typed scoped state
 */
export type CheckListeners<
  DATA extends object,
  META extends GenericMeta,
  T extends readonly {
    path: string | null
    scope?: string | null | undefined
    fn: (...args: any[]) => any
  }[],
  _Depth extends number = DefaultDepth,
> = {
  [I in keyof T]: CheckListenerElement<DATA, META, T[I]>
}
