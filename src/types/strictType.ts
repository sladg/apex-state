/**
 * Strict Type Safety Constraints
 *
 * Utilities to prevent `any` types from being used with createGenericStore.
 * Uses depth-limited recursion to detect `any` at any nesting level.
 *
 * Three layers of protection:
 * 1. Constraint: `DATA extends object & NoAnyDeep<DATA>` — maps any props to never
 * 2. Parameter: `IsAny<DATA> extends true ? never : unknown` — catches top-level any
 * 3. Return type: `ContainsAny<DATA> extends true ? AnyDetected : typeof returnVal`
 */

/**
 * Detects if a type is `any` using TypeScript's structural typing.
 *
 * `1 & T` normally narrows to `1`, so `0 extends 1` is false.
 * But `any` absorbs intersections — `1 & any` becomes `any`,
 * and `0 extends any` is true.
 */
export type IsAny<T> = 0 extends 1 & T ? true : false

/**
 * Depth-limited recursion via tuple countdown.
 * Depth[3] → 2 → 1 → 0 → never (stop).
 */
type Depth = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

/**
 * Recursive check: does T contain `any` at any depth (up to 3 levels)?
 *
 * Uses mapped type + indexed access to collect results for all known keys.
 * Index signatures (Record<string, X>) are handled naturally by keyof.
 */
export type ContainsAny<T, D extends number = 15> = [D] extends [never]
  ? false
  : IsAny<T> extends true
    ? true
    : T extends object
      ? true extends {
          [K in keyof T]: ContainsAny<T[K], Depth[D]>
        }[keyof T]
        ? true
        : false
      : false

/**
 * Constraint: maps `any` properties to `never` at any depth.
 * Since `any` is not assignable to `never`, TS rejects it.
 * Naturally preserves index signatures.
 */
export type NoAnyDeep<T, D extends number = 3> = [D] extends [never]
  ? T
  : {
      [K in keyof T]: IsAny<T[K]> extends true
        ? never
        : T[K] extends object
          ? NoAnyDeep<T[K], Depth[D]>
          : T[K]
    }

/**
 * Error marker type returned when `any` is detected in DATA.
 * Provides a readable compile-time error instead of cryptic type failures.
 */
export interface AnyDetected {
  readonly __error: 'DATA type parameter contains `any` — use concrete types instead'
}
