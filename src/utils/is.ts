/**
 * Type checking utilities — similar to lodash type guards
 *
 * Provides type-safe predicates for common type checks with TypeScript support
 */
import type { Primitive } from '../types/deep-key'

/** Check if value is null or undefined */
const isNil = (value: unknown): value is null | undefined => value == null

/** Check if value is undefined */
const isUndefined = (value: unknown): value is undefined => value === undefined

/** Check if value is null */
const isNull = (value: unknown): value is null => value === null

/** Check if value is a plain object (not null, array, Date, RegExp, class instances, etc.) */
const isObject = (value: unknown): value is Record<string, unknown> => {
  if (value == null || typeof value !== 'object' || Array.isArray(value))
    return false
  const proto = Object.getPrototypeOf(value)
  return proto === Object.prototype || proto === null
}

/** Check if value is an array */
const isArray = (value: unknown): value is unknown[] => Array.isArray(value)

/** Check if value is a plain object or an array */
const isObjectOrArray = (
  value: unknown,
): value is Record<string, unknown> | unknown[] =>
  isObject(value) || isArray(value)

/** Check if value is a string */
const isString = (value: unknown): value is string => typeof value === 'string'

/** Check if value is a number */
const isNumber = (value: unknown): value is number => typeof value === 'number'

/** Check if value is a boolean */
const isBoolean = (value: unknown): value is boolean =>
  typeof value === 'boolean'

/** Check if value is a function */
const isFunction = (value: unknown): value is (...args: unknown[]) => unknown =>
  typeof value === 'function'

/** Check if value is a symbol */
const isSymbol = (value: unknown): value is symbol => typeof value === 'symbol'

/** Check if value is a Date */
const isDate = (value: unknown): value is Date => value instanceof Date

/** Check if value is a RegExp */
const isRegExp = (value: unknown): value is RegExp => value instanceof RegExp

/** Check if a string key is a non-negative integer index (e.g. "0", "1", "42") */
const isNumericKey = (value: string): boolean => /^\d+$/.test(value)

/** Check if value is a primitive (string, number, boolean, symbol, bigint, null, undefined) */
const isPrimitive = (value: unknown): value is Primitive => {
  const type = typeof value
  return (
    type === 'string' ||
    type === 'number' ||
    type === 'boolean' ||
    type === 'symbol' ||
    type === 'bigint' ||
    value == null
  )
}

/** Check if object is empty (has no own properties) */
const isEmptyObject = (value: object): boolean => {
  for (const key in value) {
    if (Object.prototype.hasOwnProperty.call(value, key)) {
      return false
    }
  }
  return true
}

/** Check if value is empty (null, undefined, empty string, empty array, empty object) */
const isEmpty = (value: unknown): boolean => {
  if (isNil(value)) return true
  if (isNumber(value) || isBoolean(value)) return false
  if (isString(value)) return value.length === 0
  if (isArray(value)) return value.length === 0
  if (isObject(value)) return isEmptyObject(value as object)
  // Date, RegExp, Map, Set, class instances, functions, symbols — not empty
  return false
}

const isEqualArray = (a: any[], b: any[]): boolean => {
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; i++) {
    if (!iEqual(a[i], b[i])) return false
  }
  return true
}

const isEqualObject = (a: any, b: any): boolean => {
  const keysA = Object.keys(a)
  const keysB = Object.keys(b)

  if (keysA.length !== keysB.length) return false

  for (const key of keysA) {
    if (!Object.prototype.hasOwnProperty.call(b, key)) return false
    if (!iEqual(a[key], b[key])) return false
  }

  return true
}

/** Check for deep equality between two values */
const iEqual = (a: unknown, b: unknown): boolean => {
  if (a === b) return true

  if (isArray(a) && isArray(b)) {
    return isEqualArray(a, b)
  }

  if (isDate(a) && isDate(b)) {
    return a.getTime() === b.getTime()
  }

  if (isRegExp(a) && isRegExp(b)) {
    return a.toString() === b.toString()
  }

  if (isObject(a) && isObject(b)) {
    return isEqualObject(a, b)
  }

  return false
}

// Negated type guards (with proper type narrowing)
/** Check if value is not null or undefined */
const isNotNil = <T>(value: T | null | undefined): value is T => value != null

/** Check if value is not undefined */
const isNotUndefined = <T>(value: T | undefined): value is T =>
  value !== undefined

/** Check if value is not null */
const isNotNull = <T>(value: T | null): value is T => value !== null

/** Check if value is not a plain object */
const isNotObject = <T>(
  value: T,
): value is Exclude<T, Record<string, unknown>> => !isObject(value)

/** Check if value is not a plain object and not an array */
const isNotObjectOrArray = <T>(
  value: T,
): value is Exclude<T, Record<string, unknown> | unknown[]> =>
  !isObject(value) && !isArray(value)

/** Check if value is not an array */
const isNotArray = <T>(value: T): value is Exclude<T, unknown[]> =>
  !isArray(value)

/** Check if value is not a string */
const isNotString = <T>(value: T): value is Exclude<T, string> =>
  typeof value !== 'string'

/** Check if value is not a number */
const isNotNumber = <T>(value: T): value is Exclude<T, number> =>
  typeof value !== 'number'

/** Check if value is not a boolean */
const isNotBoolean = <T>(value: T): value is Exclude<T, boolean> =>
  typeof value !== 'boolean'

/** Check if value is not a function */
const isNotFunction = <T>(
  value: T,
): value is Exclude<T, (...args: unknown[]) => unknown> =>
  typeof value !== 'function'

/** Check if value is not a symbol */
const isNotSymbol = <T>(value: T): value is Exclude<T, symbol> =>
  typeof value !== 'symbol'

/** Check if value is not a Date */
const isNotDate = <T>(value: T): value is Exclude<T, Date> =>
  !(value instanceof Date)

/** Check if value is not a RegExp */
const isNotRegExp = <T>(value: T): value is Exclude<T, RegExp> =>
  !(value instanceof RegExp)

/** Check if value is not a primitive */
const isNotPrimitive = <T>(value: T): value is Exclude<T, Primitive> => {
  const type = typeof value
  return !(
    type === 'string' ||
    type === 'number' ||
    type === 'boolean' ||
    type === 'symbol' ||
    type === 'bigint' ||
    value == null
  )
}

/** Check if value is not empty */
const isNotEmpty = (value: unknown): boolean => !isEmpty(value)

/** Check for deep inequality between two values */
const isNotEqual = (a: unknown, b: unknown): boolean => !iEqual(a, b)

/**
 * Unified namespace for type checking
 *
 * @example
 * ```typescript
 * import { is } from './utils/is'
 *
 * if (is.object(value)) { ... }
 * if (is.array(value)) { ... }
 * if (is.nil(value)) { ... }
 *
 * // Negated versions
 * if (is.not.object(value)) { ... }
 * if (is.not.array(value)) { ... }
 * ```
 */
export const is = {
  nil: isNil,
  undefined: isUndefined,
  null: isNull,
  object: isObject,
  objectOrArray: isObjectOrArray,
  array: isArray,
  string: isString,
  number: isNumber,
  boolean: isBoolean,
  function: isFunction,
  symbol: isSymbol,
  date: isDate,
  regexp: isRegExp,
  numericKey: isNumericKey,
  primitive: isPrimitive,
  empty: isEmpty,
  equal: iEqual,
  not: {
    nil: isNotNil,
    undefined: isNotUndefined,
    null: isNotNull,
    object: isNotObject,
    objectOrArray: isNotObjectOrArray,
    array: isNotArray,
    string: isNotString,
    number: isNotNumber,
    boolean: isNotBoolean,
    function: isNotFunction,
    symbol: isNotSymbol,
    date: isNotDate,
    regexp: isNotRegExp,
    primitive: isNotPrimitive,
    empty: isNotEmpty,
    equal: isNotEqual,
  },
}
