/**
 * Boolean logic evaluation utilities
 *
 * Core utilities for evaluating conditional expressions against state.
 * Used by concerns and side effects for reactive condition checking.
 */

import _get from 'lodash/get'
import _gt from 'lodash/gt'
import _gte from 'lodash/gte'
import _includes from 'lodash/includes'
import _isEmpty from 'lodash/isEmpty'
import _isNil from 'lodash/isNil'
import _isNumber from 'lodash/isNumber'
import _lt from 'lodash/lt'
import _lte from 'lodash/lte'

import type { BoolLogic, ComparableValue } from '../types'

/**
 * Check if a value is considered empty
 * - null/undefined: empty
 * - string: empty if length === 0
 * - array: empty if length === 0
 * - object: empty if no own enumerable keys
 * - other primitives (number, boolean): not empty
 */
const isValueEmpty = (value: unknown): boolean => {
  if (_isNil(value)) return true
  if (_isNumber(value) || typeof value === 'boolean') return false
  return _isEmpty(value)
}

/**
 * Evaluate a numeric comparison safely
 * Returns false if the value is not a number
 */
const evaluateNumericComparison = (
  value: unknown,
  threshold: number,
  comparator: (a: number, b: number) => boolean,
): boolean => _isNumber(value) && comparator(value, threshold)

/**
 * Evaluate a BoolLogic expression against state
 *
 * Supports:
 * - IS_EQUAL: value === expected
 * - EXISTS: value is not null/undefined
 * - IS_EMPTY: string/array/object is empty
 * - AND/OR/NOT: boolean combinators
 * - GT/LT/GTE/LTE: numeric comparisons
 * - IN: value in allowed list
 *
 * Automatically tracks any state properties accessed during evaluation
 * when wrapped in effect().
 *
 * @example
 * const logic = { IS_EQUAL: ['user.role', 'admin'] }
 * evaluateBoolLogic(logic, state) // true if state.user.role === 'admin'
 */
export const evaluateBoolLogic = <STATE extends object>(
  logic: BoolLogic<STATE>,
  state: STATE,
): boolean => {
  // Equality check
  if ('IS_EQUAL' in logic) {
    const [path, expected] = logic.IS_EQUAL
    return _get(state, path) === expected
  }

  // Existence check (not null/undefined)
  if ('EXISTS' in logic) {
    return !_isNil(_get(state, logic.EXISTS))
  }

  // Emptiness check
  if ('IS_EMPTY' in logic) {
    return isValueEmpty(_get(state, logic.IS_EMPTY))
  }

  // Boolean combinators
  if ('AND' in logic) {
    return logic.AND.every((subLogic) => evaluateBoolLogic(subLogic, state))
  }
  if ('OR' in logic) {
    return logic.OR.some((subLogic) => evaluateBoolLogic(subLogic, state))
  }
  if ('NOT' in logic) {
    return !evaluateBoolLogic(logic.NOT, state)
  }

  // Numeric comparisons using lodash
  if ('GT' in logic) {
    const [path, threshold] = logic.GT
    return evaluateNumericComparison(_get(state, path), threshold, _gt)
  }
  if ('LT' in logic) {
    const [path, threshold] = logic.LT
    return evaluateNumericComparison(_get(state, path), threshold, _lt)
  }
  if ('GTE' in logic) {
    const [path, threshold] = logic.GTE
    return evaluateNumericComparison(_get(state, path), threshold, _gte)
  }
  if ('LTE' in logic) {
    const [path, threshold] = logic.LTE
    return evaluateNumericComparison(_get(state, path), threshold, _lte)
  }

  // Inclusion check
  if ('IN' in logic) {
    const [path, allowed] = logic.IN
    return _includes(allowed, _get(state, path) as ComparableValue)
  }

  return false
}
