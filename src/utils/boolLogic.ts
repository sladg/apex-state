/**
 * Boolean logic evaluation utilities
 *
 * Core utilities for evaluating conditional expressions against state.
 * Used by concerns and side effects for reactive condition checking.
 */

import { deepGet } from '../store/utils/deepAccess'
import type { BoolLogic } from '../concerns/types'

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
  state: STATE
): boolean => {
  if ('IS_EQUAL' in logic) {
    const [path, expected] = logic.IS_EQUAL
    const actual = deepGet(state, path as any)
    return actual === expected
  }

  if ('EXISTS' in logic) {
    const value = deepGet(state, logic.EXISTS as any)
    return value !== undefined && value !== null
  }

  if ('IS_EMPTY' in logic) {
    const value = deepGet(state, logic.IS_EMPTY as any)
    if (value === undefined || value === null) return true
    if (typeof value === 'string') return value.length === 0
    if (Array.isArray(value)) return value.length === 0
    if (typeof value === 'object') return Object.keys(value).length === 0
    return false
  }

  if ('AND' in logic) {
    return logic.AND.every(subLogic => evaluateBoolLogic(subLogic, state))
  }

  if ('OR' in logic) {
    return logic.OR.some(subLogic => evaluateBoolLogic(subLogic, state))
  }

  if ('NOT' in logic) {
    return !evaluateBoolLogic(logic.NOT, state)
  }

  if ('GT' in logic) {
    const [path, threshold] = logic.GT
    const value = deepGet(state, path as any)
    return typeof value === 'number' && value > threshold
  }

  if ('LT' in logic) {
    const [path, threshold] = logic.LT
    const value = deepGet(state, path as any)
    return typeof value === 'number' && value < threshold
  }

  if ('GTE' in logic) {
    const [path, threshold] = logic.GTE
    const value = deepGet(state, path as any)
    return typeof value === 'number' && value >= threshold
  }

  if ('LTE' in logic) {
    const [path, threshold] = logic.LTE
    const value = deepGet(state, path as any)
    return typeof value === 'number' && value <= threshold
  }

  if ('IN' in logic) {
    const [path, allowed] = logic.IN
    const value = deepGet(state, path as any)
    return allowed.includes(value)
  }

  return false
}
