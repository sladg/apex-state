import type { BoolLogic } from '../types'
import { dot } from './dot'
import { is } from './is'

const evaluateNumericComparison = <STATE extends object>(
  logic: BoolLogic<STATE>,
  state: STATE,
): boolean | undefined => {
  if ('GT' in logic) {
    const [path, threshold] = logic.GT
    const value = dot.get__unsafe(state, path)
    return is.number(value) && value > threshold
  }
  if ('LT' in logic) {
    const [path, threshold] = logic.LT
    const value = dot.get__unsafe(state, path)
    return is.number(value) && value < threshold
  }
  if ('GTE' in logic) {
    const [path, threshold] = logic.GTE
    const value = dot.get__unsafe(state, path)
    return is.number(value) && value >= threshold
  }
  if ('LTE' in logic) {
    const [path, threshold] = logic.LTE
    const value = dot.get__unsafe(state, path)
    return is.number(value) && value <= threshold
  }
  return undefined
}

export const evaluateBoolLogic = <STATE extends object>(
  logic: BoolLogic<STATE>,
  state: STATE,
): boolean => {
  // Shorthand tuple: [path, value]
  if (Array.isArray(logic)) {
    const [path, expected] = logic as [string, unknown]
    return dot.get__unsafe(state, path) === expected
  }

  // Equality check
  if ('IS_EQUAL' in logic) {
    const [path, expected] = logic.IS_EQUAL as [string, unknown]
    return dot.get__unsafe(state, path) === expected
  }

  // Existence check (not null/undefined)
  if ('EXISTS' in logic) {
    return is.not.nil(dot.get__unsafe(state, logic.EXISTS))
  }

  // Emptiness check
  if ('IS_EMPTY' in logic) {
    return is.empty(dot.get__unsafe(state, logic.IS_EMPTY))
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

  // Numeric comparisons
  const numericResult = evaluateNumericComparison(logic, state)
  if (is.not.undefined(numericResult)) {
    return numericResult
  }

  // Inclusion check
  if ('IN' in logic) {
    const [path, allowed] = logic.IN as [string, unknown[]]
    return allowed.includes(dot.get__unsafe(state, path))
  }

  return false
}
