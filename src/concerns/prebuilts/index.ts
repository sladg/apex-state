/**
 * Pre-built concerns provided by apex-state
 *
 * Validation & Schema:
 * - validationState: Schema validation (isError, errors, message, timestamp)
 *
 * Conditional UI State (uses BoolLogic):
 * - disabledWhen: Disable field when condition is true
 * - readonlyWhen: Make field read-only when condition is true
 * - visibleWhen: Show field when condition is true
 *
 * Dynamic Text (uses template interpolation):
 * - dynamicTooltip: Template-based tooltip text
 * - dynamicLabel: Template-based label text
 * - dynamicPlaceholder: Template-based placeholder text
 */

// Validation
import { disabledWhen } from './disabled-when'
import { dynamicLabel } from './dynamic-label'
import { dynamicPlaceholder } from './dynamic-placeholder'
import { dynamicTooltip } from './dynamic-tooltip'
import { readonlyWhen } from './readonly-when'
import { validationState } from './validation-state'
import { visibleWhen } from './visible-when'

export {
  type ValidationError,
  validationState,
  type ValidationStateConcern,
  type ValidationStateInput,
  type ValidationStateResult,
} from './validation-state'

// Conditional state
export { disabledWhen } from './disabled-when'
export { readonlyWhen } from './readonly-when'
export { visibleWhen } from './visible-when'

// Dynamic text
export { dynamicLabel } from './dynamic-label'
export { dynamicPlaceholder } from './dynamic-placeholder'
export { dynamicTooltip } from './dynamic-tooltip'

/**
 * All pre-built concerns as a tuple (for use with findConcern)
 */
export const prebuilts = [
  validationState,
  disabledWhen,
  readonlyWhen,
  visibleWhen,
  dynamicTooltip,
  dynamicLabel,
  dynamicPlaceholder,
] as const

/**
 * Namespace style access for pre-builts
 */
export const prebuiltsNamespace = {
  validationState,
  disabledWhen,
  readonlyWhen,
  visibleWhen,
  dynamicTooltip,
  dynamicLabel,
  dynamicPlaceholder,
}
