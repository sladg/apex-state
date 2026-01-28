/**
 * Pre-built concerns provided by apex-state
 *
 * Validation & Schema:
 * - zodValidation: Schema validation with Zod
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
import { disabledWhen } from './disabledWhen'
import { dynamicLabel } from './dynamicLabel'
import { dynamicPlaceholder } from './dynamicPlaceholder'
import { dynamicTooltip } from './dynamicTooltip'
import { readonlyWhen } from './readonlyWhen'
import { validationState } from './validationState'
import { visibleWhen } from './visibleWhen'
import { zodValidation } from './zodValidation'

export {
  type ValidationError,
  validationState,
  type ValidationStateResult,
} from './validationState'
export { zodValidation } from './zodValidation'

// Conditional state
export { disabledWhen } from './disabledWhen'
export { readonlyWhen } from './readonlyWhen'
export { visibleWhen } from './visibleWhen'

// Dynamic text
export { dynamicLabel } from './dynamicLabel'
export { dynamicPlaceholder } from './dynamicPlaceholder'
export { dynamicTooltip } from './dynamicTooltip'

/**
 * All pre-built concerns as a tuple (for use with findConcern)
 */
export const prebuilts = [
  zodValidation,
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
  zodValidation,
  validationState,
  disabledWhen,
  readonlyWhen,
  visibleWhen,
  dynamicTooltip,
  dynamicLabel,
  dynamicPlaceholder,
}
