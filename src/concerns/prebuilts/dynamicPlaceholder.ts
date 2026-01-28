/**
 * Dynamic placeholder template concern
 *
 * Interpolates a template string with values from state.
 * Automatically tracks all state paths referenced in the template.
 *
 * Returns the interpolated string.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'inputField': {
 *     dynamicPlaceholder: { template: "Enter {{field.name}}" }
 *   }
 * })
 * // Result: "Enter email address"
 * ```
 */

import type { ConcernType } from '../types'
import { interpolateTemplate } from '../../utils/interpolation'

export const dynamicPlaceholder: ConcernType<{ template: string }, string> = {
  name: 'dynamicPlaceholder',
  description: 'Template string interpolation for placeholders',
  evaluate: (props) => {
    return interpolateTemplate(props.template, props.state)
  }
}
