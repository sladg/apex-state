/**
 * Dynamic tooltip template concern
 *
 * Interpolates a template string with values from state.
 * Automatically tracks all state paths referenced in the template.
 *
 * Returns the interpolated string.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'strikePrice': {
 *     dynamicTooltip: { template: "Strike at {{market.spot}}" }
 *   }
 * })
 * // Result: "Strike at 105"
 * ```
 */

import type { ConcernType } from '../types'
import { interpolateTemplate } from '../../utils/interpolation'

export const dynamicTooltip: ConcernType<{ template: string }, string> = {
  name: 'dynamicTooltip',
  description: 'Template string interpolation for tooltips',
  evaluate: (props) => {
    return interpolateTemplate(props.template, props.state)
  }
}
