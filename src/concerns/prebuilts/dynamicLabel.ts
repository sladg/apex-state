/**
 * Dynamic label template concern
 *
 * Interpolates a template string with values from state.
 * Automatically tracks all state paths referenced in the template.
 *
 * Returns the interpolated string.
 *
 * @example
 * ```typescript
 * store.useConcerns('my-concerns', {
 *   'priceField': {
 *     dynamicLabel: { template: "Price: ${{product.price}}" }
 *   }
 * })
 * // Result: "Price: $99.99"
 * ```
 */

import type { ConcernType } from '../types'
import { interpolateTemplate } from '../../utils/interpolation'

export const dynamicLabel: ConcernType<{ template: string }, string> = {
  name: 'dynamicLabel',
  description: 'Template string interpolation for labels',
  evaluate: (props) => {
    return interpolateTemplate(props.template, props.state)
  }
}
