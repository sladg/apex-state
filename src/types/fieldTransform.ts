/**
 * Configuration for field transformations with bidirectional mapping
 * Used by useFieldTransformedStore to handle display vs storage format differences
 *
 * @example
 * ```typescript
 * const config: FieldTransformConfig<string, string> = {
 *   toTemporary: (iso) => format(new Date(iso), 'MM/DD/YYYY'),
 *   fromTemporary: (display) => parse(display, 'MM/DD/YYYY').toISOString()
 * }
 * ```
 */
export interface FieldTransformConfig<VAL, CTX> {
  /**
   * Transform from stored value to temporary (display) value
   */
  toTemporary: (val: VAL, context?: CTX) => CTX

  /**
   * Transform from temporary (display) value to stored value
   */
  fromTemporary: (ctx: CTX, context?: CTX) => VAL

  /**
   * Optional context for transformations (e.g., locale, format options)
   */
  context?: CTX
}
