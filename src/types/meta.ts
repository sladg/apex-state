/**
 * GenericMeta interface
 *
 * Base metadata type for change tracking with standard properties.
 * Can be extended with custom properties for specific use cases.
 *
 * @example
 * ```typescript
 * interface CustomMeta extends GenericMeta {
 *   timestamp: number
 *   userId: string
 * }
 * ```
 */
export interface GenericMeta {
  /**
   * Indicates if the change originated from a sync path side-effect.
   * Used to track cascading changes triggered by synchronization logic.
   */
  isSyncPathChange?: boolean

  /**
   * Indicates if the change originated from a flip path side-effect.
   * Used to track bidirectional synchronization changes.
   */
  isFlipPathChange?: boolean

  /**
   * Indicates if the change was triggered programmatically rather than by user action.
   * Useful for distinguishing between automated and manual state updates.
   */
  isProgramaticChange?: boolean

  /**
   * Indicates if the change originated from an aggregation side-effect.
   * Used to track changes triggered by aggregation logic.
   */
  isAggregationChange?: boolean

  /**
   * Indicates if the change originated from a listener side-effect.
   * Used to track changes triggered by listener callbacks.
   */
  isListenerChange?: boolean

  /**
   * Indicates if the change originated from a clear paths side-effect.
   * Used to track changes triggered by clear paths logic (WASM-only).
   */
  isClearPathChange?: boolean

  /**
   * Indicates if the change originated from a computation side-effect (SUM/AVG).
   * Used to track changes triggered by computation logic.
   */
  isComputationChange?: boolean

  /**
   * Identifies the originator of the change.
   * Can be a user ID, component name, or any identifier string.
   */
  sender?: string
}
