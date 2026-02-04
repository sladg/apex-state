/**
 * Template string interpolation utilities
 *
 * Core utility for interpolating state values into template strings.
 * Used by concerns and side effects for dynamic text generation.
 */

import { dot } from './dot'
import { is } from './is'

/**
 * Extract all {{path}} placeholders from a template string
 *
 * @param template The template string with {{path}} placeholders
 * @returns Array of path strings found in the template
 *
 * @example
 * extractPlaceholders("Hello {{user.name}}, you have {{count}} messages")
 * // ["user.name", "count"]
 */
export const extractPlaceholders = (template: string): string[] => {
  const regex = /\{\{([^}]+)\}\}/g
  const matches: string[] = []
  let match: RegExpExecArray | null

  while ((match = regex.exec(template)) !== null) {
    if (match[1]) {
      matches.push(match[1])
    }
  }

  return matches
}

/**
 * Interpolate {{path}} placeholders with values from state
 *
 * Replaces {{path.to.value}} with actual values from state.
 * Only replaces if value is a string, number, or boolean.
 * Missing/null/undefined/object values leave the original {{path}} for debugging.
 *
 * @param template The template string with {{path}} placeholders
 * @param state The state object to read values from
 * @returns The interpolated string
 *
 * @example
 * interpolateTemplate("Value is {{market.spot}}", state)
 * // "Value is 105"
 *
 * @example
 * interpolateTemplate("Hello {{user.name}}, missing: {{invalid.path}}", state)
 * // "Hello Alice, missing: {{invalid.path}}"
 */
export const interpolateTemplate = <STATE extends object>(
  template: string,
  state: STATE,
): string => {
  return template.replace(/\{\{([^}]+)\}\}/g, (match, path) => {
    const value = dot.get__unsafe(state, path)

    // Only interpolate serializable primitives
    if (is.string(value)) return value
    if (is.number(value)) return String(value)
    if (is.boolean(value)) return String(value)

    // Leave original {{path}} for debugging (null, undefined, objects, arrays)
    return match
  })
}
