/**
 * String interpolation examples — template helpers for dynamic text concerns.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import { extractPlaceholders, interpolateTemplate } from '@sladg/apex-state'

// @llms-example: Extract {{path}} placeholders and interpolate templates with state values
const template = 'Current value: {{legs.0.strike}}'

// Extract placeholder paths from a template
const paths = extractPlaceholders(template)
// → ['legs.0.strike']

// Interpolate template with state values
const state = { legs: [{ strike: 105 }] }
const result = interpolateTemplate(template, state)
// → "Current value: 105"
// @llms-example-end

void paths
void result
