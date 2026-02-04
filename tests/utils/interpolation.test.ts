/**
 * Tests for interpolation utility - Template string interpolation
 *
 * Tests:
 * - extractPlaceholders: Extract {{path}} placeholders from templates
 * - interpolateTemplate: Replace placeholders with state values
 */

import { describe, expect, it } from 'vitest'

import {
  extractPlaceholders,
  interpolateTemplate,
} from '../../src/utils/interpolation'

interface TestState {
  user: {
    name: string
    age: number
    isAdmin: boolean
    email: string | null
  }
  messages: {
    count: number
    unread: number
  }
  settings: {
    theme: string
    notifications: boolean
  }
  data: {
    items: string[]
    nested: {
      value: number
    }
  }
}

const createTestState = (): TestState => ({
  user: {
    name: 'Alice',
    age: 30,
    isAdmin: true,
    email: null,
  },
  messages: {
    count: 5,
    unread: 2,
  },
  settings: {
    theme: 'dark',
    notifications: false,
  },
  data: {
    items: ['a', 'b', 'c'],
    nested: {
      value: 42,
    },
  },
})

describe('extractPlaceholders', () => {
  it('should extract single placeholder', () => {
    expect(extractPlaceholders('Hello {{user.name}}')).toEqual(['user.name'])
  })

  it('should extract multiple placeholders', () => {
    expect(
      extractPlaceholders('{{user.name}} has {{messages.count}} messages'),
    ).toEqual(['user.name', 'messages.count'])
  })

  it('should return empty array for no placeholders', () => {
    expect(extractPlaceholders('Hello World')).toEqual([])
  })

  it('should handle deeply nested paths', () => {
    expect(extractPlaceholders('Value: {{data.nested.value}}')).toEqual([
      'data.nested.value',
    ])
  })

  it('should handle multiple placeholders on same path', () => {
    expect(
      extractPlaceholders('{{user.name}} and {{user.name}} again'),
    ).toEqual(['user.name', 'user.name'])
  })

  it('should handle adjacent placeholders', () => {
    expect(extractPlaceholders('{{user.name}}{{user.age}}')).toEqual([
      'user.name',
      'user.age',
    ])
  })

  it('should handle placeholders with numbers in path', () => {
    expect(extractPlaceholders('Item: {{items.0}}')).toEqual(['items.0'])
  })

  it('should handle empty string template', () => {
    expect(extractPlaceholders('')).toEqual([])
  })

  it('should not match incomplete placeholders', () => {
    expect(extractPlaceholders('{{incomplete')).toEqual([])
    expect(extractPlaceholders('incomplete}}')).toEqual([])
    expect(extractPlaceholders('{single}')).toEqual([])
  })

  it('should handle template with only placeholder', () => {
    expect(extractPlaceholders('{{path}}')).toEqual(['path'])
  })
})

describe('interpolateTemplate', () => {
  describe('string values', () => {
    it('should interpolate string values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Hello {{user.name}}!', state)).toBe(
        'Hello Alice!',
      )
    })

    it('should interpolate multiple string values', () => {
      const state = createTestState()
      expect(
        interpolateTemplate(
          '{{user.name}} uses {{settings.theme}} theme',
          state,
        ),
      ).toBe('Alice uses dark theme')
    })

    it('should handle empty string values', () => {
      const state = { value: '' }
      expect(interpolateTemplate('Value: {{value}}', state)).toBe('Value: ')
    })
  })

  describe('number values', () => {
    it('should interpolate number values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Age: {{user.age}}', state)).toBe('Age: 30')
    })

    it('should interpolate zero', () => {
      const state = { count: 0 }
      expect(interpolateTemplate('Count: {{count}}', state)).toBe('Count: 0')
    })

    it('should interpolate negative numbers', () => {
      const state = { value: -42 }
      expect(interpolateTemplate('Value: {{value}}', state)).toBe('Value: -42')
    })

    it('should interpolate decimal numbers', () => {
      const state = { price: 19.99 }
      expect(interpolateTemplate('Price: {{price}}', state)).toBe(
        'Price: 19.99',
      )
    })
  })

  describe('boolean values', () => {
    it('should interpolate true', () => {
      const state = createTestState()
      expect(interpolateTemplate('Admin: {{user.isAdmin}}', state)).toBe(
        'Admin: true',
      )
    })

    it('should interpolate false', () => {
      const state = createTestState()
      expect(
        interpolateTemplate('Notifications: {{settings.notifications}}', state),
      ).toBe('Notifications: false')
    })
  })

  describe('non-interpolatable values', () => {
    it('should leave placeholder for null values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Email: {{user.email}}', state)).toBe(
        'Email: {{user.email}}',
      )
    })

    it('should leave placeholder for undefined values', () => {
      const state = { value: undefined }
      expect(interpolateTemplate('Value: {{value}}', state)).toBe(
        'Value: {{value}}',
      )
    })

    it('should leave placeholder for missing paths', () => {
      const state = createTestState()
      expect(interpolateTemplate('Missing: {{invalid.path}}', state)).toBe(
        'Missing: {{invalid.path}}',
      )
    })

    it('should leave placeholder for array values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Items: {{data.items}}', state)).toBe(
        'Items: {{data.items}}',
      )
    })

    it('should leave placeholder for object values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Data: {{data.nested}}', state)).toBe(
        'Data: {{data.nested}}',
      )
    })
  })

  describe('mixed templates', () => {
    it('should handle mix of interpolatable and non-interpolatable', () => {
      const state = createTestState()
      expect(
        interpolateTemplate(
          '{{user.name}} ({{user.email}}) has {{messages.count}} messages',
          state,
        ),
      ).toBe('Alice ({{user.email}}) has 5 messages')
    })

    it('should handle template with no placeholders', () => {
      const state = createTestState()
      expect(interpolateTemplate('Hello World', state)).toBe('Hello World')
    })

    it('should handle empty template', () => {
      const state = createTestState()
      expect(interpolateTemplate('', state)).toBe('')
    })
  })

  describe('deeply nested paths', () => {
    it('should interpolate deeply nested values', () => {
      const state = createTestState()
      expect(interpolateTemplate('Nested: {{data.nested.value}}', state)).toBe(
        'Nested: 42',
      )
    })
  })

  describe('edge cases', () => {
    it('should handle multiple occurrences of same placeholder', () => {
      const state = createTestState()
      expect(interpolateTemplate('{{user.name}} is {{user.name}}', state)).toBe(
        'Alice is Alice',
      )
    })

    it('should handle adjacent placeholders', () => {
      const state = createTestState()
      expect(interpolateTemplate('{{user.name}}{{user.age}}', state)).toBe(
        'Alice30',
      )
    })

    it('should handle placeholder at start and end', () => {
      const state = createTestState()
      expect(interpolateTemplate('{{user.name}}', state)).toBe('Alice')
    })

    it('should handle whitespace around path in placeholder', () => {
      // Note: Current implementation does not trim whitespace
      const state = createTestState()
      expect(interpolateTemplate('{{ user.name }}', state)).toBe(
        '{{ user.name }}',
      )
    })

    it('should handle special characters in values', () => {
      const state = { text: 'Hello <World> & "Friends"' }
      expect(interpolateTemplate('Text: {{text}}', state)).toBe(
        'Text: Hello <World> & "Friends"',
      )
    })

    it('should handle values that look like placeholders', () => {
      const state = { value: '{{nested}}' }
      expect(interpolateTemplate('Value: {{value}}', state)).toBe(
        'Value: {{nested}}',
      )
    })
  })
})
