/**
 * ValueLogic Engine: Conditional Value Selection
 *
 * Validates that ValueLogic-based concerns:
 * - Evaluate IF/THEN/ELSE expressions (including elif chains)
 * - Evaluate MATCH expressions (multi-way switch)
 * - Return arbitrary JSON values (arrays, objects, strings, numbers)
 * - Re-evaluate when dependent state paths change
 * - Support complex BoolLogic conditions in IF clauses
 */

import { describe, expect, it } from 'vitest'

import type { StoreConfig } from '~/core/types'

import { createGenericStore } from '../../src'
import { flushEffects, mountStore } from '../utils/react'

interface ValueLogicTestState {
  user: {
    role: string
    age: number
  }
  item: {
    priority: number
  }
}

/** ValueLogic is WASM-only — no legacy mode support. */
const WASM_CONFIG: StoreConfig = { useLegacyImplementation: false }

describe('[WASM] ValueLogic Engine', () => {
  const config = WASM_CONFIG

  describe('IF/THEN/ELSE — simple condition', () => {
    it('should return THEN value when condition is true', async () => {
      // Create store with state { user: { role: 'admin', age: 30 } }
      // Register concern: options with value_logic IF IS_EQUAL role admin THEN ['create','read','update','delete'] ELSE ['read']
      // Assert _concerns['action.type']['options'] = ['create','read','update','delete']

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'admin', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              options: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: ['create', 'read', 'update', 'delete'],
                  ELSE: ['read'],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const options = storeInstance._concerns?.['user.role']?.['options']
      expect(options).toEqual(['create', 'read', 'update', 'delete'])
    })

    it('should return ELSE value when condition is false', async () => {
      // Create store with state { user: { role: 'viewer', age: 30 } }
      // Register same concern as above
      // Assert _concerns['user.role']['options'] = ['read']

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'viewer', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              options: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: ['create', 'read', 'update', 'delete'],
                  ELSE: ['read'],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const options = storeInstance._concerns?.['user.role']?.['options']
      expect(options).toEqual(['read'])
    })

    it('should re-evaluate when dependent path changes', async () => {
      // Create store with state { user: { role: 'viewer', age: 30 } }
      // Register concern, assert ELSE value
      // setValue('user.role', 'admin')
      // flushSync, assert THEN value

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { user: { role: 'viewer', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              options: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: ['create', 'read', 'update', 'delete'],
                  ELSE: ['read'],
                },
              },
            },
          },
        },
      )

      await flushEffects()
      expect(storeInstance._concerns?.['user.role']?.['options']).toEqual([
        'read',
      ])

      setValue('user.role', 'admin')
      await flushEffects()

      expect(storeInstance._concerns?.['user.role']?.['options']).toEqual([
        'create',
        'read',
        'update',
        'delete',
      ])
    })
  })

  describe('IF/THEN/ELSE — chained elif', () => {
    it('should resolve first matching branch in elif chain', async () => {
      // State: { user: { role: 'editor' } }
      // ValueLogic: IF admin THEN 'Full' ELSE { IF editor THEN 'Edit' ELSE 'Read' }
      // Assert result = 'Edit'

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'editor', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              accessLevel: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: 'Full',
                  ELSE: {
                    IF: { IS_EQUAL: ['user.role', 'editor'] },
                    THEN: 'Edit',
                    ELSE: 'Read',
                  },
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const accessLevel =
        storeInstance._concerns?.['user.role']?.['accessLevel']
      expect(accessLevel).toBe('Edit')
    })

    it('should fall through to final ELSE in elif chain', async () => {
      // State: { user: { role: 'viewer' } }
      // Same elif chain
      // Assert result = 'Read'

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'viewer', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              accessLevel: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: 'Full',
                  ELSE: {
                    IF: { IS_EQUAL: ['user.role', 'editor'] },
                    THEN: 'Edit',
                    ELSE: 'Read',
                  },
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const accessLevel =
        storeInstance._concerns?.['user.role']?.['accessLevel']
      expect(accessLevel).toBe('Read')
    })
  })

  describe('MATCH — multi-way switch', () => {
    it('should select correct CASES value for matching key', async () => {
      // State: { user: { role: 'editor' } }
      // MATCH 'user.role' CASES { admin: [...], editor: ['read','update'] } DEFAULT ['read']
      // Assert result = ['read', 'update']

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'editor', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              permissions: {
                valueLogic: {
                  MATCH: 'user.role',
                  CASES: {
                    admin: ['create', 'read', 'update', 'delete'],
                    editor: ['read', 'update'],
                    viewer: ['read'],
                  },
                  DEFAULT: [],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const permissions =
        storeInstance._concerns?.['user.role']?.['permissions']
      expect(permissions).toEqual(['read', 'update'])
    })

    it('should return DEFAULT when no case matches', async () => {
      // State: { user: { role: 'intern' } } — not in CASES
      // Assert result = DEFAULT value

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'intern', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              permissions: {
                valueLogic: {
                  MATCH: 'user.role',
                  CASES: {
                    admin: ['all'],
                    editor: ['read', 'update'],
                  },
                  DEFAULT: ['none'],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const permissions =
        storeInstance._concerns?.['user.role']?.['permissions']
      expect(permissions).toEqual(['none'])
    })

    it('should handle numeric MATCH keys', async () => {
      // State: { item: { priority: 2 } }
      // MATCH 'item.priority' CASES { '1': 'low', '2': 'medium', '3': 'high' } DEFAULT 'unknown'
      // Assert result = 'medium'

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'viewer', age: 30 }, item: { priority: 2 } },
        {
          concerns: {
            'item.priority': {
              label: {
                valueLogic: {
                  MATCH: 'item.priority',
                  CASES: { '1': 'low', '2': 'medium', '3': 'high' },
                  DEFAULT: 'unknown',
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const label = storeInstance._concerns?.['item.priority']?.['label']
      expect(label).toBe('medium')
    })

    it('should re-evaluate when MATCH path changes', async () => {
      // State: { user: { role: 'viewer' } }
      // Register MATCH concern, assert DEFAULT or viewer case
      // setValue('user.role', 'admin')
      // flushSync, assert admin case

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        { user: { role: 'viewer', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              permissions: {
                valueLogic: {
                  MATCH: 'user.role',
                  CASES: {
                    admin: ['all'],
                    editor: ['read', 'update'],
                    viewer: ['read'],
                  },
                  DEFAULT: [],
                },
              },
            },
          },
        },
      )

      await flushEffects()
      expect(storeInstance._concerns?.['user.role']?.['permissions']).toEqual([
        'read',
      ])

      setValue('user.role', 'admin')
      await flushEffects()

      expect(storeInstance._concerns?.['user.role']?.['permissions']).toEqual([
        'all',
      ])
    })
  })

  describe('complex values', () => {
    it('should handle object values in THEN/ELSE', async () => {
      // THEN: { label: 'Admin Panel', icon: 'shield' }
      // ELSE: { label: 'Dashboard', icon: 'home' }

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'admin', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              panelInfo: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: { label: 'Admin Panel', icon: 'shield' },
                  ELSE: { label: 'Dashboard', icon: 'home' },
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const panelInfo = storeInstance._concerns?.['user.role']?.['panelInfo']
      expect(panelInfo).toEqual({ label: 'Admin Panel', icon: 'shield' })
    })

    it('should handle nested array of objects', async () => {
      // THEN: [{ value: 'create', label: 'Create' }, { value: 'read', label: 'Read' }]
      // ELSE: [{ value: 'read', label: 'Read' }]

      const store = createGenericStore<ValueLogicTestState>(config)
      const { storeInstance } = mountStore(
        store,
        { user: { role: 'admin', age: 30 }, item: { priority: 1 } },
        {
          concerns: {
            'user.role': {
              menuItems: {
                valueLogic: {
                  IF: { IS_EQUAL: ['user.role', 'admin'] },
                  THEN: [
                    { value: 'create', label: 'Create' },
                    { value: 'read', label: 'Read' },
                  ],
                  ELSE: [{ value: 'read', label: 'Read' }],
                },
              },
            },
          },
        },
      )

      await flushEffects()

      const menuItems = storeInstance._concerns?.['user.role']?.['menuItems']
      expect(menuItems).toEqual([
        { value: 'create', label: 'Create' },
        { value: 'read', label: 'Read' },
      ])
    })
  })
})
