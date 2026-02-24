/**
 * Tests for normalizeChangesForGroups
 *
 * Verifies all matching modes work correctly:
 * - Exact match: change path equals registered path
 * - Parent change: change path is ancestor of registered path (extracts nested value)
 * - Child change: change path is descendant of registered path (preserves relative path)
 */

import { describe, expect, it } from 'vitest'

import { normalizeChangesForGroups } from '~/pipeline/normalize-changes'

describe('normalizeChangesForGroups', () => {
  describe('Exact path match', () => {
    it('should match when change path equals registered path', () => {
      // When a change matches the registered path exactly, it applies to all connected paths
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'newValue', {}]],
        pathGroups: [['a.b.c', 'path.synced', 'wow.path']],
      })

      expect(result).toHaveLength(1)
      expect(result[0]).toEqual({
        matchedPath: 'a.b.c',
        relativePath: null,
        value: 'newValue',
        meta: {},
        connectedPaths: ['a.b.c', 'path.synced', 'wow.path'],
      })
    })

    it('should propagate exact match to all paths in group', () => {
      // Exact match should have null relativePath so value applies unchanged to all neighbors
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'newValue', {}]],
        pathGroups: [['a.b.c', 'path.synced', 'wow.path']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: null,
          value: 'newValue',
          meta: {},
          connectedPaths: ['a.b.c', 'path.synced', 'wow.path'],
        },
      ])
    })
  })

  describe('Child path match', () => {
    it('should match when change path is descendant of registered path', () => {
      // When a change is deeper than the registered path, extract the relative path suffix
      const result = normalizeChangesForGroups({
        changes: [['a.b.c.deep.nested', 42, {}]],
        pathGroups: [['a.b.c', 'path.synced', 'wow.path']],
      })

      expect(result).toHaveLength(1)
      expect(result[0]).toEqual({
        matchedPath: 'a.b.c',
        relativePath: 'deep.nested',
        value: 42,
        meta: {},
        connectedPaths: ['a.b.c', 'path.synced', 'wow.path'],
      })
    })

    it('should append relative path to all paths in group', () => {
      // Child changes use relativePath to apply nested updates to all neighbor paths
      const result = normalizeChangesForGroups({
        changes: [['a.b.c.deep.nested', 42, {}]],
        pathGroups: [['a.b.c', 'path.synced', 'wow.path']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: 'deep.nested',
          value: 42,
          meta: {},
          connectedPaths: ['a.b.c', 'path.synced', 'wow.path'],
        },
      ])
    })
  })

  describe('Additional test cases', () => {
    it('should handle empty changes array', () => {
      const result = normalizeChangesForGroups({
        changes: [],
        pathGroups: [['a.b.c', 'path.synced']],
      })

      expect(result).toHaveLength(0)
    })

    it('should handle multiple path groups', () => {
      const result = normalizeChangesForGroups({
        changes: [
          ['a.b.c', 'value1', {}],
          ['x.y.z', 'value2', {}],
        ],
        pathGroups: [
          ['a.b.c', 'path.synced'],
          ['x.y.z', 'other.path'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: null,
          value: 'value1',
          meta: {},
          connectedPaths: ['a.b.c', 'path.synced'],
        },
        {
          matchedPath: 'x.y.z',
          relativePath: null,
          value: 'value2',
          meta: {},
          connectedPaths: ['x.y.z', 'other.path'],
        },
      ])
    })

    it('should preserve metadata from changes', () => {
      const meta = { isSyncPathChange: true, sender: 'test' }
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'value', meta]],
        pathGroups: [['a.b.c', 'path.synced']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: null,
          value: 'value',
          meta,
          connectedPaths: ['a.b.c', 'path.synced'],
        },
      ])
    })

    it('should match first path in group and stop', () => {
      // Only one match per group per change
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'value', {}]],
        pathGroups: [['a.b.c', 'a.b.c.other', 'path.synced']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: null,
          value: 'value',
          meta: {},
          connectedPaths: ['a.b.c', 'a.b.c.other', 'path.synced'],
        },
      ])
    })

    it('should handle deeply nested child changes', () => {
      const result = normalizeChangesForGroups({
        changes: [['a.b.c.level1.level2.level3', 'deep', {}]],
        pathGroups: [['a.b.c', 'path.synced']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b.c',
          relativePath: 'level1.level2.level3',
          value: 'deep',
          meta: {},
          connectedPaths: ['a.b.c', 'path.synced'],
        },
      ])
    })

    it('should not match unrelated paths', () => {
      const result = normalizeChangesForGroups({
        changes: [['x.y.z', 'value', {}]],
        pathGroups: [['a.b.c', 'path.synced']],
      })

      expect(result).toHaveLength(0)
    })
  })

  describe('Parent change scenarios', () => {
    it('should extract nested value when parent path changes', () => {
      // When a parent path changes with object value, extract nested property
      // changePath='user' (parent), registeredPath='user.profile.name' (child)
      const result = normalizeChangesForGroups({
        changes: [['user', { profile: { name: 'Alice' } }, {}]],
        pathGroups: [['user.profile.name', 'user.display.name']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'user.profile.name',
          relativePath: null,
          value: 'Alice',
          meta: {},
          connectedPaths: ['user.profile.name', 'user.display.name'],
        },
      ])
    })

    it('should not match parent change if value is not an object', () => {
      // Parent changes with primitive values cannot extract nested properties
      const result = normalizeChangesForGroups({
        changes: [['user', 'just a string', {}]],
        pathGroups: [['user.profile.name', 'user.display.name']],
      })

      expect(result).toHaveLength(0)
    })

    it('should not match parent change if nested property is missing', () => {
      // Parent change object doesn't contain the needed nested path
      const result = normalizeChangesForGroups({
        changes: [['user', { settings: { theme: 'dark' } }, {}]],
        pathGroups: [['user.profile.name', 'user.display.name']],
      })

      expect(result).toHaveLength(0)
    })

    it('should handle deeply nested extraction', () => {
      // Extract from multiple nesting levels
      const result = normalizeChangesForGroups({
        changes: [['data', { users: { admin: { name: 'Admin User' } } }, {}]],
        pathGroups: [['data.users.admin.name', 'data.users.admin.display']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'data.users.admin.name',
          relativePath: null,
          value: 'Admin User',
          meta: {},
          connectedPaths: ['data.users.admin.name', 'data.users.admin.display'],
        },
      ])
    })
  })

  describe('Parent changes on non-leaf paths', () => {
    it('should extract object value when parent has multiple properties', () => {
      // Parent change on 'user' with multiple nested objects
      // Registered paths listening on middle nodes (user.profile, user.settings)
      const result = normalizeChangesForGroups({
        changes: [
          [
            'user',
            {
              profile: { name: 'Alice', age: 30 },
              settings: { theme: 'dark', notifications: true },
            },
            {},
          ],
        ],
        pathGroups: [
          ['user.profile', 'backup.profile'],
          ['user.settings', 'config.settings'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'user.profile',
          relativePath: null,
          value: { name: 'Alice', age: 30 },
          meta: {},
          connectedPaths: ['user.profile', 'backup.profile'],
        },
        {
          matchedPath: 'user.settings',
          relativePath: null,
          value: { theme: 'dark', notifications: true },
          meta: {},
          connectedPaths: ['user.settings', 'config.settings'],
        },
      ])
    })

    it('should handle parent change affecting multiple non-leaf groups', () => {
      // Single parent change propagates to multiple different group hierarchies
      const result = normalizeChangesForGroups({
        changes: [
          [
            'data',
            {
              users: [{ name: 'John' }, { name: 'Jane' }],
              metadata: { updated: '2024-01-01', version: 2 },
            },
            {},
          ],
        ],
        pathGroups: [
          ['data.users', 'cache.users'],
          ['data.metadata', 'info.metadata'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'data.users',
          relativePath: null,
          value: [{ name: 'John' }, { name: 'Jane' }],
          meta: {},
          connectedPaths: ['data.users', 'cache.users'],
        },
        {
          matchedPath: 'data.metadata',
          relativePath: null,
          value: { updated: '2024-01-01', version: 2 },
          meta: {},
          connectedPaths: ['data.metadata', 'info.metadata'],
        },
      ])
    })

    it('should extract nested object when middle path is registered', () => {
      // Listening on middle node: form.fields is not a leaf
      // Parent change on 'form' contains form.fields object
      const result = normalizeChangesForGroups({
        changes: [
          [
            'form',
            {
              fields: {
                username: { value: 'user123', valid: true },
                email: { value: 'user@example.com', valid: false },
              },
              metadata: { submitCount: 0 },
            },
            {},
          ],
        ],
        pathGroups: [['form.fields', 'formBackup.fields']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'form.fields',
          relativePath: null,
          value: {
            username: { value: 'user123', valid: true },
            email: { value: 'user@example.com', valid: false },
          },
          meta: {},
          connectedPaths: ['form.fields', 'formBackup.fields'],
        },
      ])
    })

    it('should handle parent change with array of objects at non-leaf path', () => {
      // Listening on collection path (not individual items)
      const result = normalizeChangesForGroups({
        changes: [
          [
            'state',
            {
              items: [
                { id: 1, name: 'Item 1', tags: ['a', 'b'] },
                { id: 2, name: 'Item 2', tags: ['c'] },
              ],
              total: 2,
            },
            {},
          ],
        ],
        pathGroups: [['state.items', 'snapshot.items']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'state.items',
          relativePath: null,
          value: [
            { id: 1, name: 'Item 1', tags: ['a', 'b'] },
            { id: 2, name: 'Item 2', tags: ['c'] },
          ],
          meta: {},
          connectedPaths: ['state.items', 'snapshot.items'],
        },
      ])
    })

    it('should sync parent change across multiple non-leaf groups in same path', () => {
      // Multiple registered paths listening on different levels in same hierarchy
      const result = normalizeChangesForGroups({
        changes: [
          [
            'app',
            {
              ui: {
                theme: 'dark',
                layout: 'compact',
                sidebar: { visible: true, width: 250 },
              },
            },
            {},
          ],
        ],
        pathGroups: [
          ['app.ui', 'config.ui'],
          ['app.ui.sidebar', 'settings.sidebar'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'app.ui',
          relativePath: null,
          value: {
            theme: 'dark',
            layout: 'compact',
            sidebar: { visible: true, width: 250 },
          },
          meta: {},
          connectedPaths: ['app.ui', 'config.ui'],
        },
        {
          matchedPath: 'app.ui.sidebar',
          relativePath: null,
          value: { visible: true, width: 250 },
          meta: {},
          connectedPaths: ['app.ui.sidebar', 'settings.sidebar'],
        },
      ])
    })

    it('should handle parent change with null/undefined in nested structure', () => {
      // Parent object has some null properties but registered paths still extract
      const result = normalizeChangesForGroups({
        changes: [
          [
            'config',
            {
              database: { host: 'localhost', port: 5432 },
              cache: null,
              api: { timeout: 30000, retries: 3 },
            },
            {},
          ],
        ],
        pathGroups: [
          ['config.database', 'backup.database'],
          ['config.api', 'backup.api'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'config.database',
          relativePath: null,
          value: { host: 'localhost', port: 5432 },
          meta: {},
          connectedPaths: ['config.database', 'backup.database'],
        },
        {
          matchedPath: 'config.api',
          relativePath: null,
          value: { timeout: 30000, retries: 3 },
          meta: {},
          connectedPaths: ['config.api', 'backup.api'],
        },
      ])
    })

    it('should handle parent change where one nested path is missing', () => {
      // Parent change missing one of the expected nested paths in pathGroup
      const result = normalizeChangesForGroups({
        changes: [
          [
            'state',
            {
              visible: { menu: true, sidebar: false },
              // 'editing' is missing from the parent change
            },
            {},
          ],
        ],
        pathGroups: [
          ['state.visible', 'ui.visible'],
          ['state.editing', 'ui.editing'],
        ],
      })

      // First path matches, second path doesn't (missing in parent object)
      expect(result).toEqual([
        {
          matchedPath: 'state.visible',
          relativePath: null,
          value: { menu: true, sidebar: false },
          meta: {},
          connectedPaths: ['state.visible', 'ui.visible'],
        },
      ])
    })

    it('should preserve metadata with parent changes on non-leaf paths', () => {
      // Metadata should be passed through even for extracted object values
      const meta = {
        sender: 'api',
        isSyncPathChange: true,
      }
      const result = normalizeChangesForGroups({
        changes: [
          [
            'payload',
            {
              user: { id: 1, name: 'Alice' },
              timestamp: '2024-01-01T00:00:00Z',
            },
            meta,
          ],
        ],
        pathGroups: [['payload.user', 'local.user']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'payload.user',
          relativePath: null,
          value: { id: 1, name: 'Alice' },
          meta,
          connectedPaths: ['payload.user', 'local.user'],
        },
      ])
    })

    it('should handle complex real-world form state parent change', () => {
      // Realistic scenario: form state update with multiple non-leaf listeners
      const result = normalizeChangesForGroups({
        changes: [
          [
            'formState',
            {
              values: {
                username: 'john_doe',
                email: 'john@example.com',
                password: '***',
              },
              errors: {
                username: null,
                email: 'Invalid email',
              },
              touched: {
                username: true,
                email: true,
              },
              isSubmitting: false,
            },
            { sender: 'form-update' },
          ],
        ],
        pathGroups: [
          ['formState.values', 'formData.values'],
          ['formState.errors', 'formData.errors'],
          ['formState.touched', 'formData.touched'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'formState.values',
          relativePath: null,
          value: {
            username: 'john_doe',
            email: 'john@example.com',
            password: '***',
          },
          meta: { sender: 'form-update' },
          connectedPaths: ['formState.values', 'formData.values'],
        },
        {
          matchedPath: 'formState.errors',
          relativePath: null,
          value: {
            username: null,
            email: 'Invalid email',
          },
          meta: { sender: 'form-update' },
          connectedPaths: ['formState.errors', 'formData.errors'],
        },
        {
          matchedPath: 'formState.touched',
          relativePath: null,
          value: {
            username: true,
            email: true,
          },
          meta: { sender: 'form-update' },
          connectedPaths: ['formState.touched', 'formData.touched'],
        },
      ])
    })
  })

  describe('Multiple changes and groups', () => {
    it('should process multiple changes across multiple groups correctly', () => {
      const result = normalizeChangesForGroups({
        changes: [
          ['settings.theme', 'dark', { sender: 'ui' }],
          ['settings.language', 'en', { sender: 'ui' }],
          ['user.name', 'Bob', { sender: 'form' }],
        ],
        pathGroups: [
          ['settings.theme', 'config.theme'],
          ['settings.language', 'config.language'],
          ['user.name', 'profile.name'],
        ],
      })

      expect(result).toHaveLength(3)

      // Verify first change
      expect(result[0]!).toEqual({
        matchedPath: 'settings.theme',
        relativePath: null,
        value: 'dark',
        meta: { sender: 'ui' },
        connectedPaths: ['settings.theme', 'config.theme'],
      })

      // Verify second change
      expect(result[1]!).toEqual({
        matchedPath: 'settings.language',
        relativePath: null,
        value: 'en',
        meta: { sender: 'ui' },
        connectedPaths: ['settings.language', 'config.language'],
      })

      // Verify third change
      expect(result[2]!).toEqual({
        matchedPath: 'user.name',
        relativePath: null,
        value: 'Bob',
        meta: { sender: 'form' },
        connectedPaths: ['user.name', 'profile.name'],
      })
    })

    it('should handle same change matching multiple groups', () => {
      // A single change can match multiple separate groups
      const result = normalizeChangesForGroups({
        changes: [['items.0.name', 'Item 1', {}]],
        pathGroups: [
          ['items.0', 'list.first'],
          ['items', 'allItems'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'items.0',
          relativePath: 'name',
          value: 'Item 1',
          meta: {},
          connectedPaths: ['items.0', 'list.first'],
        },
        {
          matchedPath: 'items',
          relativePath: '0.name',
          value: 'Item 1',
          meta: {},
          connectedPaths: ['items', 'allItems'],
        },
      ])
    })

    it('should stop at first match within same group', () => {
      // If multiple paths in same group match, only first is recorded
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'value', {}]],
        pathGroups: [['a.b', 'a.b.c', 'a.b.c.d']],
      })

      // Matches 'a.b' first (child match with relativePath='c'), stops there
      expect(result).toEqual([
        {
          matchedPath: 'a.b',
          relativePath: 'c',
          value: 'value',
          meta: {},
          connectedPaths: ['a.b', 'a.b.c', 'a.b.c.d'],
        },
      ])
    })
  })

  describe('Mixed scenarios with various change types', () => {
    it('should handle mix of exact, parent, and child changes', () => {
      const result = normalizeChangesForGroups({
        changes: [
          // Exact match
          ['form.name', 'John', { sender: 'exact' }],
          // Parent change (form object contains form.age.value)
          ['form', { age: { value: 30 } }, { sender: 'parent' }],
          // Child change (form.address.street extends form.address)
          ['form.address.street', '123 Main St', { sender: 'child' }],
        ],
        pathGroups: [
          ['form.name', 'input.name'],
          ['form.age.value', 'input.age'],
          ['form.address', 'address.data'],
        ],
      })

      expect(result).toHaveLength(3)

      // Exact match on form.name
      expect(result[0]!).toEqual({
        matchedPath: 'form.name',
        relativePath: null,
        value: 'John',
        meta: { sender: 'exact' },
        connectedPaths: ['form.name', 'input.name'],
      })

      // Parent match on form (extracts form.age.value)
      expect(result[1]!).toEqual({
        matchedPath: 'form.age.value',
        relativePath: null,
        value: 30,
        meta: { sender: 'parent' },
        connectedPaths: ['form.age.value', 'input.age'],
      })

      // Child match on form.address
      expect(result[2]!).toEqual({
        matchedPath: 'form.address',
        relativePath: 'street',
        value: '123 Main St',
        meta: { sender: 'child' },
        connectedPaths: ['form.address', 'address.data'],
      })
    })

    it('should handle overlapping paths correctly', () => {
      // Paths that are prefixes of each other
      const result = normalizeChangesForGroups({
        changes: [['a', { b: { c: { d: 'deep' } } }, {}]],
        pathGroups: [
          ['a.b', 'sync.b'],
          ['a.b.c', 'sync.c'],
          ['a.b.c.d', 'sync.d'],
        ],
      })

      expect(result).toEqual([
        {
          matchedPath: 'a.b',
          relativePath: null,
          value: { c: { d: 'deep' } },
          meta: {},
          connectedPaths: ['a.b', 'sync.b'],
        },
        {
          matchedPath: 'a.b.c',
          relativePath: null,
          value: { d: 'deep' },
          meta: {},
          connectedPaths: ['a.b.c', 'sync.c'],
        },
        {
          matchedPath: 'a.b.c.d',
          relativePath: null,
          value: 'deep',
          meta: {},
          connectedPaths: ['a.b.c.d', 'sync.d'],
        },
      ])
    })

    it('should correctly handle complex nested structure changes', () => {
      // Complex real-world scenario: form with validation
      const result = normalizeChangesForGroups({
        changes: [
          // Change to nested property
          ['form.fields.username.value', 'jane_doe', {}],
          // Change to parent object
          ['form.fields.email', { value: 'jane@example.com', valid: true }, {}],
        ],
        pathGroups: [
          ['form.fields.username', 'backup.username'],
          ['form.fields.email.value', 'validation.email'],
        ],
      })

      expect(result).toHaveLength(2)

      // Child match: username.value deeper than username
      expect(result[0]!).toEqual({
        matchedPath: 'form.fields.username',
        relativePath: 'value',
        value: 'jane_doe',
        meta: {},
        connectedPaths: ['form.fields.username', 'backup.username'],
      })

      // Parent match: email object contains email.value
      expect(result[1]!).toEqual({
        matchedPath: 'form.fields.email.value',
        relativePath: null,
        value: 'jane@example.com',
        meta: {},
        connectedPaths: ['form.fields.email.value', 'validation.email'],
      })
    })

    it('should handle null values in changes', () => {
      const result = normalizeChangesForGroups({
        changes: [
          ['user.profile', null, {}],
          ['user.settings.theme', undefined, {}],
        ],
        pathGroups: [
          ['user.profile.name', 'display.name'],
          ['user.settings.theme', 'config.theme'],
        ],
      })

      // First change: null is not an object, cannot extract
      // Second change: exact match even with undefined
      expect(result).toEqual([
        {
          matchedPath: 'user.settings.theme',
          relativePath: null,
          value: undefined,
          meta: {},
          connectedPaths: ['user.settings.theme', 'config.theme'],
        },
      ])
    })

    it('should process empty path groups', () => {
      const result = normalizeChangesForGroups({
        changes: [['a.b.c', 'value', {}]],
        pathGroups: [[]],
      })

      expect(result).toHaveLength(0)
    })

    it('should handle large number of groups and changes', () => {
      const groups = Array.from({ length: 50 }, (_, i) => [
        `path${i}.a`,
        `path${i}.b`,
        `path${i}.c`,
      ])

      const changes: [string, string, Record<string, unknown>][] = Array.from(
        { length: 50 },
        (_, i) => [`path${i}.a`, `value${i}`, {}],
      )

      const result = normalizeChangesForGroups({
        changes,
        pathGroups: groups,
      })

      expect(result).toHaveLength(50)

      // Verify each result is correct
      result.forEach((match, i) => {
        expect(match.matchedPath).toBe(`path${i}.a`)
        expect(match.value).toBe(`value${i}`)
        expect(match.relativePath).toBeNull()
        expect(match.connectedPaths).toHaveLength(3)
      })
    })
  })

  describe('Edge cases with special characters and long paths', () => {
    it('should handle numeric path segments', () => {
      const result = normalizeChangesForGroups({
        changes: [['items.0.name', 'First', {}]],
        pathGroups: [['items.0', 'list.0']],
      })

      expect(result).toEqual([
        {
          matchedPath: 'items.0',
          relativePath: 'name',
          value: 'First',
          meta: {},
          connectedPaths: ['items.0', 'list.0'],
        },
      ])
    })

    it('should handle very long nested paths', () => {
      const longPath = 'a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z'
      const childPath = longPath + '.deep.value'

      const result = normalizeChangesForGroups({
        changes: [[childPath, 'found', {}]],
        pathGroups: [[longPath, 'alias.path']],
      })

      expect(result).toEqual([
        {
          matchedPath: longPath,
          relativePath: 'deep.value',
          value: 'found',
          meta: {},
          connectedPaths: [longPath, 'alias.path'],
        },
      ])
    })
  })
})
