/**
 * Tests for shadow state operations
 *
 * Tests all shadow state tree operations:
 * - Path parsing (parsePath, joinPath, isValidPath)
 * - Tree traversal (getNode, getNodeBySegments, traversePath)
 * - Tree queries (hasPath, getValue, collectPaths, getChildren, getParent)
 * - Node info (countDescendants, getDepth, isLeaf, isRoot)
 * - Update operations (updateNode, setValue, replaceSubtree)
 * - Affected path calculation (getAncestorPaths, getDescendantPaths, calculateAffectedPaths)
 * - Path relationships (isAncestorOf, isDescendantOf)
 */

import { describe, expect, it } from 'vitest'

import {
  calculateAffectedPaths,
  collectPaths,
  countDescendants,
  getAncestorPaths,
  getChildren,
  getDepth,
  getDescendantPaths,
  getNode,
  getNodeBySegments,
  getParent,
  getValue,
  hasPath,
  isAncestorOf,
  isDescendantOf,
  isLeaf,
  isRoot,
  mergeAffectedPaths,
  replaceSubtree,
  setValue,
  traversePath,
  updateNode,
} from '../../src/shadow-state/operations'
import {
  clearCache,
  isValidPath,
  joinPath,
  parsePath,
} from '../../src/shadow-state/pathParser'
import { createShadowTree } from '../../src/shadow-state/tree'
import type { ShadowNode } from '../../src/shadow-state/types'

interface TestState {
  user: {
    name: string
    age: number
    profile: {
      bio: string
      tags: string[]
    }
  }
  todos: {
    title: string
    done: boolean
  }[]
  settings: {
    theme: string
    notifications: {
      email: boolean
      push: boolean
    }
  }
  empty: Record<string, never>
}

const createTestState = (): TestState => ({
  user: {
    name: 'Alice',
    age: 30,
    profile: {
      bio: 'Developer',
      tags: ['typescript', 'react'],
    },
  },
  todos: [
    { title: 'Buy milk', done: false },
    { title: 'Walk dog', done: true },
  ],
  settings: {
    theme: 'dark',
    notifications: {
      email: true,
      push: false,
    },
  },
  empty: {},
})

describe('pathParser', () => {
  describe('parsePath', () => {
    it('should parse dot notation', () => {
      expect(parsePath('user.name')).toEqual(['user', 'name'])
      expect(parsePath('user.profile.bio')).toEqual(['user', 'profile', 'bio'])
    })

    it('should parse bracket notation', () => {
      expect(parsePath('todos[0]')).toEqual(['todos', '0'])
      expect(parsePath('todos[0].title')).toEqual(['todos', '0', 'title'])
    })

    it('should parse mixed notation', () => {
      expect(parsePath('user.profile.tags[0]')).toEqual([
        'user',
        'profile',
        'tags',
        '0',
      ])
      expect(parsePath('todos[0].title')).toEqual(['todos', '0', 'title'])
    })

    it('should handle empty path', () => {
      expect(parsePath('')).toEqual([])
    })

    it('should handle consecutive brackets', () => {
      expect(parsePath('matrix[0][1]')).toEqual(['matrix', '0', '1'])
    })

    it('should cache parsed paths', () => {
      clearCache()
      const path = 'user.profile.name'
      const result1 = parsePath(path)
      const result2 = parsePath(path)
      // Same array reference means it was cached
      expect(result1).toBe(result2)
    })
  })

  describe('joinPath', () => {
    it('should join segments with dots', () => {
      expect(joinPath(['user', 'name'])).toBe('user.name')
      expect(joinPath(['user', 'profile', 'bio'])).toBe('user.profile.bio')
    })

    it('should use bracket notation for numeric segments', () => {
      expect(joinPath(['todos', '0'])).toBe('todos[0]')
      expect(joinPath(['todos', '0', 'title'])).toBe('todos[0].title')
    })

    it('should handle empty array', () => {
      expect(joinPath([])).toBe('')
    })

    it('should handle single segment', () => {
      expect(joinPath(['user'])).toBe('user')
    })
  })

  describe('isValidPath', () => {
    it('should accept valid paths', () => {
      expect(isValidPath('user.name')).toBe(true)
      expect(isValidPath('todos[0].title')).toBe(true)
      expect(isValidPath('')).toBe(true)
    })

    it('should reject consecutive dots', () => {
      expect(isValidPath('user..name')).toBe(false)
    })

    it('should reject unbalanced brackets', () => {
      expect(isValidPath('todos[0')).toBe(false)
      expect(isValidPath('todos]0[')).toBe(false)
    })
  })

  describe('clearCache', () => {
    it('should clear the path cache', () => {
      const path = 'user.name'
      const result1 = parsePath(path)
      clearCache()
      const result2 = parsePath(path)
      // Different array references after cache clear
      expect(result1).not.toBe(result2)
      expect(result1).toEqual(result2)
    })
  })
})

describe('tree traversal', () => {
  describe('getNode', () => {
    it('should get node by path string', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const userNode = getNode(tree, 'user')
      expect(userNode?.value).toEqual(state.user)
      expect(userNode?.path).toEqual(['user'])
    })

    it('should get deeply nested node', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const bioNode = getNode(tree, 'user.profile.bio')
      expect(bioNode?.value).toBe('Developer')
      expect(bioNode?.path).toEqual(['user', 'profile', 'bio'])
    })

    it('should get array element node', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const todoNode = getNode(tree, 'todos[0]')
      expect(todoNode?.value).toEqual({ title: 'Buy milk', done: false })
      expect(todoNode?.path).toEqual(['todos', '0'])
    })

    it('should get array element property', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const titleNode = getNode(tree, 'todos[0].title')
      expect(titleNode?.value).toBe('Buy milk')
      expect(titleNode?.path).toEqual(['todos', '0', 'title'])
    })

    it('should return undefined for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getNode(tree, 'nonexistent')).toBeUndefined()
      expect(getNode(tree, 'user.nonexistent')).toBeUndefined()
    })

    it('should get root node with empty path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const rootNode = getNode(tree, '')
      expect(rootNode).toBe(tree.root)
    })
  })

  describe('getNodeBySegments', () => {
    it('should get node by segments array', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const node = getNodeBySegments(tree, ['user', 'name'])
      expect(node?.value).toBe('Alice')
    })

    it('should get root with empty segments', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const node = getNodeBySegments(tree, [])
      expect(node).toBe(tree.root)
    })

    it('should handle numeric segments for arrays', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const node = getNodeBySegments(tree, ['todos', '0', 'title'])
      expect(node?.value).toBe('Buy milk')
    })

    it('should return undefined for invalid path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getNodeBySegments(tree, ['invalid'])).toBeUndefined()
    })
  })

  describe('traversePath', () => {
    it('should visit all nodes in depth-first order', () => {
      const tree = createShadowTree({ a: { b: 1, c: 2 } })
      const visited: string[] = []

      traversePath(tree.root, (node) => {
        visited.push(node.path.join('.'))
      })

      expect(visited).toContain('')
      expect(visited).toContain('a')
      expect(visited).toContain('a.b')
      expect(visited).toContain('a.c')
    })

    it('should respect maxDepth option', () => {
      const tree = createShadowTree({ a: { b: { c: { d: 1 } } } })
      const visited: string[] = []

      traversePath(
        tree.root,
        (node) => {
          visited.push(node.path.join('.'))
        },
        { maxDepth: 2 },
      )

      expect(visited).toContain('')
      expect(visited).toContain('a')
      expect(visited).toContain('a.b')
      expect(visited).not.toContain('a.b.c')
    })

    it('should detect circular references', () => {
      const obj: Record<string, unknown> = { value: 1 }
      obj['self'] = obj // Circular reference
      const tree = createShadowTree(obj)

      const visited: string[] = []
      traversePath(tree.root, (node) => {
        visited.push(node.path.join('.'))
      })

      // Should not hang or stack overflow
      expect(visited.length).toBeGreaterThan(0)
      expect(visited.length).toBeLessThan(100) // Reasonable upper bound
    })

    it('should return count of visited nodes', () => {
      const tree = createShadowTree({ a: 1, b: { c: 2 } })
      const count = traversePath(tree.root, () => {
        // No-op
      })

      expect(count).toBe(4) // root + a + b + b.c
    })

    it('should pass depth to callback', () => {
      const tree = createShadowTree({ a: { b: { c: 1 } } })
      const depths: number[] = []

      traversePath(tree.root, (_node, depth) => {
        depths.push(depth)
      })

      expect(depths).toContain(0) // root
      expect(depths).toContain(1) // a
      expect(depths).toContain(2) // a.b
      expect(depths).toContain(3) // a.b.c
    })
  })
})

describe('tree queries', () => {
  describe('hasPath', () => {
    it('should return true for existing paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(hasPath(tree, 'user')).toBe(true)
      expect(hasPath(tree, 'user.name')).toBe(true)
      expect(hasPath(tree, 'todos[0].title')).toBe(true)
    })

    it('should return false for non-existent paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(hasPath(tree, 'nonexistent')).toBe(false)
      expect(hasPath(tree, 'user.nonexistent')).toBe(false)
    })

    it('should return true for root path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(hasPath(tree, '')).toBe(true)
    })
  })

  describe('getValue', () => {
    it('should get value at path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getValue(tree, 'user.name')).toBe('Alice')
      expect(getValue(tree, 'user.age')).toBe(30)
      expect(getValue(tree, 'settings.theme')).toBe('dark')
    })

    it('should get nested object value', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const profile = getValue(tree, 'user.profile')
      expect(profile).toEqual({
        bio: 'Developer',
        tags: ['typescript', 'react'],
      })
    })

    it('should get array element value', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getValue(tree, 'todos[0].title')).toBe('Buy milk')
      expect(getValue(tree, 'todos[1].done')).toBe(true)
    })

    it('should return undefined for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getValue(tree, 'nonexistent')).toBeUndefined()
    })

    it('should get boolean values', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getValue(tree, 'settings.notifications.email')).toBe(true)
      expect(getValue(tree, 'settings.notifications.push')).toBe(false)
    })
  })

  describe('collectPaths', () => {
    it('should collect all paths in tree', () => {
      const tree = createShadowTree({ a: 1, b: { c: 2 } })
      const paths = collectPaths(tree)

      expect(paths).toContain('')
      expect(paths).toContain('a')
      expect(paths).toContain('b')
      expect(paths).toContain('b.c')
    })

    it('should respect maxDepth option', () => {
      const tree = createShadowTree({ a: { b: { c: 1 } } })
      const paths = collectPaths(tree, { maxDepth: 1 })

      expect(paths).toContain('')
      expect(paths).toContain('a')
      expect(paths).not.toContain('a.b')
    })

    it('should collect array paths', () => {
      const tree = createShadowTree({ items: ['a', 'b'] })
      const paths = collectPaths(tree)

      expect(paths).toContain('items')
      expect(paths).toContain('items.0')
      expect(paths).toContain('items.1')
    })
  })

  describe('getChildren', () => {
    it('should get all child nodes', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const children = getChildren(tree, 'user')
      expect(children.length).toBe(3) // name, age, profile
    })

    it('should return empty array for leaf nodes', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const children = getChildren(tree, 'user.name')
      expect(children).toEqual([])
    })

    it('should return empty array for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const children = getChildren(tree, 'nonexistent')
      expect(children).toEqual([])
    })

    it('should get children of array node', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const children = getChildren(tree, 'todos')
      expect(children.length).toBe(2)
    })
  })

  describe('getParent', () => {
    it('should get parent node', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const parent = getParent(tree, 'user.name')
      expect(parent?.path).toEqual(['user'])
    })

    it('should return undefined for root', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const parent = getParent(tree, '')
      expect(parent).toBeUndefined()
    })

    it('should return root as parent of top-level nodes', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const parent = getParent(tree, 'user')
      expect(parent).toBe(tree.root)
    })

    it('should return undefined for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const parent = getParent(tree, 'nonexistent')
      expect(parent).toBeUndefined()
    })
  })
})

describe('node info', () => {
  describe('countDescendants', () => {
    it('should count node and all descendants', () => {
      const tree = createShadowTree({ a: { b: 1, c: 2 } })
      const node = getNode(tree, 'a')!

      const count = countDescendants(node)
      expect(count).toBe(3) // a + b + c
    })

    it('should count leaf node as 1', () => {
      const tree = createShadowTree({ a: 1 })
      const node = getNode(tree, 'a')!

      const count = countDescendants(node)
      expect(count).toBe(1)
    })

    it('should respect maxDepth option', () => {
      const tree = createShadowTree({ a: { b: { c: 1 } } })
      const node = getNode(tree, 'a')!

      const count = countDescendants(node, { maxDepth: 1 })
      expect(count).toBe(2) // a + b (c is beyond maxDepth)
    })
  })

  describe('getDepth', () => {
    it('should return path length as depth', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(getDepth(tree.root)).toBe(0)
      expect(getDepth(getNode(tree, 'user')!)).toBe(1)
      expect(getDepth(getNode(tree, 'user.name')!)).toBe(2)
      expect(getDepth(getNode(tree, 'user.profile.bio')!)).toBe(3)
    })
  })

  describe('isLeaf', () => {
    it('should return true for nodes without children', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(isLeaf(getNode(tree, 'user.name')!)).toBe(true)
      expect(isLeaf(getNode(tree, 'user.age')!)).toBe(true)
    })

    it('should return false for nodes with children', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(isLeaf(getNode(tree, 'user')!)).toBe(false)
      expect(isLeaf(getNode(tree, 'user.profile')!)).toBe(false)
    })

    it('should return true for empty object', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(isLeaf(getNode(tree, 'empty')!)).toBe(true)
    })
  })

  describe('isRoot', () => {
    it('should return true for root node', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(isRoot(tree.root)).toBe(true)
    })

    it('should return false for non-root nodes', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(isRoot(getNode(tree, 'user')!)).toBe(false)
      expect(isRoot(getNode(tree, 'user.name')!)).toBe(false)
    })
  })
})

describe('update operations', () => {
  describe('updateNode', () => {
    it('should update primitive value', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const result = updateNode(tree, 'user.name', 'Bob')

      expect(getValue(tree, 'user.name')).toBe('Bob')
      expect(result.affectedPaths).toContain('user.name')
      expect(result.affectedPaths).toContain('user')
      expect(result.affectedPaths).toContain('')
    })

    it('should update object value and rebuild children', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const newProfile = { bio: 'Designer', tags: ['figma'] }
      const result = updateNode(tree, 'user.profile', newProfile)

      expect(getValue(tree, 'user.profile')).toEqual(newProfile)
      expect(getValue(tree, 'user.profile.bio')).toBe('Designer')
      expect(result.affectedPaths).toContain('user.profile')
      expect(result.affectedPaths).toContain('user.profile.bio')
      expect(result.affectedPaths).toContain('user.profile.tags')
    })

    it('should update array value', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const newTodos = [{ title: 'New task', done: false }]
      const result = updateNode(tree, 'todos', newTodos)

      expect(getValue(tree, 'todos')).toEqual(newTodos)
      expect(getValue(tree, 'todos[0].title')).toBe('New task')
      expect(result.affectedPaths).toContain('todos')
    })

    it('should throw error for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(() => {
        updateNode(tree, 'nonexistent', 'value')
      }).toThrow('Cannot update non-existent path: nonexistent')
    })

    it('should update tree metadata', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const initialCount = tree.nodeCount
      updateNode(tree, 'user.name', 'Bob')

      // Count should not change for primitive update
      expect(tree.nodeCount).toBe(initialCount)
    })

    it('should include ancestor paths in affected paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const result = updateNode(tree, 'user.profile.bio', 'Engineer')

      expect(result.affectedPaths).toContain('')
      expect(result.affectedPaths).toContain('user')
      expect(result.affectedPaths).toContain('user.profile')
      expect(result.affectedPaths).toContain('user.profile.bio')
    })
  })

  describe('setValue', () => {
    it('should set value at path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      setValue(tree, 'user.name', 'Charlie')
      expect(getValue(tree, 'user.name')).toBe('Charlie')
    })

    it('should throw error for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(() => {
        setValue(tree, 'nonexistent', 'value')
      }).toThrow()
    })
  })

  describe('replaceSubtree', () => {
    it('should replace entire subtree', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const newUser = {
        name: 'Diana',
        age: 25,
        profile: { bio: 'Manager', tags: [] },
      }
      const result = replaceSubtree(tree, 'user', newUser)

      expect(getValue(tree, 'user')).toEqual(newUser)
      expect(getValue(tree, 'user.name')).toBe('Diana')
      expect(result.affectedPaths).toContain('user')
      expect(result.affectedPaths).toContain('user.name')
    })

    it('should handle adding new properties', () => {
      const tree = createShadowTree({ user: { name: 'Alice' } })

      const newUser = { name: 'Bob', email: 'bob@example.com' }
      const result = replaceSubtree(tree, 'user', newUser)

      expect(getValue(tree, 'user.name')).toBe('Bob')
      expect(getValue(tree, 'user.email')).toBe('bob@example.com')
      expect(result.affectedPaths).toContain('user.email')
    })

    it('should handle removing properties', () => {
      const tree = createShadowTree({
        user: { name: 'Alice', email: 'alice@example.com' },
      })

      const newUser = { name: 'Bob' }
      const result = replaceSubtree(tree, 'user', newUser)

      expect(getValue(tree, 'user.name')).toBe('Bob')
      expect(getValue(tree, 'user.email')).toBeUndefined()
      expect(result.affectedPaths).toContain('user.email')
    })

    it('should throw error for non-existent path', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      expect(() => {
        replaceSubtree(tree, 'nonexistent', {})
      }).toThrow('Cannot replace subtree at non-existent path: nonexistent')
    })

    it('should update parent reference', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      replaceSubtree(tree, 'user', {
        name: 'New',
        age: 20,
        profile: { bio: '', tags: [] },
      })

      const nameNode = getNode(tree, 'user.name')
      expect(nameNode?.parent?.path).toEqual(['user'])
    })

    it('should include both old and new paths in affected paths', () => {
      const tree = createShadowTree({ obj: { a: 1, b: 2 } })

      const result = replaceSubtree(tree, 'obj', { b: 3, c: 4 })

      expect(result.affectedPaths).toContain('obj.a') // old path
      expect(result.affectedPaths).toContain('obj.b') // common path
      expect(result.affectedPaths).toContain('obj.c') // new path
    })
  })
})

describe('affected path calculation', () => {
  describe('getAncestorPaths', () => {
    it('should return all ancestor paths', () => {
      const ancestors = getAncestorPaths('user.profile.bio')

      expect(ancestors).toContain('')
      expect(ancestors).toContain('user')
      expect(ancestors).toContain('user.profile')
      expect(ancestors).not.toContain('user.profile.bio')
    })

    it('should handle root path', () => {
      const ancestors = getAncestorPaths('')

      expect(ancestors).toEqual([''])
    })

    it('should handle top-level path', () => {
      const ancestors = getAncestorPaths('user')

      expect(ancestors).toEqual([''])
    })

    it('should accept segments array', () => {
      const ancestors = getAncestorPaths(['user', 'profile', 'bio'])

      expect(ancestors).toContain('')
      expect(ancestors).toContain('user')
      expect(ancestors).toContain('user.profile')
    })
  })

  describe('getDescendantPaths', () => {
    it('should collect all descendant paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)
      const node = getNode(tree, 'user.profile')!

      const descendants = getDescendantPaths(node)

      expect(descendants).toContain('user.profile')
      expect(descendants).toContain('user.profile.bio')
      expect(descendants).toContain('user.profile.tags')
    })

    it('should include node itself', () => {
      const tree = createShadowTree({ a: 1 })
      const node = getNode(tree, 'a')!

      const descendants = getDescendantPaths(node)

      expect(descendants).toContain('a')
    })

    it('should respect maxDepth option', () => {
      const state = createTestState()
      const tree = createShadowTree(state)
      const node = getNode(tree, 'user')!

      const descendants = getDescendantPaths(node, { maxDepth: 1 })

      expect(descendants).toContain('user')
      expect(descendants).toContain('user.name')
      expect(descendants).not.toContain('user.profile.bio')
    })
  })

  describe('calculateAffectedPaths', () => {
    it('should include ancestors, node, and descendants', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const affected = calculateAffectedPaths(tree, 'user.profile')

      expect(affected).toContain('') // ancestor
      expect(affected).toContain('user') // ancestor
      expect(affected).toContain('user.profile') // self
      expect(affected).toContain('user.profile.bio') // descendant
      expect(affected).toContain('user.profile.tags') // descendant
    })

    it('should exclude descendants when option is false', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const affected = calculateAffectedPaths(tree, 'user.profile', {
        includeDescendants: false,
      })

      expect(affected).toContain('')
      expect(affected).toContain('user')
      expect(affected).toContain('user.profile')
      expect(affected).not.toContain('user.profile.bio')
    })

    it('should deduplicate paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const affected = calculateAffectedPaths(tree, 'user')

      const uniquePaths = new Set(affected)
      expect(uniquePaths.size).toBe(affected.length)
    })

    it('should sort paths', () => {
      const state = createTestState()
      const tree = createShadowTree(state)

      const affected = calculateAffectedPaths(tree, 'user.profile')

      expect(affected).toEqual([...affected].sort())
    })
  })

  describe('mergeAffectedPaths', () => {
    it('should merge multiple path arrays', () => {
      const paths1 = ['user', 'user.name']
      const paths2 = ['user', 'user.age']

      const merged = mergeAffectedPaths(paths1, paths2)

      expect(merged).toContain('user')
      expect(merged).toContain('user.name')
      expect(merged).toContain('user.age')
    })

    it('should deduplicate paths', () => {
      const paths1 = ['user', 'user.name']
      const paths2 = ['user', 'user.name']

      const merged = mergeAffectedPaths(paths1, paths2)

      expect(merged.filter((p) => p === 'user').length).toBe(1)
    })

    it('should sort merged paths', () => {
      const paths1 = ['z', 'a']
      const paths2 = ['m', 'b']

      const merged = mergeAffectedPaths(paths1, paths2)

      expect(merged).toEqual(['a', 'b', 'm', 'z'])
    })

    it('should handle empty arrays', () => {
      const merged = mergeAffectedPaths([], [], [])

      expect(merged).toEqual([])
    })
  })

  describe('isAncestorOf', () => {
    it('should return true for ancestor paths', () => {
      expect(isAncestorOf('user', 'user.name')).toBe(true)
      expect(isAncestorOf('user', 'user.profile.bio')).toBe(true)
    })

    it('should return false for non-ancestor paths', () => {
      expect(isAncestorOf('user.name', 'user.age')).toBe(false)
      expect(isAncestorOf('user.profile', 'user.name')).toBe(false)
    })

    it('should return false for same path', () => {
      expect(isAncestorOf('user', 'user')).toBe(false)
    })

    it('should handle root as ancestor', () => {
      expect(isAncestorOf('', 'user')).toBe(true)
      expect(isAncestorOf('', 'user.name')).toBe(true)
    })

    it('should return false for root as descendant', () => {
      expect(isAncestorOf('user', '')).toBe(false)
    })

    it('should not match partial path components', () => {
      expect(isAncestorOf('use', 'user')).toBe(false)
      expect(isAncestorOf('user', 'username')).toBe(false)
    })
  })

  describe('isDescendantOf', () => {
    it('should return true for descendant paths', () => {
      expect(isDescendantOf('user.name', 'user')).toBe(true)
      expect(isDescendantOf('user.profile.bio', 'user')).toBe(true)
    })

    it('should return false for non-descendant paths', () => {
      expect(isDescendantOf('user.age', 'user.name')).toBe(false)
      expect(isDescendantOf('user.name', 'user.profile')).toBe(false)
    })

    it('should return false for same path', () => {
      expect(isDescendantOf('user', 'user')).toBe(false)
    })

    it('should handle root as ancestor', () => {
      expect(isDescendantOf('user', '')).toBe(true)
      expect(isDescendantOf('user.name', '')).toBe(true)
    })
  })
})

describe('edge cases', () => {
  describe('circular references', () => {
    it('should handle circular object references', () => {
      const obj: Record<string, unknown> = { value: 1 }
      obj['self'] = obj

      const tree = createShadowTree(obj)
      const paths = collectPaths(tree)

      expect(paths.length).toBeGreaterThan(0)
      expect(paths.length).toBeLessThan(100)
    })

    it('should handle circular references in traversal', () => {
      const obj: Record<string, unknown> = { a: 1 }
      obj['self'] = obj

      const tree = createShadowTree(obj)
      const visited: ShadowNode[] = []

      traversePath(tree.root, (node) => {
        visited.push(node)
      })

      expect(visited.length).toBeGreaterThan(0)
      expect(visited.length).toBeLessThan(100)
    })
  })

  describe('deep nesting', () => {
    it('should handle deeply nested structures', () => {
      const deep = { a: { b: { c: { d: { e: { f: 1 } } } } } }
      const tree = createShadowTree(deep)

      expect(getValue(tree, 'a.b.c.d.e.f')).toBe(1)
    })

    it('should respect maxDepth in tree creation', () => {
      const deep = { a: { b: { c: { d: 1 } } } }
      const tree = createShadowTree(deep, { maxDepth: 2 })

      expect(hasPath(tree, 'a')).toBe(true)
      expect(hasPath(tree, 'a.b')).toBe(true)
      expect(hasPath(tree, 'a.b.c')).toBe(false)
    })
  })

  describe('null and undefined values', () => {
    it('should handle null values', () => {
      const tree = createShadowTree({ value: null })

      expect(getValue(tree, 'value')).toBeNull()
      expect(hasPath(tree, 'value')).toBe(true)
    })

    it('should handle undefined values', () => {
      const tree = createShadowTree({ value: undefined })

      expect(getValue(tree, 'value')).toBeUndefined()
      expect(hasPath(tree, 'value')).toBe(true)
    })

    it('should handle objects with null properties', () => {
      const tree = createShadowTree({ obj: { a: null, b: 1 } })

      expect(getValue(tree, 'obj.a')).toBeNull()
      expect(getValue(tree, 'obj.b')).toBe(1)
    })
  })

  describe('empty objects and arrays', () => {
    it('should handle empty objects', () => {
      const tree = createShadowTree({ empty: {} })

      expect(getValue(tree, 'empty')).toEqual({})
      expect(isLeaf(getNode(tree, 'empty')!)).toBe(true)
    })

    it('should handle empty arrays', () => {
      const tree = createShadowTree({ empty: [] })

      expect(getValue(tree, 'empty')).toEqual([])
      expect(isLeaf(getNode(tree, 'empty')!)).toBe(true)
    })
  })

  describe('special characters in paths', () => {
    it('should handle numeric string keys', () => {
      const tree = createShadowTree({ items: { '0': 'a', '1': 'b' } })

      expect(getValue(tree, 'items.0')).toBe('a')
      expect(getValue(tree, 'items.1')).toBe('b')
    })

    it('should handle mixed numeric and string keys', () => {
      const tree = createShadowTree({ data: { 'a': 1, '0': 2 } })

      expect(getValue(tree, 'data.a')).toBe(1)
      expect(getValue(tree, 'data.0')).toBe(2)
    })
  })

  describe('array operations', () => {
    it('should handle arrays with different element types', () => {
      const tree = createShadowTree({ mixed: [1, 'two', { three: 3 }] })

      expect(getValue(tree, 'mixed[0]')).toBe(1)
      expect(getValue(tree, 'mixed[1]')).toBe('two')
      expect(getValue(tree, 'mixed[2].three')).toBe(3)
    })

    it('should handle nested arrays', () => {
      const tree = createShadowTree({
        matrix: [
          [1, 2],
          [3, 4],
        ],
      })

      expect(getValue(tree, 'matrix[0][0]')).toBe(1)
      expect(getValue(tree, 'matrix[1][1]')).toBe(4)
    })
  })
})
