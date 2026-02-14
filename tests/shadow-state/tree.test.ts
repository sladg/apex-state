/**
 * Tests for shadow state tree - Nested tree structure tests
 *
 * Tests all tree functions:
 * - createShadowTree: Create tree from objects/arrays
 * - insertNode: Add nodes to tree
 * - removeNode: Remove nodes and descendants
 *
 * Tests tree structure, parent-child relationships, metadata,
 * options (maxDepth, includeArrays, detectCircular), and edge cases.
 */

import { describe, expect, it } from 'vitest'

import {
  createShadowTree,
  insertNode,
  removeNode,
} from '../../src/shadow-state/tree'

describe('shadow-state/tree', () => {
  describe('createShadowTree', () => {
    describe('basic tree creation', () => {
      it('should create tree from simple object', () => {
        const state = { name: 'Alice', age: 30 }
        const tree = createShadowTree(state)

        expect(tree.root.value).toEqual(state)
        expect(tree.root.path).toEqual([])
        expect(tree.root.parent).toBeUndefined()
        expect(tree.root.children).toBeDefined()
        expect(tree.root.children?.size).toBe(2)
        expect(tree.nodeCount).toBe(3) // root + name + age
        expect(tree.depth).toBe(1) // root is 0, children are 1
      })

      it('should create tree from nested object', () => {
        const state = { user: { name: 'Alice', age: 30 } }
        const tree = createShadowTree(state)

        expect(tree.root.value).toEqual(state)
        expect(tree.nodeCount).toBe(4) // root + user + name + age
        expect(tree.depth).toBe(2) // root -> user -> name/age

        const userNode = tree.root.children?.get('user')
        expect(userNode).toBeDefined()
        expect(userNode?.value).toEqual({ name: 'Alice', age: 30 })
        expect(userNode?.path).toEqual(['user'])
        expect(userNode?.parent).toBe(tree.root)
        expect(userNode?.children?.size).toBe(2)
      })

      it('should create tree from array', () => {
        const state = ['apple', 'banana', 'cherry']
        const tree = createShadowTree(state)

        expect(tree.root.value).toEqual(state)
        expect(tree.nodeCount).toBe(4) // root + 3 items
        expect(tree.depth).toBe(1)
        expect(tree.root.children?.size).toBe(3)

        const item0 = tree.root.children?.get(0)
        expect(item0?.value).toBe('apple')
        expect(item0?.path).toEqual(['0'])
        expect(item0?.parent).toBe(tree.root)
      })

      it('should create tree from primitive', () => {
        const tree = createShadowTree(42)

        expect(tree.root.value).toBe(42)
        expect(tree.root.path).toEqual([])
        expect(tree.root.children).toBeUndefined()
        expect(tree.nodeCount).toBe(1)
        expect(tree.depth).toBe(0)
      })

      it('should create tree from null', () => {
        const tree = createShadowTree(null)

        expect(tree.root.value).toBeNull()
        expect(tree.nodeCount).toBe(1)
        expect(tree.depth).toBe(0)
      })

      it('should create tree from undefined', () => {
        const tree = createShadowTree(undefined)

        expect(tree.root.value).toBeUndefined()
        expect(tree.nodeCount).toBe(1)
        expect(tree.depth).toBe(0)
      })
    })

    describe('nested structures', () => {
      it('should create deeply nested tree', () => {
        const state = {
          user: {
            profile: {
              contact: {
                email: 'alice@example.com',
              },
            },
          },
        }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(5) // root + user + profile + contact + email
        expect(tree.depth).toBe(4)

        // Verify path at each level
        const user = tree.root.children?.get('user')
        expect(user?.path).toEqual(['user'])

        const profile = user?.children?.get('profile')
        expect(profile?.path).toEqual(['user', 'profile'])

        const contact = profile?.children?.get('contact')
        expect(contact?.path).toEqual(['user', 'profile', 'contact'])

        const email = contact?.children?.get('email')
        expect(email?.path).toEqual(['user', 'profile', 'contact', 'email'])
        expect(email?.value).toBe('alice@example.com')
      })

      it('should create tree with mixed objects and arrays', () => {
        const state = {
          users: [
            { name: 'Alice', tags: ['developer'] },
            { name: 'Bob', tags: ['designer'] },
          ],
        }
        const tree = createShadowTree(state)

        // root + users + user[0] + user[0].name + user[0].tags + user[0].tags[0]
        //       + user[1] + user[1].name + user[1].tags + user[1].tags[0]
        expect(tree.nodeCount).toBe(10)
        expect(tree.depth).toBe(4) // users -> users[0] -> tags -> tags[0]

        const users = tree.root.children?.get('users')
        expect(users?.children?.size).toBe(2)

        const user0 = users?.children?.get(0)
        expect(user0?.path).toEqual(['users', '0'])
        expect(user0?.children?.get('name')?.value).toBe('Alice')

        const tags0 = user0?.children?.get('tags')
        expect(tags0?.children?.get(0)?.value).toBe('developer')
      })

      it('should handle empty objects', () => {
        const state = { empty: {} }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2) // root + empty
        expect(tree.depth).toBe(1)

        const empty = tree.root.children?.get('empty')
        expect(empty?.value).toEqual({})
        expect(empty?.children).toBeUndefined() // No children for empty object
      })

      it('should handle empty arrays', () => {
        const state = { items: [] }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2) // root + items
        expect(tree.depth).toBe(1)

        const items = tree.root.children?.get('items')
        expect(items?.value).toEqual([])
        expect(items?.children).toBeUndefined() // No children for empty array
      })
    })

    describe('parent-child relationships', () => {
      it('should maintain bidirectional parent-child links', () => {
        const state = { a: { b: { c: 'value' } } }
        const tree = createShadowTree(state)

        const a = tree.root.children?.get('a')
        const b = a?.children?.get('b')
        const c = b?.children?.get('c')

        // Check parent links
        expect(a?.parent).toBe(tree.root)
        expect(b?.parent).toBe(a)
        expect(c?.parent).toBe(b)

        // Check children links
        expect(tree.root.children?.get('a')).toBe(a)
        expect(a?.children?.get('b')).toBe(b)
        expect(b?.children?.get('c')).toBe(c)
      })

      it('should maintain correct paths', () => {
        const state = { a: { b: { c: 'value' } } }
        const tree = createShadowTree(state)

        const a = tree.root.children?.get('a')
        const b = a?.children?.get('b')
        const c = b?.children?.get('c')

        expect(tree.root.path).toEqual([])
        expect(a?.path).toEqual(['a'])
        expect(b?.path).toEqual(['a', 'b'])
        expect(c?.path).toEqual(['a', 'b', 'c'])
      })
    })

    describe('options', () => {
      it('should respect maxDepth option', () => {
        const state = { a: { b: { c: { d: 'deep' } } } }
        const tree = createShadowTree(state, { maxDepth: 2 })

        // root (depth 0) -> a (depth 1) -> b (depth 2)
        // c should not be created because it would be at depth 3
        expect(tree.depth).toBe(2)
        expect(tree.nodeCount).toBe(3) // root + a + b

        const a = tree.root.children?.get('a')
        const b = a?.children?.get('b')
        expect(b).toBeDefined()
        expect(b?.children).toBeUndefined() // No children beyond maxDepth
      })

      it('should respect includeArrays: false option', () => {
        const state = { items: ['a', 'b', 'c'] }
        const tree = createShadowTree(state, { includeArrays: false })

        expect(tree.nodeCount).toBe(2) // root + items (array treated as leaf)
        expect(tree.depth).toBe(1)

        const items = tree.root.children?.get('items')
        expect(items?.value).toEqual(['a', 'b', 'c'])
        expect(items?.children).toBeUndefined() // Array children not created
      })

      it('should detect circular references', () => {
        const state: any = { name: 'root' }
        state.self = state // Circular reference

        const tree = createShadowTree(state, { detectCircular: true })

        // root + name + self (but self has no children because circular)
        expect(tree.nodeCount).toBe(3)

        const self = tree.root.children?.get('self')
        expect(self).toBeDefined()
        expect(self?.children).toBeUndefined() // Circular reference detected
      })

      it('should handle nested circular references', () => {
        const parent: any = { name: 'parent', child: { name: 'child' } }
        parent.child.parent = parent // Circular reference at nested level

        const tree = createShadowTree(parent, { detectCircular: true })

        // root + name + child + child.name + child.parent (no children)
        expect(tree.nodeCount).toBe(5)

        const child = tree.root.children?.get('child')
        const childParent = child?.children?.get('parent')
        expect(childParent).toBeDefined()
        expect(childParent?.children).toBeUndefined() // Circular detected
      })
    })

    describe('metadata tracking', () => {
      it('should track node count correctly', () => {
        const state = { a: 1, b: { c: 2, d: 3 } }
        const tree = createShadowTree(state)

        // root + a + b + c + d = 5
        expect(tree.nodeCount).toBe(5)
      })

      it('should track depth correctly for flat objects', () => {
        const state = { a: 1, b: 2, c: 3 }
        const tree = createShadowTree(state)

        expect(tree.depth).toBe(1) // All children at depth 1
      })

      it('should track depth correctly for nested objects', () => {
        const state = { a: { b: { c: { d: 1 } } } }
        const tree = createShadowTree(state)

        expect(tree.depth).toBe(4) // a=1, b=2, c=3, d=4
      })

      it('should track depth correctly for arrays', () => {
        const state = [[['deep']]]
        const tree = createShadowTree(state)

        expect(tree.depth).toBe(3) // [0] = 1, [0][0] = 2, [0][0][0] = 3
      })
    })

    describe('edge cases', () => {
      it('should handle null values in objects', () => {
        const state = { value: null }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2) // root + value
        const value = tree.root.children?.get('value')
        expect(value?.value).toBeNull()
        expect(value?.children).toBeUndefined()
      })

      it('should handle undefined values in objects', () => {
        const state = { value: undefined }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2) // root + value
        const value = tree.root.children?.get('value')
        expect(value?.value).toBeUndefined()
        expect(value?.children).toBeUndefined()
      })

      it('should handle boolean values', () => {
        const state = { enabled: true, disabled: false }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(3)
        expect(tree.root.children?.get('enabled')?.value).toBe(true)
        expect(tree.root.children?.get('disabled')?.value).toBe(false)
      })

      it('should handle Date objects', () => {
        const date = new Date('2024-01-01')
        const state = { created: date }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2)
        const created = tree.root.children?.get('created')
        expect(created?.value).toBe(date)
        expect(created?.children).toBeUndefined() // Date is not a plain object
      })

      it('should handle RegExp objects', () => {
        const regex = /test/gi
        const state = { pattern: regex }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(2)
        const pattern = tree.root.children?.get('pattern')
        expect(pattern?.value).toBe(regex)
        expect(pattern?.children).toBeUndefined() // RegExp is not a plain object
      })

      it('should handle numeric string keys in objects', () => {
        const state = { '0': 'zero', '1': 'one', 'normal': 'key' }
        const tree = createShadowTree(state)

        expect(tree.nodeCount).toBe(4)
        expect(tree.root.children?.get('0')?.value).toBe('zero')
        expect(tree.root.children?.get('1')?.value).toBe('one')
        expect(tree.root.children?.get('normal')?.value).toBe('key')
      })

      it('should handle sparse arrays', () => {
        const state: string[] = []
        state[0] = 'first'
        state[5] = 'sixth'
        const tree = createShadowTree(state)

        // root + index 0 + indices 1-4 (undefined) + index 5
        expect(tree.nodeCount).toBe(7) // root + 6 elements (including undefined)
        expect(tree.root.children?.get(0)?.value).toBe('first')
        expect(tree.root.children?.get(1)?.value).toBeUndefined()
        expect(tree.root.children?.get(5)?.value).toBe('sixth')
      })
    })
  })

  describe('insertNode', () => {
    describe('basic insertion', () => {
      it('should insert node at simple path', () => {
        const tree = createShadowTree({})
        const node = insertNode(tree, ['name'], 'Alice')

        expect(node.value).toBe('Alice')
        expect(node.path).toEqual(['name'])
        expect(node.parent).toBe(tree.root)
        expect(tree.root.children?.get('name')).toBe(node)
        expect(tree.nodeCount).toBe(2) // root + name
      })

      it('should insert node at nested path', () => {
        const tree = createShadowTree({})
        const node = insertNode(tree, ['user', 'name'], 'Alice')

        expect(node.value).toBe('Alice')
        expect(node.path).toEqual(['user', 'name'])
        expect(tree.nodeCount).toBe(3) // root + user + name

        const user = tree.root.children?.get('user')
        expect(user).toBeDefined()
        expect(user?.children?.get('name')).toBe(node)
        expect(node.parent).toBe(user)
      })

      it('should insert object with children', () => {
        const tree = createShadowTree({})
        const value = { name: 'Alice', age: 30 }
        const node = insertNode(tree, ['user'], value)

        expect(node.value).toEqual(value)
        expect(node.children?.size).toBe(2)
        expect(tree.nodeCount).toBe(4) // root + user + name + age

        const name = node.children?.get('name')
        expect(name?.value).toBe('Alice')
        expect(name?.path).toEqual(['user', 'name'])
        expect(name?.parent).toBe(node)
      })

      it('should insert array with children', () => {
        const tree = createShadowTree({})
        const value = ['a', 'b', 'c']
        const node = insertNode(tree, ['items'], value)

        expect(node.value).toEqual(value)
        expect(node.children?.size).toBe(3)
        expect(tree.nodeCount).toBe(5) // root + items + 3 elements

        const item0 = node.children?.get(0)
        expect(item0?.value).toBe('a')
        expect(item0?.path).toEqual(['items', '0'])
      })
    })

    describe('path creation', () => {
      it('should create intermediate nodes', () => {
        const tree = createShadowTree({})
        const node = insertNode(tree, ['a', 'b', 'c'], 'value')

        expect(tree.nodeCount).toBe(4) // root + a + b + c
        expect(node.value).toBe('value')
        expect(node.path).toEqual(['a', 'b', 'c'])

        const a = tree.root.children?.get('a')
        const b = a?.children?.get('b')
        expect(b?.children?.get('c')).toBe(node)
      })

      it('should not recreate existing intermediate nodes', () => {
        const tree = createShadowTree({ a: { b: {} } })
        const initialCount = tree.nodeCount

        const node = insertNode(tree, ['a', 'b', 'c'], 'value')

        expect(tree.nodeCount).toBe(initialCount + 1) // Only c added
        expect(node.path).toEqual(['a', 'b', 'c'])
      })
    })

    describe('metadata updates', () => {
      it('should update nodeCount when inserting', () => {
        const tree = createShadowTree({})
        expect(tree.nodeCount).toBe(1)

        insertNode(tree, ['a'], 1)
        expect(tree.nodeCount).toBe(2)

        insertNode(tree, ['b'], { c: 2 })
        expect(tree.nodeCount).toBe(4) // +b +c
      })

      it('should update depth when inserting deeper nodes', () => {
        const tree = createShadowTree({ a: 1 })
        expect(tree.depth).toBe(1)

        insertNode(tree, ['b', 'c', 'd'], 'value')
        expect(tree.depth).toBe(3)
      })

      it('should not update depth when inserting shallower nodes', () => {
        const tree = createShadowTree({ a: { b: { c: 1 } } })
        const initialDepth = tree.depth

        insertNode(tree, ['x'], 'value')
        expect(tree.depth).toBe(initialDepth)
      })
    })

    describe('options support', () => {
      it('should respect maxDepth option', () => {
        const tree = createShadowTree({})
        const value = { a: { b: { c: 'deep' } } }
        const node = insertNode(tree, ['nested'], value, { maxDepth: 2 })

        // nested (depth 1) -> a (depth 2), but b would be depth 3
        expect(node.children?.size).toBe(1)
        const a = node.children?.get('a')
        expect(a?.children).toBeUndefined() // maxDepth prevents b
      })

      it('should respect includeArrays: false option', () => {
        const tree = createShadowTree({})
        const value = ['a', 'b', 'c']
        const node = insertNode(tree, ['items'], value, {
          includeArrays: false,
        })

        expect(node.value).toEqual(value)
        expect(node.children).toBeUndefined() // Array children not created
        expect(tree.nodeCount).toBe(2) // root + items
      })
    })

    describe('edge cases', () => {
      it('should throw error for empty path', () => {
        const tree = createShadowTree({})
        expect(() => insertNode(tree, [], 'value')).toThrow(
          'Cannot insert at root path',
        )
      })

      it('should handle inserting null value', () => {
        const tree = createShadowTree({})
        const node = insertNode(tree, ['value'], null)

        expect(node.value).toBeNull()
        expect(node.children).toBeUndefined()
      })

      it('should handle inserting undefined value', () => {
        const tree = createShadowTree({})
        const node = insertNode(tree, ['value'], undefined)

        expect(node.value).toBeUndefined()
        expect(node.children).toBeUndefined()
      })

      it('should replace existing node at path', () => {
        const tree = createShadowTree({ name: 'Alice' })
        expect(tree.root.children?.get('name')?.value).toBe('Alice')

        const node = insertNode(tree, ['name'], 'Bob')
        expect(node.value).toBe('Bob')
        expect(tree.root.children?.get('name')?.value).toBe('Bob')
      })
    })
  })

  describe('removeNode', () => {
    describe('basic removal', () => {
      it('should remove leaf node', () => {
        const tree = createShadowTree({ a: 1, b: 2 })
        const removed = removeNode(tree, ['a'])

        expect(removed).toBe(1) // 1 node removed
        expect(tree.root.children?.has('a')).toBe(false)
        expect(tree.root.children?.has('b')).toBe(true)
        expect(tree.nodeCount).toBe(2) // root + b
      })

      it('should remove nested node', () => {
        const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
        const removed = removeNode(tree, ['user', 'name'])

        expect(removed).toBe(1)
        expect(tree.nodeCount).toBe(3) // root + user + age

        const user = tree.root.children?.get('user')
        expect(user?.children?.has('name')).toBe(false)
        expect(user?.children?.has('age')).toBe(true)
      })
    })

    describe('removal with descendants', () => {
      it('should remove node with children', () => {
        const tree = createShadowTree({ user: { name: 'Alice', age: 30 } })
        const removed = removeNode(tree, ['user'])

        expect(removed).toBe(3) // user + name + age
        expect(tree.root.children?.has('user')).toBe(false)
        expect(tree.nodeCount).toBe(1) // just root
      })

      it('should remove deeply nested subtree', () => {
        const tree = createShadowTree({
          a: { b: { c: { d: 1, e: 2 } } },
        })
        const removed = removeNode(tree, ['a', 'b'])

        expect(removed).toBe(4) // b + c + d + e
        expect(tree.nodeCount).toBe(2) // root + a

        const a = tree.root.children?.get('a')
        expect(a?.children?.has('b')).toBe(false)
      })

      it('should remove array with elements', () => {
        const tree = createShadowTree({ items: ['a', 'b', 'c'] })
        const removed = removeNode(tree, ['items'])

        expect(removed).toBe(4) // items + 3 elements
        expect(tree.root.children?.has('items')).toBe(false)
        expect(tree.nodeCount).toBe(1)
      })
    })

    describe('metadata updates', () => {
      it('should update nodeCount when removing', () => {
        const tree = createShadowTree({ a: 1, b: { c: 2, d: 3 } })
        const initialCount = tree.nodeCount

        const removed = removeNode(tree, ['b'])
        expect(tree.nodeCount).toBe(initialCount - removed)
        expect(tree.nodeCount).toBe(2) // root + a
      })

      it('should update depth when removing deepest node', () => {
        const tree = createShadowTree({ a: { b: { c: 1 } }, x: 1 })
        expect(tree.depth).toBe(3)

        removeNode(tree, ['a', 'b', 'c'])
        expect(tree.depth).toBe(2) // a.b remains at depth 2
      })

      it('should not recalculate depth when not removing deepest node', () => {
        const tree = createShadowTree({ a: { b: { c: 1 } }, x: 1 })
        const initialDepth = tree.depth

        removeNode(tree, ['x'])
        expect(tree.depth).toBe(initialDepth) // Depth unchanged
      })
    })

    describe('non-existent paths', () => {
      it('should return 0 for non-existent path', () => {
        const tree = createShadowTree({ a: 1 })
        const removed = removeNode(tree, ['b'])

        expect(removed).toBe(0)
        expect(tree.nodeCount).toBe(2) // Unchanged
      })

      it('should return 0 for non-existent nested path', () => {
        const tree = createShadowTree({ a: { b: 1 } })
        const removed = removeNode(tree, ['a', 'c'])

        expect(removed).toBe(0)
        expect(tree.nodeCount).toBe(3) // Unchanged
      })

      it('should return 0 when intermediate path does not exist', () => {
        const tree = createShadowTree({ a: 1 })
        const removed = removeNode(tree, ['b', 'c', 'd'])

        expect(removed).toBe(0)
        expect(tree.nodeCount).toBe(2) // Unchanged
      })
    })

    describe('edge cases', () => {
      it('should throw error when removing root', () => {
        const tree = createShadowTree({ a: 1 })
        expect(() => removeNode(tree, [])).toThrow('Cannot remove root node')
      })

      it('should handle removing from empty children', () => {
        const tree = createShadowTree({ a: {} })
        const removed = removeNode(tree, ['a', 'b'])

        expect(removed).toBe(0)
      })

      it('should handle removing last child', () => {
        const tree = createShadowTree({ a: 1 })
        const removed = removeNode(tree, ['a'])

        expect(removed).toBe(1)
        expect(tree.root.children?.size).toBe(0)
        expect(tree.nodeCount).toBe(1)
      })

      it('should update parent children map', () => {
        const tree = createShadowTree({ a: 1, b: 2, c: 3 })
        removeNode(tree, ['b'])

        expect(tree.root.children?.has('b')).toBe(false)
        expect(tree.root.children?.size).toBe(2)
        expect(tree.root.children?.has('a')).toBe(true)
        expect(tree.root.children?.has('c')).toBe(true)
      })
    })
  })

  describe('integration scenarios', () => {
    it('should handle insert then remove', () => {
      const tree = createShadowTree({ a: 1 })
      insertNode(tree, ['b'], 2)
      expect(tree.nodeCount).toBe(3)

      removeNode(tree, ['b'])
      expect(tree.nodeCount).toBe(2)
      expect(tree.root.children?.has('b')).toBe(false)
    })

    it('should handle multiple insertions and removals', () => {
      const tree = createShadowTree({})

      insertNode(tree, ['a'], { b: 1 })
      insertNode(tree, ['c'], { d: 2 })
      insertNode(tree, ['e'], { f: { g: 3 } })

      expect(tree.nodeCount).toBe(8) // root + a + b + c + d + e + f + g

      removeNode(tree, ['c'])
      expect(tree.nodeCount).toBe(6)

      removeNode(tree, ['e', 'f'])
      expect(tree.nodeCount).toBe(4) // root + a + b + e
    })

    it('should maintain tree integrity after complex operations', () => {
      const tree = createShadowTree({ user: { name: 'Alice' } })

      insertNode(tree, ['user', 'age'], 30)
      insertNode(tree, ['todos'], ['Task 1', 'Task 2'])

      expect(tree.root.children?.size).toBe(2)

      const user = tree.root.children?.get('user')
      expect(user?.children?.size).toBe(2)

      removeNode(tree, ['user', 'name'])
      expect(user?.children?.size).toBe(1)
      expect(user?.children?.has('age')).toBe(true)

      // Verify parent-child links still valid
      const age = user?.children?.get('age')
      expect(age?.parent).toBe(user)
      expect(user?.parent).toBe(tree.root)
    })
  })
})
