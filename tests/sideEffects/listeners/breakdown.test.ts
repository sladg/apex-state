/**
 * Smart Breakdown Tests (APEX-25)
 *
 * Tests smart change breakdown logic for listener triggering.
 * Verifies that breakdown only happens when listeners exist.
 */

import { describe, it, expect, vi } from 'vitest'
import { breakdownChanges } from '../../../src/sideEffects/listeners/breakdown'
import { createListenersRegistry } from '../../../src/sideEffects/listeners/registry'
import type { ArrayOfChanges, GenericMeta } from '../../../src/types'

interface TestState {
  user: {
    name: string
    age: number
    address: {
      city: string
      zip: string
    }
  }
  count: number
  nested: {
    level1: {
      level2: {
        value: string
      }
    }
  }
}

type TestMeta = GenericMeta

describe('breakdownChanges', () => {
  describe('no breakdown when no nested listeners', () => {
    it('does not break down when no listeners exist', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // No breakdown should happen - only original change
      expect(result).toHaveLength(1)
      expect(result[0][0]).toBe('user')
    })

    it('does not break down when only listener is at same level', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('user-listener', 'user', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // No breakdown - listener at same level
      expect(result).toHaveLength(1)
      expect(result[0][0]).toBe('user')
    })
  })

  describe('breaks down when nested listeners exist', () => {
    it('breaks down to level + 1 when listener exists', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // Should include original + user.name (because listener exists)
      expect(result.length).toBeGreaterThan(1)
      expect(result.some(([p]) => p === 'user')).toBe(true)
      expect(result.some(([p]) => p === 'user.name')).toBe(true)
      expect(result.find(([p]) => p === 'user.name')?.[1]).toBe('Bob')
    })

    it('does not break down properties without listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // Should NOT include user.age (no listener)
      expect(result.some(([p]) => p === 'user.age')).toBe(false)
      // Should NOT include user.address (no listener at exact level)
      expect(result.some(([p]) => p === 'user.address')).toBe(false)
    })

    it('breaks down multiple properties with listeners', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())
      registry.register('age-listener', 'user.age', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      expect(result.some(([p]) => p === 'user.name')).toBe(true)
      expect(result.some(([p]) => p === 'user.age')).toBe(true)
      expect(result.some(([p]) => p === 'user.address')).toBe(false)
    })
  })

  describe('recursive breakdown', () => {
    it('breaks down deeply nested paths when listeners exist', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('city-listener', 'user.address.city', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      expect(result.some(([p]) => p === 'user.address')).toBe(true)
      expect(result.some(([p]) => p === 'user.address.city')).toBe(true)
      expect(result.find(([p]) => p === 'user.address.city')?.[1]).toBe('LA')
    })

    it('stops breaking down when no deeper listeners exist', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('address-listener', 'user.address', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      expect(result.some(([p]) => p === 'user.address')).toBe(true)
      expect(result.some(([p]) => p === 'user.address.city')).toBe(false)
      expect(result.some(([p]) => p === 'user.address.zip')).toBe(false)
    })

    it('handles very deep nesting', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('value-listener', 'nested.level1.level2.value', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['nested', { level1: { level2: { value: 'updated' } } }, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      expect(result.some(([p]) => p === 'nested.level1')).toBe(true)
      expect(result.some(([p]) => p === 'nested.level1.level2')).toBe(true)
      expect(result.some(([p]) => p === 'nested.level1.level2.value')).toBe(true)
    })
  })

  describe('primitive values', () => {
    it('does not break down primitive values', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user.name', 'Bob', {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // Should not try to break down primitive string
      expect(result).toHaveLength(1)
      expect(result[0][0]).toBe('user.name')
      expect(result[0][1]).toBe('Bob')
    })

    it('does not break down arrays', () => {
      interface StateWithArray {
        items: string[]
      }

      const registry = createListenersRegistry<StateWithArray, TestMeta>()
      registry.register('items-listener', 'items', vi.fn())

      const state = { items: ['a', 'b'] }

      const changes: ArrayOfChanges<StateWithArray, TestMeta> = [
        ['items', ['x', 'y', 'z'], {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // Arrays should not be broken down
      expect(result).toHaveLength(1)
      expect(result[0][0]).toBe('items')
    })
  })

  describe('metadata preservation', () => {
    it('preserves metadata in broken down changes', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const meta: TestMeta = {}
      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user', { name: 'Bob', age: 25, address: { city: 'LA', zip: '90001' } }, meta],
      ]

      const result = breakdownChanges(changes, registry, state)

      // All broken down changes should have same metadata
      for (const [, , changeMeta] of result) {
        expect(changeMeta).toBe(meta)
      }
    })
  })

  describe('edge cases', () => {
    it('handles empty changes array', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('name-listener', 'user.name', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = []

      const result = breakdownChanges(changes, registry, state)

      expect(result).toEqual([])
    })

    it('handles null values', () => {
      const registry = createListenersRegistry<TestState, TestMeta>()
      registry.register('user-listener', 'user', vi.fn())

      const state = {
        user: { name: 'Alice', age: 30, address: { city: 'NYC', zip: '10001' } },
        count: 0,
        nested: { level1: { level2: { value: 'test' } } },
      }

      const changes: ArrayOfChanges<TestState, TestMeta> = [
        ['user' as any, null as any, {}],
      ]

      const result = breakdownChanges(changes, registry, state)

      // Should not throw, should not break down null
      expect(result).toHaveLength(1)
    })
  })
})
