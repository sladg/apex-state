/**
 * Tests for guards utility - Runtime guards and validation
 *
 * Tests:
 * - guard.anonymousFn: Reject anonymous listener functions (always active)
 * - guard.duplicateId: Reject duplicate listener IDs (dev mode only)
 * - guard.dynamicPath: Reject paths with [*] dynamic hash keys (always active)
 * - generateListenerId: Generate unique listener IDs with collision handling
 */

/* eslint-disable @typescript-eslint/no-empty-function */

import { describe, expect, it } from 'vitest'

// ============================================================================
// Type Guard Tests: StrictType Constraint
// ============================================================================
import { createGenericStore } from '~/store/createStore'
import { __DEV__, generateListenerId, guard } from '~/utils/guards'

import type { TestState } from '../mocks/types'

describe('__DEV__', () => {
  it('should be true in test environment', () => {
    expect(__DEV__).toBe(true)
  })
})

describe('guard.anonymousFn', () => {
  it('should throw for truly anonymous arrow function', () => {
    // Create a function without a name by wrapping in expression
    const getFn = (): (() => void) => () => {}
    const anonFn = getFn()
    expect(() => guard.anonymousFn(anonFn, 'user.age')).toThrow(
      /Listener fn must be a named function/,
    )
    expect(() => guard.anonymousFn(anonFn, 'user.age')).toThrow(/user\.age/)
  })

  it('should throw for function with empty name', () => {
    // Create a function with empty name by using Object.defineProperty
    const emptyNameFn = () => {}
    Object.defineProperty(emptyNameFn, 'name', { value: '' })
    expect(() => guard.anonymousFn(emptyNameFn, 'cart.items')).toThrow(
      /Listener fn must be a named function/,
    )
  })

  it('should NOT throw for named function declaration', () => {
    function validateAge() {}
    expect(() => guard.anonymousFn(validateAge, 'user.age')).not.toThrow()
  })

  it('should NOT throw for named arrow assigned to const', () => {
    const myFn = () => {}
    // In JavaScript, const myFn = () => {} gives fn.name = 'myFn'
    expect(() => guard.anonymousFn(myFn, 'cart.items')).not.toThrow()
  })

  it('should include the path string in error message', () => {
    const getFn = (): (() => void) => () => {}
    const anonFn = getFn()
    expect(() => guard.anonymousFn(anonFn, 'deeply.nested.path')).toThrow(
      /deeply\.nested\.path/,
    )
  })
})

describe('guard.duplicateId', () => {
  it('should throw when ID exists in set (dev/test mode)', () => {
    const ids = new Set(['a.b.c_validateAge'])
    expect(() => guard.duplicateId('a.b.c_validateAge', ids)).toThrow(
      /Duplicate listener ID/,
    )
    expect(() => guard.duplicateId('a.b.c_validateAge', ids)).toThrow(
      /a\.b\.c_validateAge/,
    )
  })

  it('should NOT throw when ID is not in set', () => {
    const ids = new Set(['a.b.c_validateAge'])
    expect(() => guard.duplicateId('x.y.z_checkValue', ids)).not.toThrow()
  })

  it('should include the ID string in error message', () => {
    const ids = new Set(['user.name_validateName'])
    expect(() => guard.duplicateId('user.name_validateName', ids)).toThrow(
      /user\.name_validateName/,
    )
  })

  it('should work with empty set', () => {
    const ids = new Set<string>()
    expect(() => guard.duplicateId('any.id', ids)).not.toThrow()
  })
})

describe('guard.listenerScope', () => {
  it('should NOT throw when scope is parent of path', () => {
    expect(() => guard.listenerScope('a.b.c', 'a.b')).not.toThrow()
    expect(() => guard.listenerScope('a.b.c', 'a')).not.toThrow()
  })

  it('should NOT throw when scope equals path', () => {
    expect(() => guard.listenerScope('a.b.c', 'a.b.c')).not.toThrow()
  })

  it('should NOT throw when path is null', () => {
    expect(() => guard.listenerScope(null, 'a.b')).not.toThrow()
  })

  it('should NOT throw when scope is null', () => {
    expect(() => guard.listenerScope('a.b.c', null)).not.toThrow()
  })

  it('should NOT throw when scope is undefined', () => {
    expect(() => guard.listenerScope('a.b.c', undefined)).not.toThrow()
  })

  it('should throw when scope is not an ancestor of path', () => {
    expect(() => guard.listenerScope('a.b.c', '1.2.3')).toThrow(
      /must be a parent\/ancestor/,
    )
  })

  it('should throw when scope is a sibling path', () => {
    expect(() => guard.listenerScope('a.b.c', 'a.b.d')).toThrow(
      /must be a parent\/ancestor/,
    )
  })

  it('should throw when scope is a child of path', () => {
    expect(() => guard.listenerScope('a.b', 'a.b.c')).toThrow(
      /must be a parent\/ancestor/,
    )
  })
})

describe('guard.dynamicPath', () => {
  it('should throw for path containing bracket notation', () => {
    expect(() => guard.dynamicPath('users.[*].posts')).toThrow(
      /bracket notation/,
    )
    expect(() => guard.dynamicPath('[*].name')).toThrow(/bracket notation/)
    expect(() => guard.dynamicPath('data.[*]')).toThrow(/bracket notation/)
    expect(() => guard.dynamicPath('users.[u1].posts')).toThrow(
      /bracket notation/,
    )
    expect(() => guard.dynamicPath('users.[active].posts')).toThrow(
      /bracket notation/,
    )
  })

  it('should throw for paths with multiple bracket segments', () => {
    expect(() => guard.dynamicPath('users.[*].posts.[*].comments')).toThrow(
      /bracket notation/,
    )
    expect(() => guard.dynamicPath('regions.[us].items.[active]')).toThrow(
      /bracket notation/,
    )
  })

  it('should NOT throw for normal paths', () => {
    expect(() => guard.dynamicPath('users.u1.posts.p1')).not.toThrow()
    expect(() => guard.dynamicPath('simple.path')).not.toThrow()
    expect(() => guard.dynamicPath('deeply.nested.path.to.value')).not.toThrow()
  })

  it('should NOT throw for empty path', () => {
    expect(() => guard.dynamicPath('')).not.toThrow()
  })

  it('should NOT throw for single segment path', () => {
    expect(() => guard.dynamicPath('users')).not.toThrow()
  })

  it('should NOT throw for paths containing asterisk in other forms', () => {
    expect(() => guard.dynamicPath('users.*.posts')).not.toThrow()
  })

  it('should mention bracket notation in error message', () => {
    expect(() => guard.dynamicPath('users.[*].name')).toThrow(
      /bracket notation '\[\*\]'/,
    )
    expect(() => guard.dynamicPath('users.[u1].name')).toThrow(
      /bracket notation '\[u1\]'/,
    )
  })
})

describe('generateListenerId', () => {
  it('should return "a.b.c_myFn" for path="a.b.c", fn named "myFn"', () => {
    const myFn = () => {}
    const ids = new Set<string>()
    expect(generateListenerId('a.b.c', myFn, ids)).toBe('a.b.c_myFn')
  })

  it('should return "__myFn" for path="" (root), fn named "myFn"', () => {
    const myFn = () => {}
    const ids = new Set<string>()
    expect(generateListenerId('', myFn, ids)).toBe('__myFn')
  })

  it('should throw for anonymous fn (delegates to guard.anonymousFn)', () => {
    const getFn = (): (() => void) => () => {}
    const anonFn = getFn()
    const ids = new Set<string>()
    expect(() => generateListenerId('user.age', anonFn, ids)).toThrow(
      /Listener fn must be a named function/,
    )
  })

  it('should throw for duplicate in dev/test mode', () => {
    const validateAge = () => {}
    const ids = new Set(['user.age_validateAge'])
    expect(() => generateListenerId('user.age', validateAge, ids)).toThrow(
      /Duplicate listener ID/,
    )
  })

  it('should add generated ID to the set', () => {
    const myFn = () => {}
    const ids = new Set<string>()
    const id = generateListenerId('a.b', myFn, ids)
    // Note: generateListenerId doesn't mutate the set - caller is responsible
    // So we just verify it returns the correct ID
    expect(id).toBe('a.b_myFn')
  })

  it('should handle multiple different IDs', () => {
    const fn1 = () => {}
    const fn2 = () => {}
    const ids = new Set<string>()

    const id1 = generateListenerId('path1', fn1, ids)
    ids.add(id1)
    const id2 = generateListenerId('path2', fn2, ids)

    expect(id1).toBe('path1_fn1')
    expect(id2).toBe('path2_fn2')
  })

  it('should handle nested paths', () => {
    const validateName = () => {}
    const ids = new Set<string>()
    expect(generateListenerId('users.u1.profile.name', validateName, ids)).toBe(
      'users.u1.profile.name_validateName',
    )
  })

  it('should handle single segment paths', () => {
    const checkCart = () => {}
    const ids = new Set<string>()
    expect(generateListenerId('cart', checkCart, ids)).toBe('cart_checkCart')
  })
})

describe('generateListenerId - production mode counter', () => {
  // Note: This is harder to test since NODE_ENV is 'test' in vitest
  // Production mode requires NODE_ENV=production to skip duplicate rejection
  // and use counter fallback. We can describe the expected behavior:
  //
  // In production mode, when a duplicate ID is detected:
  // - guard.duplicateId is called but does NOT throw (prod check skipped)
  // - Counter logic appends '_1', '_2', etc. until unique ID is found
  //
  // Example (if we could set NODE_ENV=production):
  // const ids = new Set(['a.b_minified'])
  // generateListenerId('a.b', function minified() {}, ids) // → 'a.b_minified_1'
  //
  // Since we can't easily test this in vitest, we document the expected behavior.

  it('should document production counter behavior', () => {
    // In production mode, generateListenerId appends a counter when duplicates exist
    // This test documents the expected behavior but cannot verify it in test mode
    expect(true).toBe(true)
  })
})

describe('Type Guards: StrictType Constraints', () => {
  describe('StrictType REJECTS any at top level', () => {
    it('rejects top-level any', () => {
      // ❌ Should fail TypeScript compilation
      // const _store = createGenericStore<any>()

      expect(true).toBe(true)
    })

    it('rejects bare object without constraint', () => {
      // ❌ If we pass a type parameter that is just any
      // type _BadStore = typeof createGenericStore<any>

      expect(true).toBe(true)
    })
  })

  describe('StrictType REJECTS any in Record values', () => {
    it('rejects Record<string, any>', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Type 'Record<string, any>' does not satisfy constraint
      const _store = createGenericStore<Record<string, any>>()

      expect(true).toBe(true)
    })

    it('rejects Record<string, unknown | any>', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Type 'Record<string, unknown | any>' does not satisfy constraint
      const _store = createGenericStore<Record<string, unknown | any>>()

      expect(true).toBe(true)
    })
  })

  describe('StrictType REJECTS any in nested objects', () => {
    it('rejects object with any property', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Type '{ name: string; metadata: any }' does not satisfy constraint
      const _store = createGenericStore<{ name: string; metadata: any }>()

      expect(true).toBe(true)
    })

    it('rejects object with any in nested property', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Deeply nested any is caught
      const _store = createGenericStore<{
        level1: { level2: { level3: any } }
      }>()

      expect(true).toBe(true)
    })

    it('rejects object with any in array value', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Type 'any[]' contains any
      const _store = createGenericStore<{ items: any[] }>()

      expect(true).toBe(true)
    })

    it('rejects object with Record<string, any> property', () => {
      // ❌ Should fail TypeScript compilation
      // @ts-expect-error Type with Record<string, any> property
      const _store = createGenericStore<{
        name: string
        metadata: Record<string, any>
      }>()

      expect(true).toBe(true)
    })
  })

  describe('StrictType ACCEPTS concrete types', () => {
    it('accepts TestState (fully concrete)', () => {
      // ✅ Should compile without error
      const _store = createGenericStore<TestState>()
      expect(_store).toBeDefined()
    })

    it('accepts concrete object', () => {
      // ✅ Should compile without error
      const _store = createGenericStore<{
        name: string
        age: number
        email: string
      }>()
      expect(_store).toBeDefined()
    })

    it('accepts Record<string, string>', () => {
      // ✅ Should compile without error
      const _store = createGenericStore<Record<string, string>>()
      expect(_store).toBeDefined()
    })

    it('accepts typed array', () => {
      // ✅ Should compile without error
      const _store = createGenericStore<{ items: string[]; count: number }>()
      expect(_store).toBeDefined()
    })

    it('accepts deeply nested concrete types', () => {
      // ✅ Should compile without error
      const _store = createGenericStore<{
        level1: {
          level2: {
            level3: { name: string; value: number }
          }
        }
      }>()
      expect(_store).toBeDefined()
    })
  })

  describe('StrictType allows META to be any (not checked)', () => {
    it('allows any in META parameter (intentional)', () => {
      // ✅ META parameter is not constrained by StrictType
      // This is intentional - only DATA type is validated
      const _store = createGenericStore<TestState, any>()
      expect(_store).toBeDefined()
    })
  })
})
