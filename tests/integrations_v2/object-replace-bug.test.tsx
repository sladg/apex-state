/**
 * Bug Reproduction: Object Replace at Shallow Path — Extra Sibling Keys in finalChanges
 *
 * Customer report:
 *   When setting a big object at a 1-level path (e.g., `user`),
 *   the `initialChange` value is correct but `finalChanges` shows extra sibling keys
 *   from the old state (previously there, should have been replaced).
 *
 * Example:
 *   Old state: { user: { name: "Bob", age: 30 } }
 *   Set: setValue('user', { name: "Alice" })
 *   Expected finalChange.value: { name: "Alice" }
 *   Reported finalChange.value: { name: "Alice", age: 30 }  ← BUG
 */

import { describe, expect, it } from 'vitest'

import { createGenericStore } from '../../src'
import { flushEffects, mountStore } from '../utils/react'

// ---------------------------------------------------------------------------
// State shapes
// ---------------------------------------------------------------------------

interface UserState {
  user: {
    name: string
    age?: number
    email?: string
    role?: string
  }
}

interface NestedState {
  user: {
    profile: {
      name: string
      age?: number
      email?: string
    }
    settings?: {
      theme?: string
      language?: string
    }
  }
  meta?: {
    created: string
    updated?: string
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe('Object Replace at Shallow Path — Bug Reproduction', () => {
  describe('No side-effects — basic object replacement', () => {
    it('should replace entire object at 1-level path without preserving old keys', async () => {
      // Customer scenario: setting { name: "Alice" } on path "user"
      // where old state had { name: "Bob", age: 30 }
      const store = createGenericStore<UserState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(store, {
        user: { name: 'Bob', age: 30 },
      })

      await flushEffects()
      storeInstance._debug!.clear()

      // Act: replace the entire user object with a smaller object (no age)
      setValue('user', { name: 'Alice' })
      await flushEffects()

      // Assert: applied change should NOT contain old "age" key
      const call = storeInstance._debug!.calls[0]
      expect(call).toBeDefined()

      const userChange = call.applied.find((c) => c.path === 'user')
      expect(userChange).toBeDefined()
      expect(userChange!.value).toEqual({ name: 'Alice' })
      expect(
        (userChange!.value as Record<string, unknown>)['age'],
      ).toBeUndefined()

      // Assert: actual state should also not have old keys
      const stateUser = storeInstance.state.user
      expect(stateUser).toEqual({ name: 'Alice' })
      expect(stateUser.age).toBeUndefined()
    })

    it('should replace object with empty object at 1-level path', async () => {
      // Edge case: completely empty replacement
      const store = createGenericStore<UserState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(store, {
        user: { name: 'Bob', age: 30, email: 'bob@example.com', role: 'admin' },
      })

      await flushEffects()
      storeInstance._debug!.clear()

      // Replace with minimal object
      setValue('user', { name: 'Alice' })
      await flushEffects()

      const call = storeInstance._debug!.calls[0]
      const userChange = call?.applied.find((c) => c.path === 'user')

      expect(userChange!.value).toEqual({ name: 'Alice' })
      // No email, role, or age should bleed through
      const v = userChange!.value as Record<string, unknown>
      expect(v['email']).toBeUndefined()
      expect(v['role']).toBeUndefined()
      expect(v['age']).toBeUndefined()
    })

    it('initialChange value and finalChange value should match for same path', async () => {
      // Both should show { name: "Alice" } — not the old value or mixed
      const store = createGenericStore<UserState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(store, {
        user: { name: 'Bob', age: 30 },
      })

      await flushEffects()
      storeInstance._debug!.clear()

      const newUser = { name: 'Alice' }
      setValue('user', newUser)
      await flushEffects()

      const call = storeInstance._debug!.calls[0]

      // Input and applied should both refer to the new value
      const inputEntry = call.input.find(([p]) => p === 'user')
      const appliedEntry = call.applied.find((c) => c.path === 'user')

      expect(inputEntry).toBeDefined()
      expect(appliedEntry).toBeDefined()

      // Input value should be the new value
      expect(inputEntry![1]).toEqual({ name: 'Alice' })

      // Applied value should also be the new value (not old state merged in)
      expect(appliedEntry!.value).toEqual({ name: 'Alice' })
    })
  })

  describe('With sync pairs — object replacement when sync is configured', () => {
    it('should not contaminate the original change value when sync paths are registered', async () => {
      // Hypothesis: Case 1 (exact match) sync processing might interact with original change
      interface SyncState {
        user: { name: string; age?: number }
        userCopy: { name: string; age?: number }
      }

      const store = createGenericStore<SyncState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(
        store,
        {
          user: { name: 'Bob', age: 30 },
          userCopy: { name: 'Bob', age: 30 },
        },
        {
          sideEffectsId: 'sync-test',
          sideEffects: {
            syncPaths: [['user', 'userCopy']],
          },
        },
      )

      await flushEffects()
      storeInstance._debug!.clear()

      setValue('user', { name: 'Alice' })
      await flushEffects()

      const call = storeInstance._debug!.calls[0]
      const userChange = call.applied.find((c) => c.path === 'user')
      const userCopyChange = call.applied.find((c) => c.path === 'userCopy')

      // The original user change should have { name: "Alice" } only
      expect(userChange).toBeDefined()
      expect(userChange!.value).toEqual({ name: 'Alice' })
      expect(
        (userChange!.value as Record<string, unknown>)['age'],
      ).toBeUndefined()

      // The sync should propagate { name: "Alice" } to userCopy
      expect(userCopyChange).toBeDefined()
      expect(userCopyChange!.value).toEqual({ name: 'Alice' })
    })

    it('should not contaminate change when child paths are synced', async () => {
      // Hypothesis: Case 3 (parent expansion) reads shadow state after update
      // but maybe reads from WRONG shadow? Old state leaking into sync?
      interface ChildSyncState {
        user: { name: string; age?: number }
        displayName: string
        userAge: number
      }

      const store = createGenericStore<ChildSyncState>({
        debug: { track: true },
      })
      const { storeInstance, setValue } = mountStore(
        store,
        {
          user: { name: 'Bob', age: 30 },
          displayName: 'Bob',
          userAge: 30,
        },
        {
          sideEffectsId: 'child-sync-test',
          sideEffects: {
            syncPaths: [
              ['user.name', 'displayName'],
              ['user.age', 'userAge'],
            ],
          },
        },
      )

      await flushEffects()
      storeInstance._debug!.clear()

      // Set user to object WITHOUT age — age should become undefined/null, not 30
      setValue('user', { name: 'Alice' })
      await flushEffects()

      const call = storeInstance._debug!.calls[0]
      const userChange = call.applied.find((c) => c.path === 'user')

      // The user change itself should be clean
      expect(userChange).toBeDefined()
      expect(userChange!.value).toEqual({ name: 'Alice' })
      expect(
        (userChange!.value as Record<string, unknown>)['age'],
      ).toBeUndefined()

      // displayName should now be "Alice" (synced from user.name)
      expect(storeInstance.state.displayName).toBe('Alice')
    })
  })

  describe('Nested object replacement', () => {
    it('should replace nested object at 2-level path without leaking sibling keys', async () => {
      // Customer said "1-level path" but test at 2 levels too
      const store = createGenericStore<NestedState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(store, {
        user: {
          profile: { name: 'Bob', age: 30, email: 'bob@example.com' },
          settings: { theme: 'dark', language: 'en' },
        },
        meta: { created: '2024-01-01' },
      })

      await flushEffects()
      storeInstance._debug!.clear()

      // Replace profile with a smaller object
      setValue('user.profile', { name: 'Alice' })
      await flushEffects()

      const call = storeInstance._debug!.calls[0]
      const profileChange = call.applied.find((c) => c.path === 'user.profile')

      expect(profileChange).toBeDefined()
      expect(profileChange!.value).toEqual({ name: 'Alice' })
      const v = profileChange!.value as Record<string, unknown>
      expect(v['age']).toBeUndefined()
      expect(v['email']).toBeUndefined()

      // Actual state should also be clean
      expect(storeInstance.state.user.profile).toEqual({ name: 'Alice' })
    })
  })

  describe('Shadow state consistency after replacement', () => {
    it('should not expose old keys after object replacement via shadow dump', async () => {
      // Verifies WASM shadow state is correctly updated
      const store = createGenericStore<UserState>({})
      const { storeInstance, setValue } = mountStore(store, {
        user: { name: 'Bob', age: 30 },
      })

      await flushEffects()

      // Verify shadow was initialized with age
      const shadowBefore = storeInstance._internal.pipeline!.shadowDump()
      expect((shadowBefore as Record<string, unknown>)['user']).toEqual({
        name: 'Bob',
        age: 30,
      })

      // Replace with object without age
      setValue('user', { name: 'Alice' })
      await flushEffects()

      // Shadow should now NOT contain age
      const shadowAfter = storeInstance._internal.pipeline!.shadowDump()
      const shadowUser = (shadowAfter as Record<string, unknown>)['user']
      expect(shadowUser).toEqual({ name: 'Alice' })
      expect((shadowUser as Record<string, unknown>)['age']).toBeUndefined()
    })

    it('should correctly report affected_paths after object replacement', async () => {
      // When we set user = { name: "Alice" }, shadow_dump should show clean state
      // and a second processChanges on a child path should not revive old keys
      const store = createGenericStore<UserState>({ debug: { track: true } })
      const { storeInstance, setValue } = mountStore(store, {
        user: { name: 'Bob', age: 30 },
      })

      await flushEffects()
      storeInstance._debug!.clear()

      // First: replace the user object entirely
      setValue('user', { name: 'Alice' })
      await flushEffects()

      // Second: change only name (should not revive age)
      storeInstance._debug!.clear()
      setValue('user.name', 'Charlie')
      await flushEffects()

      const call = storeInstance._debug!.calls[0]
      const nameChange = call.applied.find((c) => c.path === 'user.name')
      const userChange = call.applied.find((c) => c.path === 'user')

      // Only user.name should be in applied, not user (no phantom user change)
      expect(nameChange).toBeDefined()
      expect(nameChange!.value).toBe('Charlie')

      // No phantom 'user' change with old age
      if (userChange) {
        expect(
          (userChange.value as Record<string, unknown>)['age'],
        ).toBeUndefined()
      }

      // State should have name=Charlie, no age
      expect(storeInstance.state.user.name).toBe('Charlie')
      expect(storeInstance.state.user.age).toBeUndefined()
    })
  })
})
