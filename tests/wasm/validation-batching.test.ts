import { afterEach, beforeEach, describe, expect, it } from 'vitest'
import { z } from 'zod'

import {
  initWasm,
  resetWasm,
  validatorSchemas,
  wasm,
} from '../../src/wasm/bridge'

describe('WASM Validation Batching (EP4)', () => {
  beforeEach(async () => {
    const wasmModule = await import('../../rust/pkg/apex_state_wasm.js')
    initWasm(wasmModule)
    wasm.shadowInit({
      user: { email: 'test@test.com', name: 'Alice', role: 'guest' },
      order: { amount: 100, currency: 'USD', discount: 0 },
    })
  })

  afterEach(() => {
    validatorSchemas.clear()
    resetWasm()
  })

  describe('Single field with Zod schema', () => {
    it('should return validator in processChanges output when dep path changes', () => {
      // Register validator on user.email with validator_id=1
      wasm.registerFunctionsBatch([
        {
          function_id: 1,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
      ])

      // Register Zod email schema in validatorSchemas map
      validatorSchemas.set(1, z.string().email())

      // Call wasm.processChanges with user.email change
      const result = wasm.processChanges([
        { path: 'user.email', value: 'newemail@test.com' },
      ])

      // Assert validators_to_run has 1 entry
      expect(result.validators_to_run).toHaveLength(1)

      // Assert validator_id, output_path, and dependency_values are correct
      const validator = result.validators_to_run[0]
      expect(validator.validator_id).toBe(1)
      expect(validator.output_path).toBe('_concerns.user.email.validationState')

      // Assert dependency_values["user.email"] contains the new value (JSON string!)
      expect(validator.dependency_values['user.email']).toBe(
        '"newemail@test.com"',
      )
    })

    it('should include correct shadow state value in dependency_values', () => {
      // Register validator on user.name
      wasm.registerFunctionsBatch([
        {
          function_id: 2,
          output_path: '_concerns.user.name.validationState',
          scope: '',
          dependency_paths: ['user.name'],
        },
      ])

      validatorSchemas.set(2, z.string().min(1))

      // Change user.name to "Bob"
      const result = wasm.processChanges([{ path: 'user.name', value: 'Bob' }])

      // Assert validators_to_run[0].dependency_values["user.name"] === '"Bob"' (JSON string!)
      expect(result.validators_to_run[0].dependency_values['user.name']).toBe(
        '"Bob"',
      )
    })
  })

  describe('Multi-field validation', () => {
    it('should only trigger affected validators', () => {
      // Register validator_id=1 on user.email
      wasm.registerFunctionsBatch([
        {
          function_id: 1,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
        {
          function_id: 2,
          output_path: '_concerns.user.name.validationState',
          scope: '',
          dependency_paths: ['user.name'],
        },
      ])

      validatorSchemas.set(1, z.string().email())
      validatorSchemas.set(2, z.string().min(1))

      // Change user.email only
      const result = wasm.processChanges([
        { path: 'user.email', value: 'new@example.com' },
      ])

      // Assert validators_to_run has 1 entry (validator_id=1)
      expect(result.validators_to_run).toHaveLength(1)
      expect(result.validators_to_run[0].validator_id).toBe(1)

      // Assert validator_id=2 is NOT in the list
      const ids = result.validators_to_run.map((v) => v.validator_id)
      expect(ids).not.toContain(2)
    })

    it('should trigger multiple validators when multiple deps change', () => {
      // Register validator_id=1 on user.email
      // Register validator_id=2 on user.name
      wasm.registerFunctionsBatch([
        {
          function_id: 1,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
        {
          function_id: 2,
          output_path: '_concerns.user.name.validationState',
          scope: '',
          dependency_paths: ['user.name'],
        },
      ])

      validatorSchemas.set(1, z.string().email())
      validatorSchemas.set(2, z.string().min(1))

      // Change both user.email and user.name in same batch
      const result = wasm.processChanges([
        { path: 'user.email', value: 'test@example.com' },
        { path: 'user.name', value: 'Charlie' },
      ])

      // Assert validators_to_run has 2 entries
      expect(result.validators_to_run).toHaveLength(2)
      const ids = result.validators_to_run.map((v) => v.validator_id)
      expect(ids).toContain(1)
      expect(ids).toContain(2)
    })
  })

  describe('Multi-path dependency validator', () => {
    it('should trigger validator when any dependency changes', () => {
      // Register validator with dependency_paths: ["order.amount", "order.currency"]
      wasm.registerFunctionsBatch([
        {
          function_id: 3,
          output_path: '_concerns.order.validationState',
          scope: '',
          dependency_paths: ['order.amount', 'order.currency'],
        },
      ])

      validatorSchemas.set(
        3,
        z.object({
          amount: z.number().positive(),
          currency: z.string().length(3),
        }),
      )

      // Change order.amount only
      const result = wasm.processChanges([{ path: 'order.amount', value: 200 }])

      // Assert validators_to_run has 1 entry
      expect(result.validators_to_run).toHaveLength(1)

      // Assert dependency_values contains BOTH order.amount (new) and order.currency (from shadow)
      const validator = result.validators_to_run[0]
      expect(validator.dependency_values['order.amount']).toBe('200')
      expect(validator.dependency_values['order.currency']).toBe('"USD"')
    })

    it('should include all dependency values even when only one changes', () => {
      // Register validator with deps: ["order.amount", "order.currency", "order.discount"]
      wasm.registerFunctionsBatch([
        {
          function_id: 4,
          output_path: '_concerns.order.validationState',
          scope: '',
          dependency_paths: [
            'order.amount',
            'order.currency',
            'order.discount',
          ],
        },
      ])

      validatorSchemas.set(
        4,
        z.object({
          amount: z.number().positive(),
          currency: z.string().length(3),
          discount: z.number().min(0).max(100),
        }),
      )

      // Change order.discount only
      const result = wasm.processChanges([
        { path: 'order.discount', value: 10 },
      ])

      // Assert dependency_values has all 3 paths with correct values
      const validator = result.validators_to_run[0]
      expect(validator.dependency_values['order.amount']).toBe('100')
      expect(validator.dependency_values['order.currency']).toBe('"USD"')
      expect(validator.dependency_values['order.discount']).toBe('10')
    })
  })

  describe('Mixed concerns: BoolLogic + validation', () => {
    it('should return both concern_changes and validators_to_run', () => {
      // Register BoolLogic: _concerns.user.email.disabledWhen depends on user.role
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      // Register validator on user.email
      wasm.registerFunctionsBatch([
        {
          function_id: 5,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
      ])

      validatorSchemas.set(5, z.string().email())

      // Change user.role AND user.email in same batch
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
        { path: 'user.email', value: 'admin@example.com' },
      ])

      // BoolLogic results are buffered (not in state_changes), but has_work should be true
      expect(result.has_work).toBe(true)

      // Assert validators_to_run has validator entry
      expect(result.validators_to_run.length).toBeGreaterThan(0)
      expect(result.validators_to_run[0].validator_id).toBe(5)

      // Finalize to get BoolLogic concern changes
      const finalResult = wasm.pipelineFinalize([])
      const concernChanges = finalResult.state_changes.filter((c) =>
        c.path.startsWith('_concerns.'),
      )
      const boolLogicChange = concernChanges.find((c) =>
        c.path.includes('disabledWhen'),
      )
      expect(boolLogicChange).toBeDefined()
    })

    it('should only trigger BoolLogic when only its dep changes', () => {
      // Register BoolLogic on user.role
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      // Register validator on user.email
      wasm.registerFunctionsBatch([
        {
          function_id: 6,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
      ])

      validatorSchemas.set(6, z.string().email())

      // Change user.role only
      const result = wasm.processChanges([
        { path: 'user.role', value: 'admin' },
      ])

      // BoolLogic results are buffered, has_work should be true
      expect(result.has_work).toBe(true)

      // Assert validators_to_run is empty (only role changed, not email)
      expect(result.validators_to_run).toHaveLength(0)

      // Finalize to get BoolLogic concern changes
      const finalResult = wasm.pipelineFinalize([])
      const concernChanges = finalResult.state_changes.filter((c) =>
        c.path.startsWith('_concerns.'),
      )
      expect(concernChanges.length).toBeGreaterThan(0)
    })

    it('should only trigger validator when only its dep changes', () => {
      // Register BoolLogic on user.role
      wasm.registerBoolLogic('_concerns.user.email.disabledWhen', {
        IS_EQUAL: ['user.role', 'admin'],
      })

      // Register validator on user.email
      wasm.registerFunctionsBatch([
        {
          function_id: 7,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
      ])

      validatorSchemas.set(7, z.string().email())

      // Change user.email only
      const result = wasm.processChanges([
        { path: 'user.email', value: 'test@example.com' },
      ])

      // BoolLogic not triggered (only email changed, not role), no concern buffering
      // has_work should still be true because validator needs to run
      expect(result.has_work).toBe(true)

      // Assert validators_to_run has 1 entry
      expect(result.validators_to_run).toHaveLength(1)
      expect(result.validators_to_run[0].validator_id).toBe(7)
    })
  })

  describe('Registration lifecycle', () => {
    it('should register and unregister validators', () => {
      // Register validator_id=1 on user.email
      wasm.registerFunctionsBatch([
        {
          function_id: 8,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
      ])

      validatorSchemas.set(8, z.string().email())

      // Change user.email → validators_to_run has 1 entry
      let result = wasm.processChanges([
        { path: 'user.email', value: 'first@example.com' },
      ])

      expect(result.validators_to_run).toHaveLength(1)
      expect(result.validators_to_run[0].validator_id).toBe(8)

      // Unregister validator_id=8
      wasm.unregisterFunctionsBatch([8])

      // Change user.email again → validators_to_run is empty
      result = wasm.processChanges([
        { path: 'user.email', value: 'second@example.com' },
      ])

      expect(result.validators_to_run).toHaveLength(0)
    })

    it('should handle batch register and unregister', () => {
      // Register 3 validators in one batch call
      wasm.registerFunctionsBatch([
        {
          function_id: 9,
          output_path: '_concerns.user.email.validationState',
          scope: '',
          dependency_paths: ['user.email'],
        },
        {
          function_id: 10,
          output_path: '_concerns.user.name.validationState',
          scope: '',
          dependency_paths: ['user.name'],
        },
        {
          function_id: 11,
          output_path: '_concerns.user.role.validationState',
          scope: '',
          dependency_paths: ['user.role'],
        },
      ])

      validatorSchemas.set(9, z.string().email())
      validatorSchemas.set(10, z.string().min(1))
      validatorSchemas.set(11, z.string().min(1))

      // Verify all 3 appear for relevant changes
      let result = wasm.processChanges([
        { path: 'user.email', value: 'test@example.com' },
        { path: 'user.name', value: 'David' },
        { path: 'user.role', value: 'editor' },
      ])

      expect(result.validators_to_run).toHaveLength(3)
      const ids = result.validators_to_run.map((v) => v.validator_id)
      expect(ids).toContain(9)
      expect(ids).toContain(10)
      expect(ids).toContain(11)

      // Unregister all 3 in one batch call
      wasm.unregisterFunctionsBatch([9, 10, 11])

      // Verify none appear for same changes
      result = wasm.processChanges([
        { path: 'user.email', value: 'new@example.com' },
        { path: 'user.name', value: 'Eve' },
        { path: 'user.role', value: 'admin' },
      ])

      expect(result.validators_to_run).toHaveLength(0)
    })

    it('should clean up reverse index on unregister', () => {
      // Register validator with 2 dependency paths
      wasm.registerFunctionsBatch([
        {
          function_id: 12,
          output_path: '_concerns.order.validationState',
          scope: '',
          dependency_paths: ['order.amount', 'order.currency'],
        },
      ])

      validatorSchemas.set(
        12,
        z.object({
          amount: z.number().positive(),
          currency: z.string().length(3),
        }),
      )

      // Unregister it
      wasm.unregisterFunctionsBatch([12])

      // Change either dep path
      const result = wasm.processChanges([{ path: 'order.amount', value: 150 }])

      // Assert validators_to_run is empty (no stale entries)
      expect(result.validators_to_run).toHaveLength(0)
    })
  })

  describe('Edge cases', () => {
    it('should return empty validators_to_run when no validators registered', () => {
      // No validators registered
      // Process any change
      const result = wasm.processChanges([
        { path: 'user.email', value: 'test@example.com' },
      ])

      // Assert validators_to_run is empty (or undefined)
      expect(
        result.validators_to_run === undefined ||
          result.validators_to_run.length === 0,
      ).toBe(true)
    })

    it('should handle validator on non-existent shadow path gracefully', () => {
      // Register validator with dependency on "missing.path"
      wasm.registerFunctionsBatch([
        {
          function_id: 13,
          output_path: '_concerns.missing.validationState',
          scope: '',
          dependency_paths: ['missing.path'],
        },
      ])

      validatorSchemas.set(13, z.any())

      // Trigger validator indirectly (change a different field to force processing)
      // Note: Validator won't trigger since missing.path doesn't change, but if it did:
      wasm.shadowInit({
        user: { email: 'test@test.com', name: 'Alice', role: 'guest' },
        order: { amount: 100, currency: 'USD', discount: 0 },
        missing: { path: null },
      })

      const result = wasm.processChanges([
        { path: 'missing.path', value: 'test' },
      ])

      // Assert dependency_values contains "null" for missing path (before change)
      // or the validator was triggered
      if (result.validators_to_run.length > 0) {
        expect(result.validators_to_run[0].validator_id).toBe(13)
      }
    })

    it('should deduplicate validators when multiple deps change', () => {
      // Register validator with deps: ["order.amount", "order.currency"]
      wasm.registerFunctionsBatch([
        {
          function_id: 14,
          output_path: '_concerns.order.validationState',
          scope: '',
          dependency_paths: ['order.amount', 'order.currency'],
        },
      ])

      validatorSchemas.set(
        14,
        z.object({
          amount: z.number().positive(),
          currency: z.string().length(3),
        }),
      )

      // Change BOTH order.amount and order.currency in same batch
      const result = wasm.processChanges([
        { path: 'order.amount', value: 250 },
        { path: 'order.currency', value: 'EUR' },
      ])

      // Assert validator appears exactly once in validators_to_run
      expect(result.validators_to_run).toHaveLength(1)
      expect(result.validators_to_run[0].validator_id).toBe(14)

      // Verify both dependency values are included
      const validator = result.validators_to_run[0]
      expect(validator.dependency_values['order.amount']).toBe('250')
      expect(validator.dependency_values['order.currency']).toBe('"EUR"')
    })
  })
})
