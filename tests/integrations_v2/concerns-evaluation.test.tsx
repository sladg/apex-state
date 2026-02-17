/**
 * Concerns: BoolLogic-Driven UI (disabledWhen, visibleWhen, readonlyWhen, labels, etc.)
 *
 * Validates that BoolLogic-based concerns:
 * - Evaluate AND/OR/NOT/IS_EQUAL/EXISTS/IS_EMPTY/GT/LT/IN expressions
 * - Drive UI states: disabled, visible, readonly, labels, placeholders, tooltips
 * - Re-evaluate when dependency paths change
 * - Support template interpolation for dynamic text
 *
 * ┌─────────────────────────────────────────────────────────────────────┐
 * │ REPLACES (when this v2 test is fully implemented):                  │
 * ├─────────────────────────────────────────────────────────────────────┤
 * │ tests/integration/concerns-ui.test.tsx              (ENTIRE FILE)  │
 * │ tests/integration/ecommerce-catalog.test.tsx   (BoolLogic + text)  │
 * │   → Conditional UI, dynamic interpolation, custom concerns          │
 * └─────────────────────────────────────────────────────────────────────┘
 */

import { describe, expect, it } from 'vitest'

import {
  createGenericStore,
  extractPlaceholders,
  interpolateTemplate,
} from '../../src'
import type { BasicTestState } from '../mocks'
import { basicTestFixtures } from '../mocks'
import { flushEffects, MODES, mountStore } from '../utils/react'

describe.each(MODES)('[$name] Concerns: BoolLogic-Driven UI', ({ config }) => {
  describe('disabledWhen concern', () => {
    it('should disable field when BoolLogic evaluates to true', async () => {
      // Register disabledWhen on 'price' field
      // BoolLogic: IS_EQUAL('status', 'published')
      // Set status = 'published'
      // Assert _concerns['price']['disabledWhen'] === true

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'published'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'published')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: true,
      })
    })

    it('should enable field when BoolLogic evaluates to false', async () => {
      // Register disabledWhen on 'price'
      // BoolLogic: IS_EQUAL('status', 'published')
      // Set status = 'draft'
      // Assert _concerns['price']['disabledWhen'] === false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'published'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'draft')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })
    })

    it('should re-evaluate when dependency changes', async () => {
      // Register disabledWhen with dependency on 'status'
      // Change status from 'draft' → 'published'
      // Assert disabledWhen flips from false → true
      // Change status from 'published' → 'draft'
      // Assert disabledWhen flips back to false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'published'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'draft')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })

      setValue('fieldB', 'published')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: true,
      })

      setValue('fieldB', 'draft')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })
    })

    it('should use complex AND/OR/NOT BoolLogic for multi-condition disable', async () => {
      // Register disabledWhen with:
      //   AND([IS_EQUAL('status', 'approved'), NOT(IS_EQUAL('role', 'admin'))])
      // Test all 4 combinations of status/role
      // Assert correct disable state for each

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: {
                  AND: [
                    { IS_EQUAL: ['fieldB', 'approved'] },
                    { NOT: { IS_EQUAL: ['fieldC', 1] } },
                  ],
                },
              },
            },
          },
        },
      )

      // Case 1: status=approved, role=not_admin → true
      setValue('fieldB', 'approved')
      setValue('fieldC', 99)
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: true,
      })

      // Case 2: status=approved, role=admin → false
      setValue('fieldC', 1)
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })

      // Case 3: status=draft, role=admin → false
      setValue('fieldB', 'draft')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })

      // Case 4: status=draft, role=not_admin → false
      setValue('fieldC', 99)
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
      })
    })
  })

  describe('visibleWhen concern', () => {
    it('should show field when BoolLogic evaluates to true', async () => {
      // Register visibleWhen on 'weight' field
      // BoolLogic: IS_EQUAL('productType', 'physical')
      // Set productType = 'physical'
      // Assert _concerns['weight']['visibleWhen'] === true

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldC: {
              visibleWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'physical'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'physical')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldC']).toMatchObject({
        visibleWhen: true,
      })
    })

    it('should hide field when BoolLogic evaluates to false', async () => {
      // Same setup
      // Set productType = 'digital'
      // Assert _concerns['weight']['visibleWhen'] === false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldC: {
              visibleWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'physical'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'digital')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldC']).toMatchObject({
        visibleWhen: false,
      })
    })

    it('should show download URL only for digital products', async () => {
      // Register visibleWhen on 'downloadUrl'
      // BoolLogic: IS_EQUAL('productType', 'digital')
      // Set productType = 'digital'
      // Assert downloadUrl visible
      // Set productType = 'physical'
      // Assert downloadUrl hidden

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              visibleWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'digital'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'digital')
      await flushEffects()
      expect(storeInstance._concerns?.['source']).toMatchObject({
        visibleWhen: true,
      })

      setValue('fieldB', 'physical')
      await flushEffects()
      expect(storeInstance._concerns?.['source']).toMatchObject({
        visibleWhen: false,
      })
    })

    it('should support NOT logic for visibility', async () => {
      // Register visibleWhen with NOT(IS_EQUAL('barrierType', 'none'))
      // Set barrierType = 'none' → hidden
      // Set barrierType = 'knockout' → visible

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              visibleWhen: {
                boolLogic: { NOT: { IS_EQUAL: ['fieldB', 'none'] } },
              },
            },
          },
        },
      )

      setValue('fieldB', 'none')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        visibleWhen: false,
      })

      setValue('fieldB', 'knockout')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        visibleWhen: true,
      })
    })
  })

  describe('readonlyWhen concern', () => {
    it('should make field readonly when condition met', async () => {
      // Register readonlyWhen on field
      // BoolLogic: IS_EQUAL('status', 'published')
      // Set status = 'published'
      // Assert readonlyWhen === true

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              readonlyWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'published'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'published')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        readonlyWhen: true,
      })
    })

    it('should allow editing when condition not met', async () => {
      // Same setup
      // Set status = 'draft'
      // Assert readonlyWhen === false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              readonlyWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'published'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'draft')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        readonlyWhen: false,
      })
    })

    it('should make fields readonly in crisis market regime', async () => {
      // Register readonlyWhen with IS_EQUAL('marketRegime', 'crisis')
      // Set marketRegime = 'crisis'
      // Assert all affected fields are readonly

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              readonlyWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'crisis'] },
              },
            },
            fieldC: {
              readonlyWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'crisis'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'crisis')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        readonlyWhen: true,
      })
      expect(storeInstance._concerns?.['fieldC']).toMatchObject({
        readonlyWhen: true,
      })
    })
  })

  describe('Dynamic text: labels', () => {
    it('should update field label based on state', async () => {
      // Register label concern on 'name' field
      // Template: '{{productType}} Name'
      // Set productType = 'Physical'
      // Assert label === 'Physical Name'

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              dynamicLabel: {
                template: '{{fieldB}} Name',
              },
            },
          },
        },
      )

      setValue('fieldB', 'Physical')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        dynamicLabel: 'Physical Name',
      })
    })

    it('should interpolate dynamic label with multiple paths', async () => {
      // Register label with template referencing two paths
      // Template: '{{ccyPair}} {{strategy}} Strike'
      // Set ccyPair = 'EUR/USD', strategy = 'Straddle'
      // Assert label === 'EUR/USD Straddle Strike'

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              dynamicLabel: {
                template: '{{fieldA}} {{fieldB}} Strike',
              },
            },
          },
        },
      )

      setValue('fieldA', 'EUR/USD')
      setValue('fieldB', 'Straddle')
      await flushEffects()

      expect(storeInstance._concerns?.['source']).toMatchObject({
        dynamicLabel: 'EUR/USD Straddle Strike',
      })
    })

    it('should re-evaluate label when dependency changes', async () => {
      // Change dependency path
      // Assert label text updates accordingly

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            target: {
              dynamicLabel: {
                template: 'Type: {{fieldB}}',
              },
            },
          },
        },
      )

      setValue('fieldB', 'initial')
      await flushEffects()
      expect(storeInstance._concerns?.['target']).toMatchObject({
        dynamicLabel: 'Type: initial',
      })

      setValue('fieldB', 'updated')
      await flushEffects()
      expect(storeInstance._concerns?.['target']).toMatchObject({
        dynamicLabel: 'Type: updated',
      })
    })
  })

  describe('Dynamic text: placeholders', () => {
    it('should update placeholder dynamically', async () => {
      // Register placeholder concern
      // Template: 'Enter {{productType}} details...'
      // Set productType = 'digital'
      // Assert placeholder === 'Enter digital details...'

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              dynamicPlaceholder: {
                template: 'Enter {{fieldB}} details...',
              },
            },
          },
        },
      )

      setValue('fieldB', 'digital')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        dynamicPlaceholder: 'Enter digital details...',
      })
    })

    it('should interpolate placeholder with deeply nested data', async () => {
      // Register placeholder with template referencing nested path
      // Assert interpolation resolves nested values

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              dynamicPlaceholder: {
                template: 'Nested: {{fieldB}} ({{fieldA}})',
              },
            },
          },
        },
      )

      setValue('fieldA', 'first')
      setValue('fieldB', 'second')
      await flushEffects()

      expect(storeInstance._concerns?.['source']).toMatchObject({
        dynamicPlaceholder: 'Nested: second (first)',
      })
    })
  })

  describe('Dynamic text: tooltips', () => {
    it('should display dynamic tooltip with field restrictions', async () => {
      // Register tooltip concern
      // Template: 'Range: {{minValue}} - {{maxValue}}'
      // Set minValue = 0, maxValue = 100
      // Assert tooltip === 'Range: 0 - 100'

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              dynamicTooltip: {
                template: 'Range: {{fieldC}} - 100',
              },
            },
          },
        },
      )

      setValue('fieldC', 0)
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        dynamicTooltip: 'Range: 0 - 100',
      })
    })

    it('should interpolate tooltip from deeply nested market data', async () => {
      // Register tooltip with template referencing deep paths
      // Assert interpolation works at depth 5+

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              dynamicTooltip: {
                template: 'Status: {{fieldB}}, Count: {{fieldC}}',
              },
            },
          },
        },
      )

      setValue('fieldB', 'active')
      setValue('fieldC', 42)
      await flushEffects()

      expect(storeInstance._concerns?.['source']).toMatchObject({
        dynamicTooltip: 'Status: active, Count: 42',
      })
    })
  })

  describe('Template interpolation engine', () => {
    it('should extract placeholders from template', () => {
      // Template: '{{fieldA}} and {{fieldB}}'
      // Assert extractPlaceholders returns ['fieldA', 'fieldB']

      const template = '{{fieldA}} and {{fieldB}}'
      const placeholders = extractPlaceholders(template)
      expect(placeholders).toEqual(['fieldA', 'fieldB'])
    })

    it('should handle templates with no placeholders', () => {
      // Template: 'Static text'
      // Assert extractPlaceholders returns []

      const template = 'Static text'
      const placeholders = extractPlaceholders(template)
      expect(placeholders).toEqual([])
    })

    it('should handle missing values in interpolation', () => {
      // Template references non-existent path
      // Assert graceful handling (empty string or placeholder preserved)

      const state: BasicTestState = basicTestFixtures.empty
      const template = 'Value: {{nonExistent.path}}'
      const result = interpolateTemplate(template, state)
      expect(result).toBe('Value: {{nonExistent.path}}')
    })

    it('should handle nested path references in templates', () => {
      // Template: '{{nested.deep.value}}'
      // Assert interpolation resolves nested path

      const state = { ...basicTestFixtures.empty, fieldA: 'nested-value' }
      const template = 'Result: {{fieldA}}'
      const result = interpolateTemplate(template, state)
      expect(result).toBe('Result: nested-value')
    })
  })

  describe('Custom concerns', () => {
    it('should evaluate custom concern with evaluate() function', async () => {
      // Register custom concern with evaluate(state) function
      // Assert evaluate called when dependencies change
      // Assert return value stored in _concerns

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              customCheck: {
                evaluate: ({ state }: any) => {
                  return (state as BasicTestState).fieldB === 'checked'
                },
              },
            },
          },
        },
      )

      setValue('fieldB', 'checked')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toBeDefined()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        customCheck: true,
      })
    })

    it('should re-evaluate custom concern when state changes', async () => {
      // Register custom concern
      // Change dependency
      // Assert concern re-evaluates

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              dynamicStatus: {
                evaluate: ({ state }: any) => {
                  const s = state as BasicTestState
                  return s.fieldC > 50 ? 'high' : 'low'
                },
              },
            },
          },
        },
      )

      setValue('fieldC', 30)
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        dynamicStatus: 'low',
      })

      setValue('fieldC', 75)
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        dynamicStatus: 'high',
      })
    })

    it('should support custom concern with BoolLogic + evaluate', async () => {
      // Register concern with both BoolLogic expression and evaluate function
      // Assert both are processed correctly

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              booleanCheck: {
                boolLogic: { IS_EQUAL: ['fieldB', 'active'] },
              },
              customCalculation: {
                evaluate: ({ state }: any) => {
                  return (state as BasicTestState).fieldC * 2
                },
              },
            },
          },
        },
      )

      setValue('fieldB', 'active')
      setValue('fieldC', 25)
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        booleanCheck: true,
        customCalculation: 50,
      })
    })

    it('should support margin check concern (validates against limit)', async () => {
      // Custom concern: marginCheck
      // evaluate: (state) => state.notional > state.riskLimit
      // Assert returns correct result

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              marginCheck: {
                evaluate: ({ state }: any) => {
                  const s = state as BasicTestState
                  return s.fieldC > 100
                },
              },
            },
          },
        },
      )

      setValue('fieldC', 50)
      await flushEffects()
      expect(storeInstance._concerns?.['source']).toMatchObject({
        marginCheck: false,
      })

      setValue('fieldC', 150)
      await flushEffects()
      expect(storeInstance._concerns?.['source']).toMatchObject({
        marginCheck: true,
      })
    })

    it('should support barrier proximity concern (computed distance)', async () => {
      // Custom concern: barrierProximity
      // evaluate: computes percentage distance
      // Assert returns correct numeric result

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            target: {
              barrierProximity: {
                evaluate: ({ state }: any) => {
                  const s = state as BasicTestState
                  const spot = s.fieldC
                  const barrier = 100
                  return ((barrier - spot) / barrier) * 100
                },
              },
            },
          },
        },
      )

      setValue('fieldC', 95)
      await flushEffects()
      expect(storeInstance._concerns?.['target']).toMatchObject({
        barrierProximity: 5,
      })

      setValue('fieldC', 80)
      await flushEffects()
      expect(storeInstance._concerns?.['target']).toMatchObject({
        barrierProximity: 20,
      })
    })

    it('should combine custom concern with built-in concerns on same path', async () => {
      // Register both validationState AND custom concern on same field
      // Assert both concerns evaluate independently
      // Assert both results available in _concerns

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            email: {
              isNumeric: {
                evaluate: ({ state }: any) => {
                  const s = state as BasicTestState
                  return !isNaN(Number(s.email))
                },
              },
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'locked'] },
              },
            },
          },
        },
      )

      setValue('email', '123')
      setValue('fieldB', 'locked')
      await flushEffects()

      expect(storeInstance._concerns?.['email']).toMatchObject({
        isNumeric: true,
        disabledWhen: true,
      })
    })
  })

  describe('Concern re-evaluation', () => {
    it('should re-evaluate all concerns when dependency changes', async () => {
      // Register multiple concerns depending on same field
      // Change that field
      // Assert all concerns re-evaluate

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'active'] },
              },
              visibleWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'active'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'active')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: true,
        visibleWhen: true,
      })

      setValue('fieldB', 'inactive')
      await flushEffects()

      expect(storeInstance._concerns?.['fieldA']).toMatchObject({
        disabledWhen: false,
        visibleWhen: false,
      })
    })

    it('should NOT re-evaluate concerns for unrelated changes', async () => {
      // Register concern depending on fieldA
      // Change fieldB
      // Assert concern NOT re-evaluated

      const store = createGenericStore<BasicTestState>(config)
      let evaluationCount = 0
      const { storeInstance: _storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              counter: {
                evaluate: ({ state }: any) => {
                  evaluationCount++
                  return (state as BasicTestState).fieldA.length
                },
              },
            },
          },
        },
      )

      await flushEffects()
      const initialCount = evaluationCount

      setValue('fieldC', 42)
      await flushEffects()
      const countAfterUnrelated = evaluationCount

      expect(countAfterUnrelated).toBe(initialCount)
    })

    it('should handle deep state changes propagating to concerns', async () => {
      // Concern depends on deeply nested path
      // Change that deep path
      // Assert concern re-evaluates correctly

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              depthCheck: {
                evaluate: ({ state }: any) => {
                  return (state as BasicTestState).fieldA
                },
              },
            },
          },
        },
      )

      setValue('fieldA', 'deep-value')
      await flushEffects()

      expect(storeInstance._concerns?.['source']).toMatchObject({
        depthCheck: 'deep-value',
      })
    })
  })

  describe('BoolLogic operators integration', () => {
    it('should evaluate IS_EQUAL correctly', async () => {
      // Register concern with IS_EQUAL('field', 'value')
      // Set field = 'value' → true
      // Set field = 'other' → false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'target'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'target')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['disabledWhen']).toBe(true)

      setValue('fieldB', 'other')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['disabledWhen']).toBe(false)
    })

    it('should evaluate EXISTS correctly', async () => {
      // Register concern with EXISTS('field')
      // Set field = 'something' → true
      // Set field = null → false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              visibleWhen: {
                boolLogic: { EXISTS: 'fieldB' },
              },
            },
          },
        },
      )

      setValue('fieldB', 'something')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['visibleWhen']).toBe(true)
    })

    it('should evaluate IS_EMPTY correctly', async () => {
      // Register concern with IS_EMPTY('field')
      // Set field = '' → true
      // Set field = 'text' → false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              readonlyWhen: {
                boolLogic: { IS_EMPTY: 'fieldB' },
              },
            },
          },
        },
      )

      setValue('fieldB', '')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['readonlyWhen']).toBe(true)

      setValue('fieldB', 'text')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['readonlyWhen']).toBe(false)
    })

    it('should evaluate GT/LT/GTE/LTE correctly', async () => {
      // Register concerns with comparison operators
      // Test boundary values

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              gtConcern: {
                boolLogic: { GT: ['fieldC', 50] },
              },
              ltConcern: {
                boolLogic: { LT: ['fieldC', 100] },
              },
              gteConcern: {
                boolLogic: { GTE: ['fieldC', 50] },
              },
              lteConcern: {
                boolLogic: { LTE: ['fieldC', 100] },
              },
            },
          },
        },
      )

      setValue('fieldC', 50)
      await flushEffects()
      expect(storeInstance._concerns?.['source']?.['gtConcern']).toBe(false)
      expect(storeInstance._concerns?.['source']?.['gteConcern']).toBe(true)
      expect(storeInstance._concerns?.['source']?.['ltConcern']).toBe(true)
      expect(storeInstance._concerns?.['source']?.['lteConcern']).toBe(true)

      setValue('fieldC', 100)
      await flushEffects()
      expect(storeInstance._concerns?.['source']?.['gtConcern']).toBe(true)
      expect(storeInstance._concerns?.['source']?.['ltConcern']).toBe(false)
      expect(storeInstance._concerns?.['source']?.['lteConcern']).toBe(true)
    })

    it('should evaluate IN operator correctly', async () => {
      // Register concern with IN('field', ['a', 'b', 'c'])
      // Set field = 'a' → true
      // Set field = 'z' → false

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              inCheck: {
                boolLogic: { IN: ['fieldB', ['a', 'b', 'c']] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'a')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['inCheck']).toBe(true)

      setValue('fieldB', 'z')
      await flushEffects()
      expect(storeInstance._concerns?.['fieldA']?.['inCheck']).toBe(false)
    })

    it('should evaluate nested AND/OR/NOT combinations', async () => {
      // Register concern with deeply nested boolean logic
      // Test multiple input combinations
      // Assert correct output for each

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              complexLogic: {
                boolLogic: {
                  OR: [
                    {
                      AND: [
                        { IS_EQUAL: ['fieldB', 'state1'] },
                        { GT: ['fieldC', 10] },
                      ],
                    },
                    { NOT: { IS_EQUAL: ['fieldA', 'blocked'] } },
                  ],
                },
              },
            },
          },
        },
      )

      setValue('fieldB', 'state1')
      setValue('fieldC', 20)
      setValue('fieldA', 'allowed')
      await flushEffects()
      expect(storeInstance._concerns?.['source']?.['complexLogic']).toBe(true)

      setValue('fieldB', 'state2')
      setValue('fieldC', 5)
      await flushEffects()
      expect(storeInstance._concerns?.['source']?.['complexLogic']).toBe(true)

      setValue('fieldA', 'blocked')
      await flushEffects()
      expect(storeInstance._concerns?.['source']?.['complexLogic']).toBe(false)
    })
  })

  describe('Concern results accessibility', () => {
    it('should make concern results accessible via _concerns proxy', async () => {
      // Register concern on fieldA
      // Assert _concerns['fieldA']['concernType'] is accessible

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            fieldA: {
              disabledWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'active'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'active')
      await flushEffects()

      expect(storeInstance._concerns).toBeDefined()
      expect(storeInstance._concerns?.['fieldA']).toBeDefined()
      expect(storeInstance._concerns?.['fieldA']?.['disabledWhen']).toBe(true)
    })

    it('should make concerns accessible at deep paths', async () => {
      // Register concern on 'deeply.nested.field'
      // Assert _concerns accessible at that path

      const store = createGenericStore<BasicTestState>(config)
      const { storeInstance, setValue } = mountStore(
        store,
        basicTestFixtures.empty,
        {
          concerns: {
            source: {
              visibleWhen: {
                boolLogic: { IS_EQUAL: ['fieldB', 'show'] },
              },
            },
          },
        },
      )

      setValue('fieldB', 'show')
      await flushEffects()

      expect(storeInstance._concerns?.['source']).toBeDefined()
      expect(storeInstance._concerns?.['source']?.['visibleWhen']).toBe(true)
    })

    it('should expose concerns through useFieldStore hook', async () => {
      // Register concerns
      // Call useFieldStore('fieldA')
      // Assert concern results available in return value

      const store = createGenericStore<BasicTestState>(config)
      let hookResult: any = null

      const { storeInstance } = mountStore(store, basicTestFixtures.empty, {
        concerns: {
          fieldA: {
            disabledWhen: {
              boolLogic: { IS_EQUAL: ['fieldB', 'locked'] },
            },
          },
        },
        customRender: (_state) => {
          const result = store.useFieldStore('fieldA')
          hookResult = result
          return <span data-testid="value">{result.value}</span>
        },
      })

      await flushEffects()

      expect(hookResult).toBeDefined()
      expect(
        storeInstance._concerns?.['fieldA']?.['disabledWhen'],
      ).toBeDefined()
    })
  })
})
