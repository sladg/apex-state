/**
 * Metadata Preservation — Full pipeline test
 *
 * Verifies that metadata (sender, stage, lineage flags) flows through the entire
 * pipeline without being lost, especially for concern changes.
 * Regression test for: WASM pipeline_finalize losing meta when partitioning concerns.
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const makePipeline = (): WasmPipeline => {
  return createWasmPipeline()
}

// ---------------------------------------------------------------------------
// Test suite
// ---------------------------------------------------------------------------

describe('Metadata Preservation in Pipeline', () => {
  let pipeline: WasmPipeline

  afterEach(() => {
    if (pipeline) {
      pipeline.destroy()
    }
  })

  describe('Listener output metadata preservation', () => {
    it('should preserve sender metadata for listener-produced changes', () => {
      // Create a pipeline and verify that metadata sent to pipelineFinalize
      // (which includes sender set by invokeHandler) is preserved through finalization
      pipeline = makePipeline()
      pipeline.shadowInit({ user: { name: 'Alice' } })

      // Simulate changes coming from listener with sender metadata
      // Note: pipelineFinalize expects Wasm.Change[] with value_json
      const listenerProducedChanges: any[] = [
        {
          path: 'user.name',
          value_json: JSON.stringify('Bob'),
          meta: { sender: 'onNameChange' },
        },
      ]

      // Run pipelineFinalize which was the problematic function
      const result = pipeline.pipelineFinalize(listenerProducedChanges)

      // The metadata should be preserved in the returned state_changes
      expect(result.state_changes.length).toBeGreaterThan(0)
      const change = result.state_changes.find((c) => c.path === 'user.name')
      expect(change).toBeDefined()
      expect(change?.meta?.sender).toBe('onNameChange')
    })
  })

  describe('Concern changes metadata preservation', () => {
    it('should not lose metadata when partitioning concern changes in pipelineFinalize', () => {
      pipeline = makePipeline()
      pipeline.shadowInit({
        user: { role: 'admin' },
        _concerns: { roleValid: true },
      })

      // Simulate concern changes with metadata that should be preserved
      // This tests the fix where metadata was being lost via ..Default::default()
      const changesWithConcerns: any[] = [
        {
          path: '_concerns.roleValid',
          value_json: JSON.stringify(true),
          meta: {
            stage: 'listeners',
            sender: 'roleValidator',
            isListenerChange: true,
          },
        },
      ]

      const result = pipeline.pipelineFinalize(changesWithConcerns)

      // Find the concern change in the result
      const concernChange = result.state_changes.find(
        (c) => c.path === '_concerns.roleValid',
      )

      // All metadata should be preserved, not lost
      expect(concernChange).toBeDefined()
      expect(concernChange?.meta?.stage).toBe('listeners')
      expect(concernChange?.meta?.sender).toBe('roleValidator')
      expect(concernChange?.meta?.isListenerChange).toBe(true)
    })

    it('should preserve metadata through multiple concern path manipulations', () => {
      pipeline = makePipeline()
      pipeline.shadowInit({
        validation: { email: true },
        _concerns: { emailCheck: true },
      })

      // Multiple concern changes with various metadata
      const concernChanges: any[] = [
        {
          path: '_concerns.emailCheck',
          value_json: JSON.stringify(true),
          meta: {
            stage: 'listeners',
            sender: 'emailValidator',
          },
        },
        {
          path: 'validation.email',
          value_json: JSON.stringify(false),
          meta: {
            stage: 'sync',
            isSyncPathChange: true,
          },
        },
      ]

      const result = pipeline.pipelineFinalize(concernChanges)

      // Both changes should preserve their metadata
      const concern = result.state_changes.find(
        (c) => c.path === '_concerns.emailCheck',
      )
      expect(concern?.meta?.sender).toBe('emailValidator')

      const stateChange = result.state_changes.find(
        (c) => c.path === 'validation.email',
      )
      expect(stateChange?.meta?.isSyncPathChange).toBe(true)
    })
  })

  describe('Metadata flow through full pipeline', () => {
    it('should preserve metadata from listener output through pipelineFinalize', () => {
      pipeline = makePipeline()
      pipeline.shadowInit({ user: { name: 'Alice', email: '' } })

      // Simulate listener-produced changes with metadata
      const changesFromListener: any[] = [
        {
          path: 'user.email',
          value_json: JSON.stringify('new@example.com'),
          meta: {
            sender: 'onNameChange',
            isListenerChange: true,
          },
        },
      ]

      // These go through pipelineFinalize (which was losing metadata)
      const result = pipeline.pipelineFinalize(changesFromListener)

      // All metadata should be preserved
      const change = result.state_changes.find((c) => c.path === 'user.email')
      expect(change).toBeDefined()
      expect(change?.meta?.sender).toBe('onNameChange')
      expect(change?.meta?.isListenerChange).toBe(true)
    })
  })
})
