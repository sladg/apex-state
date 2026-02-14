/**
 * Tests for timing utilities
 *
 * Verifies debug timing measurement for slow operation detection.
 */

import { describe, expect, it, vi } from 'vitest'

import { createTiming, type TimingEvent } from '~/_internal/utils/timing'

describe('createTiming', () => {
  describe('when timing is enabled', () => {
    const config = {
      timing: true,
      timingThreshold: 5,
    }

    it('should return the function result', () => {
      const timing = createTiming(config)
      const result = timing.run('concerns', () => 42, {
        path: 'test',
        name: 'test',
      })
      expect(result).toBe(42)
    })

    it('should call onSlowOperation when operation exceeds threshold', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation })

      timing.run(
        'concerns',
        () => {
          const start = Date.now()
          while (Date.now() - start < 10) {
            // busy wait
          }
          return 'done'
        },
        { path: 'user.email', name: 'validationState' },
      )

      expect(onSlowOperation).toHaveBeenCalledTimes(1)
      expect(onSlowOperation).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'concerns',
          path: 'user.email',
          name: 'validationState',
          threshold: 5,
        }),
      )

      const event = onSlowOperation.mock.calls[0]![0] as TimingEvent
      expect(event.duration).toBeGreaterThan(5)
    })

    it('should deduplicate warnings for same operation', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation })

      const slowFn = () => {
        const start = Date.now()
        while (Date.now() - start < 10) {
          // busy wait
        }
        return 'done'
      }

      // Call same operation multiple times
      for (let i = 0; i < 5; i++) {
        timing.run('concerns', slowFn, { path: 'test', name: 'test' })
      }

      // Should only warn once
      expect(onSlowOperation).toHaveBeenCalledTimes(1)
    })

    it('should warn separately for different operations', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation })

      const slowFn = () => {
        const start = Date.now()
        while (Date.now() - start < 10) {
          // busy wait
        }
        return 'done'
      }

      timing.run('concerns', slowFn, { path: 'path1', name: 'concern1' })
      timing.run('concerns', slowFn, { path: 'path2', name: 'concern2' })

      expect(onSlowOperation).toHaveBeenCalledTimes(2)
    })

    it('should not call onSlowOperation for fast operations', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({
        timing: true,
        timingThreshold: 100,
        onSlowOperation,
      })

      timing.run('concerns', () => 'fast', { path: 'test', name: 'test' })

      expect(onSlowOperation).not.toHaveBeenCalled()
    })

    it('should work with listeners type', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation })

      timing.run(
        'listeners',
        () => {
          const start = Date.now()
          while (Date.now() - start < 10) {
            // busy wait
          }
          return 'done'
        },
        { path: 'user', name: 'fn' },
      )

      expect(onSlowOperation).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'listeners',
          path: 'user',
          name: 'fn',
        }),
      )
    })

    it('should report batch summary via onSummary', () => {
      const onSummary = vi.fn()
      const timing = createTiming({ ...config, onSummary })

      // Run some operations
      timing.run('listeners', () => 1, { path: 'a', name: 'x' })
      timing.run('listeners', () => 2, { path: 'b', name: 'y' })

      timing.reportBatch('listeners')

      expect(onSummary).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'listeners',
          operationCount: 2,
        }),
      )
    })

    it('should track slow operations in summary', () => {
      const onSummary = vi.fn()
      const timing = createTiming({ ...config, onSummary })

      timing.run(
        'concerns',
        () => {
          const start = Date.now()
          while (Date.now() - start < 10) {
            // busy wait
          }
        },
        { path: 'slow', name: 'op' },
      )

      timing.reportBatch('concerns')

      const summary = onSummary.mock.calls[0]![0]
      expect(summary.slowOperations).toHaveLength(1)
      expect(summary.slowOperations[0]).toMatchObject({
        path: 'slow',
        name: 'op',
      })
    })

    it('should reset batch counters after report but keep warned set', () => {
      const onSlowOperation = vi.fn()
      const onSummary = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation, onSummary })

      const slowFn = () => {
        const start = Date.now()
        while (Date.now() - start < 10) {
          // busy wait
        }
        return 'done'
      }

      // First batch
      timing.run('listeners', slowFn, { path: 'test', name: 'test' })
      timing.reportBatch('listeners')

      expect(onSummary).toHaveBeenCalledWith(
        expect.objectContaining({ operationCount: 1 }),
      )

      // Second batch - same operation
      timing.run('listeners', slowFn, { path: 'test', name: 'test' })
      timing.reportBatch('listeners')

      // Should NOT warn again (dedup persists across batches)
      expect(onSlowOperation).toHaveBeenCalledTimes(1)
      // But counter should reset
      expect(onSummary).toHaveBeenLastCalledWith(
        expect.objectContaining({ operationCount: 1 }),
      )
    })
  })

  describe('when timing is disabled', () => {
    const config = {
      timing: false,
      timingThreshold: 5,
    }

    it('should return the function result', () => {
      const timing = createTiming(config)
      const result = timing.run('concerns', () => 42, {
        path: 'test',
        name: 'test',
      })
      expect(result).toBe(42)
    })

    it('should not call onSlowOperation even for slow operations', () => {
      const onSlowOperation = vi.fn()
      const timing = createTiming({ ...config, onSlowOperation })

      timing.run(
        'concerns',
        () => {
          const start = Date.now()
          while (Date.now() - start < 10) {
            // busy wait
          }
          return 'done'
        },
        { path: 'test', name: 'test' },
      )

      expect(onSlowOperation).not.toHaveBeenCalled()
    })

    it('should not call onSummary when reportBatch is called', () => {
      const onSummary = vi.fn()
      const timing = createTiming({ ...config, onSummary })

      timing.run('concerns', () => 1, { path: 'a', name: 'x' })
      timing.reportBatch('concerns')

      expect(onSummary).not.toHaveBeenCalled()
    })
  })

  describe('isolation between types', () => {
    const config = {
      timing: true,
      timingThreshold: 1,
    }

    it('should track concerns and listeners separately', () => {
      const onSummary = vi.fn()
      const timing = createTiming({ ...config, onSummary })

      timing.run('concerns', () => 1, { path: 'a', name: 'x' })
      timing.run('concerns', () => 2, { path: 'b', name: 'y' })
      timing.run('listeners', () => 3, { path: 'c', name: 'z' })

      timing.reportBatch('concerns')
      expect(onSummary).toHaveBeenLastCalledWith(
        expect.objectContaining({ type: 'concerns', operationCount: 2 }),
      )

      timing.reportBatch('listeners')
      expect(onSummary).toHaveBeenLastCalledWith(
        expect.objectContaining({ type: 'listeners', operationCount: 1 }),
      )
    })
  })
})
