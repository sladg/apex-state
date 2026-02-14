import { beforeEach, describe, expect, it } from 'vitest'

describe('Reverse Dependency Index (WASM-004)', () => {
  let wasm: typeof import('../../rust/pkg/apex_state_wasm.js')

  beforeEach(async () => {
    wasm = await import('../../rust/pkg/apex_state_wasm.js')
    wasm.clear_rev_deps()
  })

  describe('Registration', () => {
    it('registers BoolLogic with single path', () => {
      const tree = JSON.stringify({
        IS_EQUAL: { path_id: 5, expected: true },
      })

      wasm.register_bool_logic(100, tree)

      const affected = wasm.affected_by_change(5)
      expect(Array.from(affected)).toEqual([100])
    })

    it('registers BoolLogic with multiple paths', () => {
      const tree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 1, expected: 'admin' } },
            { EXISTS: { path_id: 2 } },
          ],
        },
      })

      wasm.register_bool_logic(200, tree)

      expect(Array.from(wasm.affected_by_change(1))).toEqual([200])
      expect(Array.from(wasm.affected_by_change(2))).toEqual([200])
    })

    it('handles multiple BoolLogics referencing the same path', () => {
      const tree1 = JSON.stringify({
        IS_EQUAL: { path_id: 10, expected: true },
      })
      const tree2 = JSON.stringify({
        EXISTS: { path_id: 10 },
      })

      wasm.register_bool_logic(300, tree1)
      wasm.register_bool_logic(301, tree2)

      const affected = Array.from(wasm.affected_by_change(10)).sort()
      expect(affected).toEqual([300, 301])
    })

    it('extracts paths from deeply nested trees', () => {
      const tree = JSON.stringify({
        AND: {
          children: [
            { IS_EQUAL: { path_id: 1, expected: 'admin' } },
            {
              OR: {
                children: [
                  { EXISTS: { path_id: 2 } },
                  { GT: { path_id: 3, threshold: 0 } },
                ],
              },
            },
            {
              NOT: {
                child: { IS_EMPTY: { path_id: 4 } },
              },
            },
          ],
        },
      })

      wasm.register_bool_logic(400, tree)

      expect(Array.from(wasm.affected_by_change(1))).toEqual([400])
      expect(Array.from(wasm.affected_by_change(2))).toEqual([400])
      expect(Array.from(wasm.affected_by_change(3))).toEqual([400])
      expect(Array.from(wasm.affected_by_change(4))).toEqual([400])
    })
  })

  describe('Lookups', () => {
    it('returns empty array for paths with no dependencies', () => {
      const affected = wasm.affected_by_change(999)
      expect(Array.from(affected)).toEqual([])
    })

    it('returns sorted logic IDs', () => {
      const tree1 = JSON.stringify({
        EXISTS: { path_id: 20 },
      })
      const tree2 = JSON.stringify({
        EXISTS: { path_id: 20 },
      })
      const tree3 = JSON.stringify({
        EXISTS: { path_id: 20 },
      })

      // Register in non-sequential order
      wasm.register_bool_logic(503, tree1)
      wasm.register_bool_logic(501, tree2)
      wasm.register_bool_logic(502, tree3)

      const affected = Array.from(wasm.affected_by_change(20))
      expect(affected).toEqual([501, 502, 503]) // Sorted
    })

    it('handles lookups after multiple registrations', () => {
      // Register multiple logics with overlapping dependencies
      wasm.register_bool_logic(
        600,
        JSON.stringify({
          AND: {
            children: [{ EXISTS: { path_id: 1 } }, { EXISTS: { path_id: 2 } }],
          },
        }),
      )

      wasm.register_bool_logic(
        601,
        JSON.stringify({
          AND: {
            children: [{ EXISTS: { path_id: 2 } }, { EXISTS: { path_id: 3 } }],
          },
        }),
      )

      expect(Array.from(wasm.affected_by_change(1))).toEqual([600])
      expect(Array.from(wasm.affected_by_change(2)).sort()).toEqual([600, 601])
      expect(Array.from(wasm.affected_by_change(3))).toEqual([601])
    })
  })

  describe('Unregistration', () => {
    it('unregisters BoolLogic and cleans up path references', () => {
      const tree = JSON.stringify({
        AND: {
          children: [{ EXISTS: { path_id: 30 } }, { EXISTS: { path_id: 31 } }],
        },
      })

      wasm.register_bool_logic(700, tree)

      expect(Array.from(wasm.affected_by_change(30))).toEqual([700])
      expect(Array.from(wasm.affected_by_change(31))).toEqual([700])

      wasm.unregister_bool_logic(700)

      expect(Array.from(wasm.affected_by_change(30))).toEqual([])
      expect(Array.from(wasm.affected_by_change(31))).toEqual([])
    })

    it('only removes the specified logic from shared paths', () => {
      const tree1 = JSON.stringify({
        EXISTS: { path_id: 40 },
      })
      const tree2 = JSON.stringify({
        IS_EQUAL: { path_id: 40, expected: 'test' },
      })

      wasm.register_bool_logic(800, tree1)
      wasm.register_bool_logic(801, tree2)

      expect(Array.from(wasm.affected_by_change(40)).sort()).toEqual([800, 801])

      wasm.unregister_bool_logic(800)

      expect(Array.from(wasm.affected_by_change(40))).toEqual([801])
    })

    it('handles unregistering non-existent logic gracefully', () => {
      wasm.unregister_bool_logic(9999)

      // Should not throw or affect other registrations
      const tree = JSON.stringify({
        EXISTS: { path_id: 50 },
      })
      wasm.register_bool_logic(900, tree)

      expect(Array.from(wasm.affected_by_change(50))).toEqual([900])
    })
  })

  describe('Clear', () => {
    it('clears all registrations', () => {
      wasm.register_bool_logic(
        1000,
        JSON.stringify({
          EXISTS: { path_id: 60 },
        }),
      )
      wasm.register_bool_logic(
        1001,
        JSON.stringify({
          EXISTS: { path_id: 61 },
        }),
      )

      expect(Array.from(wasm.affected_by_change(60))).toEqual([1000])
      expect(Array.from(wasm.affected_by_change(61))).toEqual([1001])

      wasm.clear_rev_deps()

      expect(Array.from(wasm.affected_by_change(60))).toEqual([])
      expect(Array.from(wasm.affected_by_change(61))).toEqual([])
    })
  })

  describe('Statistics', () => {
    it('reports correct statistics', () => {
      wasm.register_bool_logic(
        1100,
        JSON.stringify({
          AND: {
            children: [{ EXISTS: { path_id: 1 } }, { EXISTS: { path_id: 2 } }],
          },
        }),
      )

      wasm.register_bool_logic(
        1101,
        JSON.stringify({
          EXISTS: { path_id: 3 },
        }),
      )

      const statsJson = wasm.rev_deps_stats()
      const stats = JSON.parse(statsJson)

      expect(stats.path_count).toBe(3) // paths 1, 2, 3
      expect(stats.logic_count).toBe(2) // logics 1100, 1101
      expect(stats.total_edges).toBe(3) // 1100→{1,2}, 1101→{3}
    })

    it('reports empty stats after clear', () => {
      wasm.register_bool_logic(
        1200,
        JSON.stringify({
          EXISTS: { path_id: 70 },
        }),
      )

      wasm.clear_rev_deps()

      const statsJson = wasm.rev_deps_stats()
      const stats = JSON.parse(statsJson)

      expect(stats.path_count).toBe(0)
      expect(stats.logic_count).toBe(0)
      expect(stats.total_edges).toBe(0)
    })
  })

  describe('Performance', () => {
    it('handles large numbers of registrations efficiently', () => {
      const start = performance.now()

      // Register 1000 BoolLogic trees
      for (let i = 0; i < 1000; i++) {
        wasm.register_bool_logic(
          i,
          JSON.stringify({
            IS_EQUAL: { path_id: i % 100, expected: i },
          }),
        )
      }

      const registrationTime = performance.now() - start

      // Lookups should be fast (O(1) average)
      const lookupStart = performance.now()
      for (let i = 0; i < 100; i++) {
        wasm.affected_by_change(i)
      }
      const lookupTime = performance.now() - lookupStart

      expect(registrationTime).toBeLessThan(100) // < 100ms for 1000 registrations
      expect(lookupTime).toBeLessThan(10) // < 10ms for 100 lookups
    })

    it('handles repeated register/unregister cycles', () => {
      const tree = JSON.stringify({
        EXISTS: { path_id: 80 },
      })

      const start = performance.now()

      for (let i = 0; i < 1000; i++) {
        wasm.register_bool_logic(2000 + i, tree)
        wasm.unregister_bool_logic(2000 + i)
      }

      const duration = performance.now() - start

      expect(duration).toBeLessThan(50) // < 50ms for 1000 cycles

      // Should be fully cleaned up
      expect(Array.from(wasm.affected_by_change(80))).toEqual([])

      const statsJson = wasm.rev_deps_stats()
      const stats = JSON.parse(statsJson)
      expect(stats.path_count).toBe(0)
    })
  })
})
