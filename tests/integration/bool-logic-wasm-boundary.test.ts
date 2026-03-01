/**
 * BoolLogic WASM boundary tests
 *
 * Verifies that both JSON formats cross the WASM boundary correctly and evaluate
 * against the same shared state fixture used in:
 * - tests/utils/bool-logic.test.ts (TypeScript evaluator)
 * - rust/src/bool_logic.rs (Rust unit tests)
 *
 * Named format:    { IS_EQUAL: ['user.role', 'admin'] }
 * Shorthand format: ['user.role', 'admin']  ← deserialized to IS_EQUAL in Rust
 */

import { afterEach, describe, expect, it } from 'vitest'

import { createWasmPipeline, type WasmPipeline } from '~/wasm/bridge'

// -- Shared state fixture (mirrors Rust make_state() + TS bool-logic.test.ts) --

const STATE = {
  user: {
    role: 'admin',
    age: 25,
    active: true,
    email: 'alice@example.com',
    score: 150,
    tags: ['premium'],
    bio: '',
    deleted: null,
    profile: {
      verified: true,
      name: 'Alice',
    },
  },
  document: {
    id: 'doc-456',
    status: 'draft',
  },
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Create a pipeline pre-loaded with the shared STATE fixture. */
const makePipeline = (): WasmPipeline => {
  const pipeline = createWasmPipeline()
  pipeline.shadowInit(STATE)
  return pipeline
}

// ---------------------------------------------------------------------------
// Test state
// ---------------------------------------------------------------------------

let pipeline: WasmPipeline

afterEach(() => {
  pipeline.destroy()
})

// ---------------------------------------------------------------------------
// Named operator format — baseline
// ---------------------------------------------------------------------------

describe('named operator format (baseline)', () => {
  it('should evaluate IS_EQUAL correctly at registration', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({ IS_EQUAL: ['user.role', 'admin'] }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should re-evaluate when the input path changes', () => {
    pipeline = makePipeline()
    pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({ IS_EQUAL: ['user.role', 'admin'] }),
        },
      ],
    })

    pipeline.processChanges([{ path: 'user.role', value: 'editor', meta: {} }])
    const { state_changes } = pipeline.pipelineFinalize([])

    expect(state_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          path: '_concerns.result',
          value: false,
          meta: {},
        }),
      ]),
    )
  })
})

// ---------------------------------------------------------------------------
// Shorthand [path, value] format
// ---------------------------------------------------------------------------

describe('shorthand [path, value] format', () => {
  it('should evaluate shorthand correctly at registration', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.role', 'admin']),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should evaluate shorthand to false when value differs', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.role', 'editor']),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: false, meta: {} }),
      ]),
    )
  })

  it('should re-evaluate shorthand when the input path changes', () => {
    pipeline = makePipeline()
    pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.role', 'admin']),
        },
      ],
    })

    pipeline.processChanges([{ path: 'user.role', value: 'editor', meta: {} }])
    const { state_changes } = pipeline.pipelineFinalize([])

    expect(state_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          path: '_concerns.result',
          value: false,
          meta: {},
        }),
      ]),
    )
  })

  it('should evaluate shorthand for numbers', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.age', 25]),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should evaluate shorthand for booleans', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.active', true]),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should evaluate shorthand for nested paths', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify(['user.profile.verified', true]),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })
})

// ---------------------------------------------------------------------------
// Parity: shorthand == named IS_EQUAL
// ---------------------------------------------------------------------------

describe('shorthand parity with named IS_EQUAL', () => {
  it('should produce identical initial evaluation', () => {
    const namedPipeline = makePipeline()
    const { bool_logic_changes: namedChanges } = namedPipeline.registerConcerns(
      {
        registration_id: 'test',
        bool_logics: [
          {
            output_path: 'result',
            tree_json: JSON.stringify({ IS_EQUAL: ['user.role', 'admin'] }),
          },
        ],
      },
    )
    namedPipeline.destroy()

    const shorthandPipeline = makePipeline()
    const { bool_logic_changes: shorthandChanges } =
      shorthandPipeline.registerConcerns({
        registration_id: 'test',
        bool_logics: [
          {
            output_path: 'result',
            tree_json: JSON.stringify(['user.role', 'admin']),
          },
        ],
      })
    shorthandPipeline.destroy()

    // Assign pipeline so afterEach doesn't throw (already destroyed above)
    pipeline = makePipeline()

    const expected = expect.arrayContaining([
      expect.objectContaining({ path: 'result', value: true, meta: {} }),
    ])
    expect(namedChanges).toEqual(expected)
    expect(shorthandChanges).toEqual(expected)
  })

  it('should produce identical re-evaluation after state change', () => {
    const runPipeline = (treeJson: string) => {
      const p = makePipeline()
      p.registerConcerns({
        registration_id: 'test',
        bool_logics: [{ output_path: 'result', tree_json: treeJson }],
      })
      p.processChanges([{ path: 'user.role', value: 'editor', meta: {} }])
      const { state_changes } = p.pipelineFinalize([])
      p.destroy()
      return state_changes
    }

    const expected = expect.arrayContaining([
      expect.objectContaining({
        path: '_concerns.result',
        value: false,
        meta: {},
      }),
    ])

    expect(
      runPipeline(JSON.stringify({ IS_EQUAL: ['user.role', 'admin'] })),
    ).toEqual(expected)
    expect(runPipeline(JSON.stringify(['user.role', 'admin']))).toEqual(
      expected,
    )

    pipeline = makePipeline() // for afterEach
  })
})

// ---------------------------------------------------------------------------
// Shorthand as children of AND / OR / NOT
// ---------------------------------------------------------------------------

describe('shorthand as children of compound operators', () => {
  it('AND with shorthand children should evaluate correctly', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            AND: [
              ['user.role', 'admin'], // shorthand
              { EXISTS: 'user.email' }, // named
              { GTE: ['user.age', 18] }, // named numeric
            ],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('AND with failing shorthand child should be false', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            AND: [
              ['user.role', 'admin'], // true
              ['user.active', false], // false — active is true
            ],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: false, meta: {} }),
      ]),
    )
  })

  it('OR with shorthand children should evaluate correctly', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            OR: [
              ['user.role', 'editor'], // false
              ['user.role', 'admin'], // true
            ],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('NOT with shorthand child should negate correctly', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            NOT: ['user.role', 'guest'], // NOT (role == guest) → true
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })
})

// ---------------------------------------------------------------------------
// CONTAINS_ANY — array contains at least one of the given candidates
// ---------------------------------------------------------------------------

describe('CONTAINS_ANY', () => {
  it('should return true when array contains any of the candidates', () => {
    pipeline = makePipeline()
    // user.tags = ['premium'] — 'premium' is among the candidates
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ANY: ['user.tags', ['premium', 'vip']],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should return false when no candidate is in the array', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ANY: ['user.tags', ['vip', 'free']],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: false, meta: {} }),
      ]),
    )
  })

  it('should re-evaluate when a matching candidate is added to the array', () => {
    pipeline = makePipeline()
    pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ANY: ['user.tags', ['vip', 'free']],
          }),
        },
      ],
    })

    // Add 'vip' to tags — now matches
    pipeline.processChanges([
      { path: 'user.tags', value: ['premium', 'vip'], meta: {} },
    ])
    const { state_changes } = pipeline.pipelineFinalize([])

    expect(state_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          path: '_concerns.result',
          value: true,
          meta: {},
        }),
      ]),
    )
  })
})

// ---------------------------------------------------------------------------
// CONTAINS_ALL — array contains every one of the given candidates
// ---------------------------------------------------------------------------

describe('CONTAINS_ALL', () => {
  it('should return false when only some candidates are present', () => {
    pipeline = makePipeline()
    // user.tags = ['premium'] only — 'vip' is missing
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ALL: ['user.tags', ['premium', 'vip']],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: false, meta: {} }),
      ]),
    )
  })

  it('should return true when all candidates are present', () => {
    pipeline = makePipeline()
    // user.tags = ['premium'] — checking against ['premium'] only
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ALL: ['user.tags', ['premium']],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should re-evaluate to true when the missing candidate is added', () => {
    pipeline = makePipeline()
    pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            CONTAINS_ALL: ['user.tags', ['premium', 'vip']],
          }),
        },
      ],
    })

    // Add 'vip' — now all candidates are present
    pipeline.processChanges([
      { path: 'user.tags', value: ['premium', 'vip'], meta: {} },
    ])
    const { state_changes } = pipeline.pipelineFinalize([])

    expect(state_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          path: '_concerns.result',
          value: true,
          meta: {},
        }),
      ]),
    )
  })
})

// ---------------------------------------------------------------------------
// Complex expression parity — same expression as Rust + TypeScript tests
// ---------------------------------------------------------------------------

describe('complex expression parity', () => {
  it('should evaluate complex AND with named operators (mirrors test_eval_complex_real_world)', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            AND: [
              {
                OR: [
                  { IS_EQUAL: ['user.role', 'admin'] },
                  { IS_EQUAL: ['user.role', 'editor'] },
                ],
              },
              { GTE: ['user.age', 18] },
              { GT: ['user.score', 100] },
              { NOT: { IS_EMPTY: 'user.tags' } },
              { IN: ['user.role', ['admin', 'editor', 'mod']] },
              { EXISTS: 'document.id' },
              { IS_EQUAL: ['user.profile.verified', true] },
            ],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should evaluate same complex AND with shorthand children (mirrors test_eval_complex_with_shorthand_children)', () => {
    pipeline = makePipeline()
    const { bool_logic_changes } = pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            AND: [
              {
                OR: [
                  ['user.role', 'admin'],
                  ['user.role', 'editor'],
                ],
              },
              { GTE: ['user.age', 18] },
              { GT: ['user.score', 100] },
              { NOT: { IS_EMPTY: 'user.tags' } },
              { IN: ['user.role', ['admin', 'editor', 'mod']] },
              { EXISTS: 'document.id' },
              ['user.profile.verified', true],
            ],
          }),
        },
      ],
    })

    expect(bool_logic_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ path: 'result', value: true, meta: {} }),
      ]),
    )
  })

  it('should re-evaluate complex shorthand expression when key path changes', () => {
    pipeline = makePipeline()
    pipeline.registerConcerns({
      registration_id: 'test',
      bool_logics: [
        {
          output_path: 'result',
          tree_json: JSON.stringify({
            AND: [
              {
                OR: [
                  ['user.role', 'admin'],
                  ['user.role', 'editor'],
                ],
              },
              { GTE: ['user.age', 18] },
              ['user.profile.verified', true],
            ],
          }),
        },
      ],
    })

    // Flip role to something outside the OR — expression should become false
    pipeline.processChanges([{ path: 'user.role', value: 'viewer', meta: {} }])
    const { state_changes } = pipeline.pipelineFinalize([])

    expect(state_changes).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          path: '_concerns.result',
          value: false,
          meta: {},
        }),
      ]),
    )
  })
})
