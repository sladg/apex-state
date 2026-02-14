import { describe, expect, it } from 'vitest'

describe('WASM Integration (WASM-001)', () => {
  it('can import WASM module from rust/pkg', async () => {
    // Dynamic import to handle async WASM initialization
    const wasm = await import('../../rust/pkg/apex_state_wasm.js')

    expect(wasm.add).toBeDefined()
    expect(wasm.version).toBeDefined()
  })

  it('exports work correctly from WASM', async () => {
    const wasm = await import('../../rust/pkg/apex_state_wasm.js')

    expect(wasm.add(2, 3)).toBe(5)
    expect(wasm.add(-1, 1)).toBe(0)
    expect(wasm.add(0, 0)).toBe(0)
  })

  it('version string matches Cargo.toml', async () => {
    const wasm = await import('../../rust/pkg/apex_state_wasm.js')

    expect(wasm.version()).toBe('0.1.0')
  })
})
