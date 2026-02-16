/**
 * WASM Implementation - Flip Registration
 *
 * Registers flip pairs in WASM only, skips JS graph entirely.
 */

import { wasm } from '../../wasm/bridge'

export const registerFlipPair: typeof import('./flip').registerFlipPair = (
  store,
  path1,
  path2,
) => {
  // WASM mode: register in WASM only, skip JS graph
  wasm.registerFlipBatch([[path1, path2]])

  // Return WASM-only cleanup
  return () => {
    wasm.unregisterFlipBatch([[path1, path2]])
  }
}
