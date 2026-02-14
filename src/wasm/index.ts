/**
 * WASM utilities for apex-state
 * Re-exports WASM functions for internal use
 */

// Re-export WASM functions for internal use
// The .wasm files will be inlined as base64 by esbuild-plugin-wasm during build
export * from '../../rust/pkg/apex_state_wasm.js'
