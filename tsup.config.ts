import { wasmLoader } from 'esbuild-plugin-wasm'
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts'],
  format: ['esm', 'cjs'],
  dts: true,
  splitting: false,
  clean: true,
  sourcemap: true,
  external: ['react', 'zod', 'valtio'],
  esbuildPlugins: [
    // Inline WASM as embedded binary for zero-config consumption
    wasmLoader({ mode: 'embedded' }),
  ],
})
