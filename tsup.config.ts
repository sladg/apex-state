import { wasmLoader } from 'esbuild-plugin-wasm'
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts'],
  format: ['esm'],
  dts: true,
  splitting: false,
  clean: true,
  sourcemap: true,
  external: ['react', 'zod', 'valtio'],
  esbuildPlugins: [wasmLoader({ mode: 'embedded' })],
})
