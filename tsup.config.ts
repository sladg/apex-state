import { wasmLoader } from 'esbuild-plugin-wasm'
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts', 'src/testing/index.ts'],
  format: ['esm'],
  dts: true,
  splitting: true,
  clean: true,
  sourcemap: true,
  platform: 'browser',
  external: ['react', 'zod', 'valtio', '@testing-library/react'],
  esbuildPlugins: [wasmLoader({ mode: 'embedded' })],
})
