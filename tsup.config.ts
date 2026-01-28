import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts'],
  format: ['esm', 'cjs'],
  dts: true,
  splitting: false,
  clean: true,
  sourcemap: true,
  external: [
    'react',
    'zod',
    'valtio',
    'lodash',
    'deepdash',
    'graphology',
    'graphology-shortest-path',
    'graphology-components',
    'graphology-traversal',
    'remeda'
  ]
})
