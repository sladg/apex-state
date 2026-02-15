# WASM-006: Inline WASM in tsup build ✅

**Status**: COMPLETE
**Completed**: 2026-02-14

## Summary

Successfully configured the build system to inline WASM as embedded binary data in the JS bundle. Consumers can import the library without any WASM plugins or configuration.

## Implementation

### Plugin Configuration

- **Plugin**: `esbuild-plugin-wasm` v1.1.0
- **Mode**: `embedded` (inlines WASM as binary data)
- **Location**: `tsup.config.ts`

```typescript
esbuildPlugins: [
  wasmLoader({ mode: 'embedded' }),
],
```

### How It Works

1. **Tree-shaking**: WASM is only included when actually imported
2. **Embedded mode**: WASM binary is inlined as binary data (not base64)
3. **Zero config**: Consumers need no WASM plugins or build configuration

### Bundle Size Impact

| State | ESM Bundle | Notes |
|-------|-----------|-------|
| **Without WASM** | 52 KB | Current state (WASM not imported yet) |
| **With WASM** | 275 KB | +223 KB when WASM is imported |
| **WASM Binary** | 158 KB | Source size (inlining adds encoding overhead) |

**Note**: WASM will only be included in bundle when imported (WASM-005)

## Verification

### Test Files

- `test-dist-import.mjs` - Verifies dist bundle is consumable
- `test-wasm-inline.mjs` - Tests WASM functions directly

### Verification Results

✅ No `.wasm` files in `dist/` (WASM is inlined)
✅ Bundle imports work without WASM plugins
✅ Both ESM and CJS outputs supported
✅ WASM functions accessible when imported
✅ Tree-shaking works (WASM excluded when not used)

## Files Modified

- `tsup.config.ts` - Added `wasmLoader({ mode: 'embedded' })`
- `src/wasm/index.ts` - Created re-export module for WASM functions
- `src/index.ts` - Added note about WASM configuration
- `package.json` - Already had `esbuild-plugin-wasm` from initial setup

## Next Steps

**WASM-005**: JS bridge - BoolLogic concerns
- Import WASM functions in BoolLogic concerns
- Replace effect() wrapping with WASM evaluation
- WASM will be automatically inlined when imported

## Acceptance Criteria ✅

- [x] `npm run build` produces a single JS bundle with WASM inlined
- [x] No `.wasm` files in `dist/`
- [x] Bundle size increase documented (~223 KB when WASM is imported)
- [x] Consumer test: `import { createGenericStore } from './dist'` works without plugins
- [x] Both ESM and CJS outputs work
