# WASM-EP15: Bundle Size Reduction

**Type**: Epic
**Priority**: P2
**Depends on**: —
**Goal**: Reduce the library's bundle size impact by (1) offering a non-inlined WASM build, and (2) applying Rust release optimisations to shrink the `.wasm` binary itself.

---

## Background

Currently the build uses `esbuild-plugin-wasm` in `embedded` mode (`tsup.config.ts`), which base64-encodes the `.wasm` binary and inlines it directly into the JS bundle. This means:

- Zero configuration for consumers — no `.wasm` file to host or import
- **+~223KB to the bundle** (before gzip) regardless of whether the library is actually used

This is acceptable for simple setups but painful for bundle-size-sensitive applications.

---

## Story 1: Dual Build Output (inlined + external)

**Points**: 3

Produce two build artefacts from a single source:

| Artefact | Entry point | WASM loading | Use case |
|---|---|---|---|
| `dist/index.js` | `src/index.ts` | Inlined (current) | Zero-config, simple apps |
| `dist/index.external.js` | same | External `.wasm` file | Bundle-size-conscious apps |

### Implementation

1. **Add a second tsup entry** with `wasmLoader({ mode: 'file' })` instead of `'embedded'`:

   ```ts
   // tsup.config.ts
   export default defineConfig([
     {
       // Current: inlined
       entry: { index: 'src/index.ts', 'testing/index': 'src/testing/index.ts' },
       esbuildPlugins: [wasmLoader({ mode: 'embedded' })],
       outDir: 'dist',
       // ...rest unchanged
     },
     {
       // New: external .wasm file
       entry: { 'index.external': 'src/index.ts' },
       esbuildPlugins: [wasmLoader({ mode: 'file' })],
       outDir: 'dist',
       // ...rest unchanged
     },
   ])
   ```

2. **Update `package.json` exports** to expose both:

   ```json
   {
     "exports": {
       ".": "./dist/index.js",
       "./external": "./dist/index.external.js"
     }
   }
   ```

3. **Verify the external build** copies the `.wasm` file into `dist/` alongside the JS.

4. **Document** in README: consumers who want to save ~223KB should import from `@sladg/apex-state/external` and ensure their bundler/server serves the `.wasm` file.

### Acceptance

- [ ] `dist/index.js` still works with zero config (embedded, unchanged behaviour)
- [ ] `dist/index.external.js` + `dist/*.wasm` work when the consumer's bundler handles WASM
- [ ] Both outputs pass the full test suite
- [ ] `package.json` exports updated
- [ ] README documents the two import paths

---

## Story 2: Rust Release Optimisations

**Points**: 2

Experiment with Cargo release profile flags to shrink the `.wasm` binary. The current build uses `wasm-pack build --target bundler` with no custom profile, which defaults to Cargo's release profile (opt-level 3, no LTO, no stripping).

### Candidates to evaluate

Add a custom profile to `rust/Cargo.toml`:

```toml
[profile.release]
opt-level = "z"      # optimise for size (vs "3" for speed, "s" for balanced)
lto = true           # link-time optimisation — significant size reduction
codegen-units = 1    # required for full LTO benefit
strip = "symbols"    # strip debug symbols (wasm-pack may do this already)
panic = "abort"      # smaller panic machinery (no unwinding)
```

Also evaluate:

- **`wee_alloc`** — a smaller allocator already listed as an optional dep in `Cargo.toml`. Enable it for release builds:
  ```toml
  [features]
  default = ["console_error_panic_hook"]
  release-small = ["wee_alloc"]
  ```

- **`wasm-opt`** — `wasm-pack` runs `wasm-opt` automatically in release mode (`-O`). Verify it's active, and experiment with `-Oz` (size) vs `-O3` (speed) via a `wasm-pack` config or post-build step.

### Process

1. Measure baseline: `ls -lh rust/pkg/apex_state_wasm_bg.wasm`
2. Apply each flag one at a time, rebuild, measure
3. Record results in a table (see below)
4. Pick the combination with the best size/perf tradeoff

### Results table (fill in during implementation)

| Configuration | .wasm size | gzip size | Benchmark impact | Notes |
|---|---|---|---|---|
| Baseline (current) | — | — | — | opt-level=3, no LTO |
| opt-level="s" | | | | |
| opt-level="z" | | | | |
| + lto=true | | | | |
| + strip=symbols | | | | |
| + panic=abort | | | | |
| + wee_alloc | | | | |
| Best combination | | | | |

### Acceptance

- [ ] Baseline `.wasm` size measured and recorded
- [ ] At least 4 configurations evaluated
- [ ] Results table filled in
- [ ] Best configuration applied to `rust/Cargo.toml`
- [ ] All tests still pass after optimisation
- [ ] No meaningful regression in benchmark timing (< 5% slowdown acceptable for size wins)

---

## Summary

| Story | Points | What changes |
|---|---|---|
| 1: Dual build output | 3 | `tsup.config.ts`, `package.json` exports, README |
| 2: Rust release flags | 2 | `rust/Cargo.toml` profile, measure + record results |
| **Total** | **5** | |

Stories are independent — can be done in either order.
