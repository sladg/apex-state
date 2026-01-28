# Phase 1, Task 01: Project Setup

**Task IDs**: APEX-1, APEX-2
**Priority**: P0 (Critical)
**Dependencies**: None
**Phase**: Foundation

---

## 🎯 Worker Prompt

**YOU ARE**: A TypeScript package setup specialist
**YOUR FOCUS**: Initialize the npm package with proper build configuration and testing setup
**STAY FOCUSED**: Do NOT implement any store logic, hooks, or types. Only setup the project infrastructure.
**SUCCESS MEANS**: Package builds successfully, tests can run, all dependencies installed

---

## 📋 Task Breakdown

### APEX-1: Initialize Package Structure

Initialize the npm package with TypeScript, tsup build configuration, and basic project structure.

**What to do:**
1. Create `package.json` with name `@sladg/apex-state`
2. Configure TypeScript with strict mode
3. Configure tsup for ESM/CJS builds (prepare for future WASM/Rust integration)
4. Create basic folder structure

**Configuration requirements:**
- Package name: `@sladg/apex-state`
- TypeScript target: ES2020+
- Strict mode: enabled
- Build tool: tsup
- Output: ESM and CJS formats

### APEX-2: Configure Vitest Testing Environment

Set up Vitest with React testing library for use-case specific tests.

**What to do:**
1. Install and configure Vitest
2. Add React testing library
3. Configure test environment for React components
4. Add test script to package.json

---

## ✅ Acceptance Criteria

### APEX-1 Criteria:
- [ ] `package.json` exists with correct name and metadata
- [ ] TypeScript configured with `tsconfig.json` (strict: true)
- [ ] tsup configured with `tsup.config.ts`
- [ ] Folder structure created:
  - `src/` - source code
  - `dist/` - build output (gitignored)
  - `tests/` - test files
- [ ] Peer dependencies declared: `react`, `zod`
- [ ] Direct dependencies installed: `valtio` (latest), `lodash`, `deepdash`
- [ ] Build script works: `npm run build` produces ESM and CJS output
- [ ] Package exports configured in package.json for ESM/CJS

### APEX-2 Criteria:
- [ ] Vitest installed with React support
- [ ] `@testing-library/react` installed
- [ ] `vitest.config.ts` created with React environment
- [ ] Test script in package.json: `npm test` runs vitest
- [ ] Basic test helper file exists: `tests/setup.ts`
- [ ] Dummy test passes to verify setup works

---

## 📦 Expected Output

### Files that MUST exist:

```
/package.json
/tsconfig.json
/tsup.config.ts
/vitest.config.ts
/src/index.ts (empty or with placeholder export)
/tests/setup.ts
/tests/dummy.test.ts (simple passing test)
/.gitignore
/README.md (minimal)
```

### package.json structure:

```json
{
  "name": "@sladg/apex-state",
  "version": "0.1.0",
  "type": "module",
  "exports": {
    ".": {
      "import": "./dist/index.js",
      "require": "./dist/index.cjs"
    }
  },
  "main": "./dist/index.cjs",
  "module": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "scripts": {
    "build": "tsup",
    "test": "vitest run",
    "test:watch": "vitest",
    "type-check": "tsc --noEmit"
  },
  "peerDependencies": {
    "react": "^18.0.0",
    "zod": "^3.0.0"
  },
  "dependencies": {
    "valtio": "latest",
    "lodash": "^4.17.21",
    "deepdash": "^5.3.9"
  },
  "devDependencies": {
    "@testing-library/react": "^14.0.0",
    "@types/lodash": "^4.14.0",
    "@types/react": "^18.0.0",
    "tsup": "^8.0.0",
    "typescript": "^5.0.0",
    "vitest": "^1.0.0"
  }
}
```

### tsconfig.json essentials:

```json
{
  "compilerOptions": {
    "strict": true,
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "jsx": "react-jsx",
    "declaration": true,
    "declarationMap": true,
    "skipLibCheck": true
  }
}
```

### tsup.config.ts essentials:

```typescript
import { defineConfig } from 'tsup'

export default defineConfig({
  entry: ['src/index.ts'],
  format: ['esm', 'cjs'],
  dts: true,
  splitting: false,
  clean: true,
  external: ['react', 'zod', 'valtio', 'lodash', 'deepdash']
})
```

---

## 🧪 Verification Steps

After completing this task, run:

```bash
# Install dependencies
npm install

# Type check should pass
npm run type-check

# Build should succeed
npm run build

# Tests should run (even if just dummy test)
npm test

# Verify dist/ contains:
# - index.js (ESM)
# - index.cjs (CJS)
# - index.d.ts (types)
ls -la dist/
```

---

## 🚨 Common Pitfalls

- **DON'T**: Implement any store logic yet
- **DON'T**: Create types or hooks
- **DON'T**: Over-configure with linting/formatting (not needed yet)
- **DO**: Keep it minimal and focused on build infrastructure
- **DO**: Ensure tsup can handle future WASM by keeping config simple
- **DO**: Test that the build actually works before moving on

---

## ➡️ Next Steps

Once this task is complete, proceed to:
- **Task 02**: `02-core-types.md` - Implement core TypeScript utilities
