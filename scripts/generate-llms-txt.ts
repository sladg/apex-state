/**
 * Generates llms.txt (index) and llms-full.txt (concatenated docs) for LLM consumption.
 *
 * Usage: npx tsx scripts/generate-llms-txt.ts
 *
 * The llms.txt follows the spec at https://llmstxt.org/
 *
 * The llms-full.txt concatenates:
 * 1. Usage examples (extracted from @llms-example markers in examples/ and src/)
 * 2. Public API surface (from src/index.ts)
 * 3. Documentation (README, guides, architecture)
 *
 * ## Adding examples
 *
 * Add markers in any .ts/.tsx file under examples/ or src/:
 *
 *   // @llms-example: My Example Title
 *   const store = createGenericStore<MyState>()
 *   store.useFieldStore('path')
 *   // @llms-example-end
 *
 * Examples in examples/ are type-checked by `tsc --noEmit` via tsconfig paths.
 * If the API changes, the build breaks — no more stale markdown examples.
 */

import { execSync } from 'node:child_process'
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'

const ROOT = resolve(import.meta.dirname, '..')

// Optional --outdir <path> to write files somewhere other than project root
const outdirIdx = process.argv.indexOf('--outdir')
const OUTDIR =
  outdirIdx !== -1 && process.argv[outdirIdx + 1]
    ? resolve(process.argv[outdirIdx + 1])
    : ROOT

// ---------------------------------------------------------------------------
// llms.txt — index file
// ---------------------------------------------------------------------------

const LLMS_TXT = `# @sladg/apex-state

> Reactive state management for React built on Valtio. Declare what your fields need — validation, conditional UI, sync, listeners — and the store handles the rest. Optional Rust/WASM accelerator for complex workloads (up to 367x faster). Type-safe paths with DeepKey<T> and DeepValue<T, P>.

Key concepts:
- Concerns-based architecture: validation (Zod), conditional UI (BoolLogic), dynamic text — all declarative
- Two-proxy pattern: read from state, write to _concerns — prevents infinite loops
- Side effects: sync paths, flip paths, aggregations, listeners — registered via hooks
- Dual-layer: JS/React owns reactivity and rendering; Rust/WASM owns heavy computation
- Composable hooks: useBufferedField, useThrottledField, useTransformedField chain together
- Testing mock: drop-in vi.mock replacement with call tracking via @sladg/apex-state/testing

## Quick Start

\`\`\`bash
npm install @sladg/apex-state valtio zod react
\`\`\`

\`\`\`tsx
import { createGenericStore } from '@sladg/apex-state'
import { z } from 'zod'

const store = createGenericStore<MyState>()

// In components:
store.useSideEffects('id', { syncPaths: [...], flipPaths: [...] })
store.useConcerns('id', { 'path': { validationState: { schema: z.string() } } })
const { value, setValue, validationState } = store.useFieldStore('path')
\`\`\`

## API Reference

- [Store & Hooks](docs/guides/STORE_HOOKS.md): createGenericStore, useFieldStore, useStore, useJitStore, useConcerns, useSideEffects, withConcerns, composable field hooks (buffered, throttled, transformed)
- [Concerns System](docs/guides/CONCERNS_GUIDE.md): Validation (Zod), BoolLogic conditions (disabledWhen, visibleWhen, readonlyWhen), dynamic text (tooltip, label, placeholder), custom concerns
- [Side Effects](docs/SIDE_EFFECTS_GUIDE.md): Sync paths, flip paths, aggregations (consensus mode), listeners — registration, processing order, scoping
- [String Interpolation](docs/INTERPOLATION.md): Template helpers for dynamic text concerns — extractPlaceholders, interpolateTemplate
- [Hash Key Paths](docs/WILD_FUNCTION_GUIDE.md): _() hash key utility for Record<string, V> types
- [Testing Mock](docs/TESTING_MOCK.md): Drop-in vi.mock replacement with call tracking — \`@sladg/apex-state/testing\`
- [Record Migration](docs/RECORD_MIGRATION.md): Migration patterns for dynamic Record types
`

// ---------------------------------------------------------------------------
// Docs to include (order matters)
// ---------------------------------------------------------------------------

const DOCS = [
  'README.md',
  'docs/guides/STORE_HOOKS.md',
  'docs/guides/CONCERNS_GUIDE.md',
  'docs/SIDE_EFFECTS_GUIDE.md',
  'docs/INTERPOLATION.md',
  'docs/WILD_FUNCTION_GUIDE.md',
  'docs/TESTING_MOCK.md',
  'docs/WASM_ARCHITECTURE.md',
  'docs/guides/ARCHITECTURE.md',
]

// ---------------------------------------------------------------------------
// Extract examples from @llms-example markers AND @example TSDoc tags
// ---------------------------------------------------------------------------

interface LlmsExample {
  title: string
  code: string
  sourceFile: string
}

const stripCommonIndent = (codeLines: string[]): string[] => {
  const nonEmptyLines = codeLines.filter((l) => l.trim().length > 0)
  const minIndent = nonEmptyLines.reduce((min, l) => {
    const indent = l.match(/^(\s*)/)?.[1].length ?? 0
    return Math.min(min, indent)
  }, Infinity)
  return minIndent === Infinity
    ? codeLines
    : codeLines.map((l) => l.slice(minIndent))
}

// Parse // @llms-example: Title ... // @llms-example-end blocks
const parseLlmsExamples = (
  content: string,
  sourceFile: string,
): LlmsExample[] => {
  const lines = content.split('\n')
  const examples: LlmsExample[] = []
  let currentTitle: string | null = null
  let currentLines: string[] = []
  let collecting = false

  for (const line of lines) {
    const startMatch = line.match(/@llms-example:\s*(.+)/)
    if (startMatch) {
      currentTitle = startMatch[1].replace(/\s*-*>$/, '').trim()
      currentLines = []
      collecting = true
      continue
    }

    if (line.includes('@llms-example-end')) {
      if (collecting && currentTitle) {
        const stripped = stripCommonIndent(currentLines)
        examples.push({
          title: currentTitle,
          code: stripped.join('\n').trim(),
          sourceFile,
        })
      }
      collecting = false
      currentTitle = null
      continue
    }

    if (collecting) {
      currentLines.push(line)
    }
  }

  return examples
}

// Parse @example blocks from TSDoc comments:
//  * @example Optional title
//  * ```typescript
//  * code here
//  * ```
const parseTsDocExamples = (
  content: string,
  sourceFile: string,
): LlmsExample[] => {
  const lines = content.split('\n')
  const examples: LlmsExample[] = []
  let title: string | null = null
  let codeLines: string[] = []
  let inFence = false

  for (const line of lines) {
    // Strip leading ` * ` from TSDoc lines
    const stripped = line.replace(/^\s*\*\s?/, '')

    // Match @example with optional title text
    const exampleMatch = stripped.match(/^@example\s*(.*)/)
    if (exampleMatch) {
      title = exampleMatch[1].trim() || null
      continue
    }

    // Opening fence inside a TSDoc @example
    if (title !== null && !inFence && stripped.match(/^```/)) {
      inFence = true
      codeLines = []
      continue
    }

    // Closing fence
    if (inFence && stripped.match(/^```/)) {
      if (title && codeLines.length > 0) {
        const strippedCode = stripCommonIndent(codeLines)
        examples.push({
          title,
          code: strippedCode.join('\n').trim(),
          sourceFile,
        })
      }
      inFence = false
      title = null
      continue
    }

    if (inFence) {
      codeLines.push(stripped)
    }
  }

  return examples
}

const findMarkerFiles = (): string[] => {
  try {
    const output = execSync(
      'grep -rl "@llms-example:\\|@example" --include="*.ts" --include="*.tsx" examples/ src/',
      { cwd: ROOT, encoding: 'utf-8' },
    )
    return output.trim().split('\n').filter(Boolean)
  } catch {
    return []
  }
}

const extractExamples = (): LlmsExample[] => {
  const matchingFiles = findMarkerFiles()
  return matchingFiles.flatMap((relPath) => {
    const content = readFileSync(resolve(ROOT, relPath), 'utf-8')
    return [
      ...parseLlmsExamples(content, relPath),
      ...parseTsDocExamples(content, relPath),
    ]
  })
}

// ---------------------------------------------------------------------------
// Extract public API exports from src/index.ts
// ---------------------------------------------------------------------------

const extractPublicApi = (): string => {
  const indexContent = readFileSync(resolve(ROOT, 'src/index.ts'), 'utf-8')
  return `# Public API Surface

> Extracted from \`src/index.ts\` — these are the exports consumers can import.

\`\`\`typescript
${indexContent.trim()}
\`\`\``
}

// ---------------------------------------------------------------------------
// Build llms-full.txt
// ---------------------------------------------------------------------------

const docSections = DOCS.map((docPath) => {
  const content = readFileSync(resolve(ROOT, docPath), 'utf-8')
  const stripped = content.replace(/^---\n[\s\S]*?\n---\n/, '')
  return `<!-- source: ${docPath} -->\n\n${stripped.trim()}`
})

const examples = extractExamples()
const apiSection = extractPublicApi()

const examplesSection =
  examples.length > 0
    ? examples
        .map(
          (ex) =>
            `<!-- source: ${ex.sourceFile} -->\n\n## ${ex.title}\n\n\`\`\`tsx\n${ex.code}\n\`\`\``,
        )
        .join('\n\n---\n\n')
    : '> No @llms-example markers found. Add markers to examples/ or src/ files to include usage examples.'

const LLMS_FULL_TXT = `# @sladg/apex-state — Full Documentation

> This file contains the complete documentation for @sladg/apex-state, concatenated for LLM consumption.
> Generated by: npx tsx scripts/generate-llms-txt.ts
>
> Structure:
> 1. Usage Examples (extracted from @llms-example markers in examples/ and src/)
> 2. Public API Surface (from src/index.ts)
> 3. Documentation (README, guides, architecture)

---

# Usage Examples

> Type-checked examples extracted from the codebase. Each example lives in the source file indicated,
> marked with \`@llms-example: Title\` / \`@llms-example-end\` comments.

---

${examplesSection}

---

# Public API

---

${apiSection}

---

# Documentation

---

${docSections.join('\n\n---\n\n')}
`

// ---------------------------------------------------------------------------
// Write files
// ---------------------------------------------------------------------------

mkdirSync(OUTDIR, { recursive: true })
writeFileSync(resolve(OUTDIR, 'llms.txt'), LLMS_TXT)
writeFileSync(resolve(OUTDIR, 'llms-full.txt'), LLMS_FULL_TXT)

const llmsTxtLines = LLMS_TXT.split('\n').length
const llmsFullLines = LLMS_FULL_TXT.split('\n').length
console.log(`llms.txt:      ${llmsTxtLines} lines`)
console.log(
  `llms-full.txt: ${llmsFullLines} lines (${examples.length} usage examples + API surface + ${DOCS.length} docs)`,
)

if (examples.length > 0) {
  console.log('\nExtracted examples:')
  for (const ex of examples) {
    console.log(`  - "${ex.title}" from ${ex.sourceFile}`)
  }
}
