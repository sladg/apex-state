# String Interpolation

Dynamic template helpers that power concerns like `dynamicTooltip`, `dynamicLabel`, and `dynamicPlaceholder`.

## Import

```ts
import { extractPlaceholders, interpolateTemplate } from "@sladg/apex-state";
import type { ExtractPlaceholders, ValidatedTemplate } from "@sladg/apex-state";
```

## API

| Helper                                 | Module                       | Purpose                                                           | Notes                                                      |
| -------------------------------------- | ---------------------------- | ----------------------------------------------------------------- | ---------------------------------------------------------- |
| `extractPlaceholders(template)`        | `src/utils/interpolation.ts` | Returns array of `{{path}}` placeholders for tooling.             | Ignores duplicates to keep deterministic order.            |
| `interpolateTemplate(template, state)` | `src/utils/interpolation.ts` | Produces string with placeholders resolved.                       | Leaves unknown paths untouched for debuggability.          |
| `ExtractPlaceholders<T>`               | `src/types/interpolation.ts` | Compile-time union of placeholder paths.                          | Works with template literals; strips braces automatically. |
| `ValidatedTemplate<T, DATA>`           | `src/types/interpolation.ts` | Ensures placeholders exist on `DATA`; yields `never` on mismatch. | Drives intellisense for concern configs.                   |

## Runtime Behavior

- Strings, numbers, and booleans are rendered verbatim.
- `null`, `undefined`, or missing paths leave the original `{{placeholder}}` so issues are obvious.
- Objects and arrays are not stringified; they fall back to the placeholder for safety.

## Usage Examples

See `examples/interpolation.ts` for complete usage with `extractPlaceholders` and `interpolateTemplate`.

See `examples/concerns.tsx` for how dynamic concerns (`dynamicTooltip`, `dynamicLabel`) use templates via `useConcerns`.

## Edge Cases & Tests

- Template parsing: `tests/types/interpolation.test.ts`
- Runtime integration: `tests/integration/concerns-ui.test.tsx`
- Escaping braces: double the braces (`{{\{` / `\}}`) for literal curly braces.

## Source Files

- `src/utils/interpolation.ts` — runtime helpers
- `src/types/interpolation.ts` — type-level utilities

Touch both files together when changing placeholder semantics.
