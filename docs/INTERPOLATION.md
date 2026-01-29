# String Interpolation

Dynamic template helpers that power concerns like `dynamicTooltip`, `dynamicLabel`, and `dynamicPlaceholder`.

## Where to Import

Runtime helpers are exported from the package root:

```ts
import { extractPlaceholders, interpolateTemplate } from "apex-state";
```

Type helpers are available as:

```ts
import type { ExtractPlaceholders, ValidatedTemplate } from "apex-state";
```

## Runtime vs Type Utilities

| Helper                                 | Module                       | Purpose                                                           | Notes                                                      |
| -------------------------------------- | ---------------------------- | ----------------------------------------------------------------- | ---------------------------------------------------------- |
| `extractPlaceholders(template)`        | `src/utils/interpolation.ts` | Returns array of `{{path}}` placeholders for tooling.             | Ignores duplicates to keep deterministic order.            |
| `interpolateTemplate(template, state)` | `src/utils/interpolation.ts` | Produces string with placeholders resolved.                       | Leaves unknown paths untouched for debuggability.          |
| `ExtractPlaceholders<T>`               | `src/types/interpolation.ts` | Compile-time union of placeholder paths.                          | Works with template literals; strips braces automatically. |
| `ValidatedTemplate<T, DATA>`           | `src/types/interpolation.ts` | Ensures placeholders exist on `DATA`; yields `never` on mismatch. | Drives intellisense for concern configs.                   |

## Runtime Behavior

- Strings, numbers, and booleans are rendered verbatim.
- `null`, `undefined`, or missing paths leave the original `{{placeholder}}` so issues are obvious.
- Objects and arrays are not stringified automatically; they also fall back to the placeholder for safety.

```ts
const state = { user: { name: "Alice" }, count: 5 };

interpolateTemplate("Hello {{user.name}}", state);
// "Hello Alice"

interpolateTemplate("Missing: {{invalid.path}}", state);
// "Missing: {{invalid.path}}"
```

## Using in Concerns

The prebuilt dynamic concerns wire these helpers for you:

```ts
store.useConcerns("legs.0.strike", {
  dynamicTooltip: {
    concern: dynamicTooltip,
    config: { template: "Current value: {{legs.0.strike}}" },
  },
});

const { dynamicTooltip } = store.useFieldConcerns("legs.0.strike");
// → "Current value: 105"
```

When creating custom concerns or side-effects, favour `ValidatedTemplate` to catch typoed paths at compile time.

## Edge Cases & Tests

- Template parsing: `tests/types/interpolation.test.ts` verifies placeholder extraction and validation.
- Runtime integration: `tests/integration/concerns-ui.test.tsx` exercises dynamic tooltip/label concerns end-to-end.
- Escaping braces: double the braces (`{{\{` / `\}}`) if you need literal curly braces; the runtime treats escaped braces as literals.

## File Map

- `src/utils/interpolation.ts` — runtime helpers + behaviour tests.
- `src/types/interpolation.ts` — type-level utilities.

Touch both files together when changing placeholder semantics so the runtime and type system stay aligned.
