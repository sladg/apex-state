# String Interpolation

Template string interpolation for dynamic text in concerns and side effects.

## API

### Runtime Functions

```typescript
import { extractPlaceholders, interpolateTemplate } from 'apex-state'
```

#### `extractPlaceholders(template: string): string[]`

Extract all `{{path}}` placeholders from a template string.

```typescript
extractPlaceholders("Hello {{user.name}}, you have {{count}} messages")
// ["user.name", "count"]
```

#### `interpolateTemplate<STATE>(template: string, state: STATE): string`

Replace `{{path}}` placeholders with values from state.

```typescript
const state = { user: { name: 'Alice' }, count: 5 }

interpolateTemplate("Hello {{user.name}}", state)
// "Hello Alice"

interpolateTemplate("You have {{count}} messages", state)
// "You have 5 messages"
```

**Behavior:**
- Strings, numbers, booleans are interpolated
- Missing/null/undefined/objects leave original `{{path}}` for debugging

```typescript
interpolateTemplate("Missing: {{invalid.path}}", state)
// "Missing: {{invalid.path}}"
```

### Type Utilities

```typescript
import type { ExtractPlaceholders, ValidatedTemplate } from 'apex-state'
```

#### `ExtractPlaceholders<S>`

Extract placeholder paths at type level.

```typescript
type Paths = ExtractPlaceholders<"Hello {{user.name}}, email: {{user.email}}">
// "user.name" | "user.email"
```

#### `ValidatedTemplate<T, DATA>`

Validate that all placeholders are valid paths in DATA. Returns template if valid, `never` if invalid.

```typescript
type State = { user: { name: string } }

type Valid = ValidatedTemplate<"Hello {{user.name}}", State>
// "Hello {{user.name}}"

type Invalid = ValidatedTemplate<"Hello {{invalid.path}}", State>
// never (compile error)
```

## Usage in Concerns

Pre-built concerns use interpolation for dynamic text:

```typescript
import { dynamicTooltip, dynamicLabel, dynamicPlaceholder } from 'apex-state'

// Register dynamic tooltip
store.useConcerns('legs.0.strike', {
  dynamicTooltip: {
    concern: dynamicTooltip,
    config: { template: "Current value: {{legs.0.strike}}" }
  }
})

// Read in component
const { dynamicTooltip } = store.useFieldConcerns('legs.0.strike')
// "Current value: 105"
```

## Files

- `src/utils/interpolation.ts` - Runtime functions
- `src/types/interpolation.ts` - Type utilities
