# _() — Hash Key Paths for Record Types

The `_()` utility marks concrete IDs as hash keys in template string paths, enabling type-safe access to `Record<string, V>` fields.

## Import

```typescript
import { _, hashKey } from '@sladg/apex-state'
```

## How It Works

`_()` takes a concrete ID and returns it typed as `HASH_KEY`. The runtime value is unchanged — the function exists purely for the type system.

Without `_()`, TypeScript rejects the path because `Record<string, V>` keys aren't part of the static `DeepKey<T>` union. `_()` bridges this gap.

## Usage

See `examples/wildcard-paths.ts` for complete examples of:

- Single hash key paths with `_()`
- Deeply nested Record paths with multiple `_()` calls
- Using hash key paths with `useConcerns`

## hashKey Namespace

```typescript
import { hashKey } from '@sladg/apex-state'

// Reject paths containing [*] at runtime
hashKey.rejectDynamic('users.u1.posts.p1')  // OK
hashKey.rejectDynamic('users.[*].posts')     // throws Error
```

## When to Use

- Building paths for `Record<string, V>` typed fields
- Constructing paths inline in template strings
- Registering concerns or side effects for Record paths

## Source

- `src/utils/hash-key.ts`
