# `store.useFieldConcerns` API

`useFieldConcerns` exposes the computed concern outputs for a given path. Pair it with `store.useConcerns` to register computations; this hook does the reading.

## Source

- Implementation: `src/store/create-store.ts`
- Types: `EvaluatedConcerns` in `src/concerns/types.ts`

## Signature

```ts
store.useFieldConcerns<P extends DeepKey<DATA>>(
  path: P,
): EvaluatedConcerns<CONCERNS>
```

### Parameters

- `path`: Type-safe field path (e.g. `'user.email'`, `'legs.0.strike'`).

### Returns

An object whose optional keys align with the concerns registered for that path:

```ts
type ConcernValues = {
  validationState?: ValidationStateResult;
  disabledWhen?: boolean;
  visibleWhen?: boolean;
  dynamicTooltip?: string;
  // ...plus any custom concerns you register
};
```

When no concerns exist for the path the hook returns `{}`.

## How It Works

1. Reads the store instance from context via `useStoreContext()`.
2. Subscribes to `store._concerns` with `useSnapshot` from Valtio.
3. Returns the value at `path`, cast to `EvaluatedConcerns<CONCERNS>`.

Re-renders only when the relevant concern data changes.

## Usage

See `examples/concerns.tsx` for a complete example of registering validation, BoolLogic, and dynamic text concerns, then reading results with `useFieldConcerns`.

See `examples/basic-store.tsx` for an end-to-end example combining concerns with side effects and Provider.

## Related Hooks

- `store.useConcerns(id, registration)` — Registers the concerns that populate `_concerns`.
- `store.useStore(path)` / `store.useFieldStore(path)` — Read/write the underlying field value.
- `store.withConcerns(selection)` — Compose concern values directly into `useFieldStore` results.

## Tests

- React wiring: `tests/concerns/react-integration.test.tsx`
- Concern scenarios: `tests/integration/concerns-ui.test.tsx`
- Registration/reader interplay: `tests/concerns/selective-recalc.test.ts`
