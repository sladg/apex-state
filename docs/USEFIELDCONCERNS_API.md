---
title: useFieldConcerns Hook
updated: 2026-01-29
audience: frontend consumers of concern results
---

# `store.useFieldConcerns` API

`useFieldConcerns` exposes the computed concern outputs for a given path on the store instance created by `createGenericStore`. Pair it with `store.useConcerns` to register computations; this hook does the reading.

## Location

- Implementation: `src/store/createStore.ts` (`useFieldConcerns` export)
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

An object whose optional keys align with the concerns registered for that path. Example shape when using the built-ins:

```ts
type ConcernValues = {
  validationState?: ValidationStateResult;
  disabledWhen?: boolean;
  visibleWhen?: boolean;
  dynamicTooltip?: string;
  // ...plus any custom concerns you register
};
```

When no concerns exist for the path the hook returns an empty object (`{}`), so defensive null checks are unnecessary.

## How It Works

1. Reads the store instance from context via `useStoreContext()`.
2. Subscribes to `store._concerns` with `useSnapshot` from Valtio.
3. Returns the value at `path`, cast to `EvaluatedConcerns<CONCERNS>`.

Because concern evaluation writes into `_concerns`, React components re-render only when the relevant concern data changes.

## Usage

```tsx
const Component = () => {
  store.useConcerns("form.concerns", {
    email: {
      validationState: {
        concern: validationState,
        config: { schema: emailSchema },
      },
      dynamicTooltip: {
        concern: dynamicTooltip,
        config: { template: "Current value: {{email}}" },
      },
    },
  });

  const emailConcerns = store.useFieldConcerns("email");

  return (
    <label>
      <input
        type="email"
        className={emailConcerns.validationState?.isValid ? "" : "error"}
        disabled={emailConcerns.disabledWhen === true}
        placeholder={emailConcerns.dynamicTooltip}
      />
      {emailConcerns.validationState?.message && (
        <span role="alert">{emailConcerns.validationState.message}</span>
      )}
    </label>
  );
};
```

## Related Hooks

- `store.useConcerns(id, registration, customConcerns?)` — Registers the concerns that populate `_concerns`.
- `store.useStore(path)` / `store.useFieldStore(path)` — Read/write the underlying field value.
- `store.withConcerns(selection)` — Compose concern values directly into `useFieldStore` results when you want a single call site.

## Test Coverage

- React wiring: `tests/concerns/react-integration.test.tsx`
- Concern scenarios: `tests/integration/concerns-ui.test.tsx`
- Registration/reader interplay: `tests/concerns/selective-recalc.test.ts`

Use these specs when validating new concern outputs or debugging memoization issues.
