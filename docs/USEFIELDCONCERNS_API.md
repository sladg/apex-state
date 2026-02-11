---
title: withConcerns API
updated: 2026-02-05
audience: frontend consumers of concern results
---

# `store.withConcerns` API

`withConcerns` creates a scoped `useFieldStore` hook that includes selected concern values alongside the field's `value` and `setValue`. Pair it with `store.useConcerns` to register computations; `withConcerns` does the reading.

## Location

- Implementation: `src/store/createStore.ts` (`withConcerns` export)
- Types: `EvaluatedConcerns` in `src/types/concerns.ts`

## Signature

```ts
store.withConcerns<SELECTION extends Partial<Record<string, boolean>>>(
  selection: SELECTION,
): { useFieldStore: <P extends DeepKey<DATA>>(path: P) => { value, setValue, ...selectedConcerns } }
```

### Parameters

- `selection`: An object mapping concern names to `true` to include them. E.g. `{ validationState: true, disabledWhen: true }`.

### Returns

An object with a `useFieldStore` hook. When called with a path, it returns:

- `value`: The current field value (`DeepValue<DATA, P>`)
- `setValue`: Setter function
- Plus each selected concern value as a property (if registered for that path)

```ts
type ConcernValues = {
  validationState?: ValidationStateResult;
  disabledWhen?: boolean;
  visibleWhen?: boolean;
  dynamicTooltip?: string;
  // ...plus any custom concerns you register
};
```

When no concerns exist for the path, concern properties are `undefined`.

## How It Works

1. Calls `_useFieldValue(path)` to get `value` and `setValue` from store context.
2. Subscribes to `store._concerns` with `useSnapshot` from Valtio.
3. Filters concern values based on the `selection` keys.
4. Returns merged object with field value, setter, and selected concerns.

Because concern evaluation writes into `_concerns`, React components re-render only when the relevant concern data changes.

## Usage

```tsx
const { useFieldStore } = store.withConcerns({
  validationState: true,
  disabledWhen: true,
  dynamicTooltip: true,
})

const Component = () => {
  store.useConcerns("form.concerns", {
    email: {
      validationState: { schema: emailSchema },
      dynamicTooltip: { template: "Current value: {{email}}" },
    },
  });

  const email = useFieldStore("email");

  return (
    <label>
      <input
        type="email"
        className={email.validationState?.isValid ? "" : "error"}
        disabled={email.disabledWhen === true}
        placeholder={email.dynamicTooltip}
        value={email.value}
        onChange={(e) => email.setValue(e.target.value)}
      />
      {email.validationState?.message && (
        <span role="alert">{email.validationState.message}</span>
      )}
    </label>
  );
};
```

## Related Hooks

- `store.useConcerns(id, registration, customConcerns?)` — Registers the concerns that populate `_concerns`.
- `store.useStore(path)` / `store.useFieldStore(path)` — Read/write the underlying field value (without concerns).

## Test Coverage

- React wiring: `tests/concerns/react-integration.test.tsx`
- Concern scenarios: `tests/integration/concerns-ui.test.tsx`
- Registration/reader interplay: `tests/concerns/selective-recalc.test.ts`

Use these specs when validating new concern outputs or debugging memoization issues.
