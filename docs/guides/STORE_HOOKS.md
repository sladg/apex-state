---
title: Store Hook Guide
audience: contributors working on store hooks
created: 2026-02-05 (c2c957e)
updated: 2026-02-11 (bd07f7d)
status: active
---

# Store Hooks Reference

Look at `docs/guides/WORKFLOW_RULES.md` for the global editing contract. This guide maps each hook to its responsibilities so you can jump straight to the right code and tests.

## Hook Atlas

### Store Hooks

Returned by `createGenericStore()`. All implementations live in `src/store/createStore.ts`.

| Hook                            | Returns                                | Primary use                       | Notes                                                       |
| ------------------------------- | -------------------------------------- | --------------------------------- | ----------------------------------------------------------- |
| `useStore(path)`                | `[value, setValue]`                    | Basic read/write                  | Setter pipes through `processChanges` for batching.         |
| `useFieldStore(path)`           | `{ value, setValue }`                  | Form fields needing object API    | Captures default values automatically.                      |
| `useJitStore()`                 | `{ proxyValue, setChanges, getState }` | Bulk updates / non-reactive reads | `setChanges` accepts ArrayOfChanges.                        |
| `useConcerns(id, registration)` | `void`                                 | Register concern definitions      | Returns cleanup that unregisters effects.                   |
| `useSideEffects(id, config)`    | `void`                                 | Register side-effects graph       | Delegates to `src/sideEffects/registration.ts`.             |
| `withConcerns(selection)`       | `{ useFieldStore }`                    | Field + selected concerns in one  | Returns a scoped `useFieldStore` with concern props merged. |

### Composable Field Hooks

Standalone hooks exported from `src/hooks/`. They accept any object with `{ value, setValue }` and compose with each other.

| Hook                                       | Returns                                                         | Primary use                         | Notes                                                       |
| ------------------------------------------ | --------------------------------------------------------------- | ----------------------------------- | ----------------------------------------------------------- |
| `useBufferedField(field)`                  | `{ value, setValue, commit, cancel, isDirty }`                  | Hold edits locally until commit     | Wraps any `{ value, setValue }` field.                      |
| `useThrottledField(field, { ms })`         | Same shape as input field                                       | Rate-limit `setValue` calls         | First call immediate, subsequent buffered. Last value wins. |
| `useTransformedField(field, { to, from })` | `{ value: TDisplay, setValue: (v: TDisplay) => void, ...rest }` | Format conversion (dates, currency) | Passes through extra props (e.g. `commit`, `isDirty`).      |
| `useKeyboardSelect(field, { options })`    | Input field + `{ onKeyDown }`                                   | Type-ahead selection                | Accumulates keystrokes with configurable debounce.          |

**Composition example:**

```ts
const raw = store.useFieldStore("price")
const buffered = useBufferedField(raw)
const formatted = useTransformedField(buffered, {
  to: (cents) => (cents / 100).toFixed(2),
  from: (dollars) => Math.round(parseFloat(dollars) * 100),
})
// formatted has: value, setValue, commit, cancel, isDirty
```

## Dependency Chain

```
Provider → createStore factory
  └─ store context (StoreInstance)
       ├─ state proxy (user data)
       ├─ _concerns proxy (computed outputs)
       └─ _internal ref (graphs, bookkeeping)
            ↓
Hooks pull from context → useSnapshot(store.state or store._concerns)
Setters funnel into processChanges() → side-effects → concerns re-run
```

If you need the underlying logic, start with `src/store/Provider.tsx`, then trace through `src/store/executor.ts` for setters, and `docs/guides/ARCHITECTURE.md` for broader context.

## Patterns In Practice

- **Validation workflows**: `useConcerns` + `withConcerns` + `validationState` concern. Integration tests: `tests/integration/form-validation.test.tsx`.
- **Conditional UI / BoolLogic**: `disabledWhen` + `visibleWhen`. See `tests/integration/concerns-ui.test.tsx`.
- **Bulk imports**: `useJitStore().setChanges` for loading server payloads. Covered in `tests/integration/bulk-updates.test.tsx`.
- **Composable hooks**: `useBufferedField`, `useThrottledField`, `useTransformedField`, `useKeyboardSelect`. Tests under `tests/hooks/`.

## Type Safety Cheatsheet

- Paths: `DeepKey<DATA>` from `src/types/deepKey.ts`.
- Values: `DeepValue<DATA, Path>` from `src/types/deepValue.ts`.
- Side effects: `SideEffects<DATA>` in `src/types/sideEffects.ts`.

Keep hooks generic; rely on these utilities instead of manual casts.

## Testing Hooks

| Scenario               | Where to look                                        |
| ---------------------- | ---------------------------------------------------- |
| Hook unit helpers      | `tests/mocks/` + `tests/concerns/test-utils.ts`      |
| React integration      | `tests/integration/*.test.tsx` (search by hook name) |
| Performance + batching | `tests/concerns/batch-updates.test.ts`               |

When adding features, extend the integration spec that already exercises similar behavior.

## Checklist

| Ship It                                      | Debug It                                         |
| -------------------------------------------- | ------------------------------------------------ |
| Hook signature unchanged (public API)        | Provider wraps the component tree                |
| Setter returns cleanup or noop appropriately | Path resolves per `DeepKey`                      |
| New behavior covered by tests                | `useConcerns` registered before reading concerns |
| Transform hooks keep calculations pure       | Snapshots update when expected                   |

Stick to the table above, keep changes minimal, and the store surface stays predictable.
