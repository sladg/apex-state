---
title: Store Hook Guide
updated: 2026-01-29
audience: contributors working on store hooks
---

# Store Hooks Reference

Look at `docs/agents/WORKFLOW_RULES.md` for the global editing contract. This guide maps each hook to its responsibilities so you can jump straight to the right code and tests.

## Hook Atlas

| Hook                                        | Returns                                | Primary use                       | Notes                                                      |
| ------------------------------------------- | -------------------------------------- | --------------------------------- | ---------------------------------------------------------- |
| `useStore(path)`                            | `[value, setValue]`                    | Basic read/write                  | Setter pipes through `processChanges` for batching.        |
| `useFieldStore(path)`                       | `{ value, setValue }`                  | Form fields needing object API    | Captures default values automatically.                     |
| `useFieldTransformedStore(path, transform)` | `{ value, setValue }`                  | Format conversion (dates, units)  | Keeps canonical value in state; transform only touches UI. |
| `useJitStore()`                             | `{ proxyValue, setChanges, getState }` | Bulk updates / non-reactive reads | `setChanges` accepts ArrayOfChanges.                       |
| `useConcerns(path, register)`               | `void`                                 | Register concern definitions      | Returns cleanup that unregisters effects.                  |
| `useFieldConcerns(path)`                    | `EvaluatedConcerns`                    | Read concern results              | Reads from `_concerns` proxy snapshot.                     |
| `useSideEffects(id, config)`                | `void`                                 | Register side-effects graph       | Delegates to `src/sideEffects/registration.ts`.            |

All implementations live in `src/store/createStore.ts` with TSDoc that stays current with the code.

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

If you need the underlying logic, start with `src/store/Provider.tsx`, then trace through `src/store/executor.ts` for setters, and `docs/agents/ARCHITECTURE.md` for broader context.

## Patterns In Practice

- **Validation workflows**: `useConcerns` + `useFieldConcerns` + `validationState` concern. Integration tests: `tests/integration/form-validation.test.tsx`.
- **Conditional UI / BoolLogic**: `disabledWhen` + `visibleWhen`. See `tests/integration/concerns-ui.test.tsx`.
- **Bulk imports**: `useJitStore().setChanges` for loading server payloads. Covered in `tests/integration/bulk-updates.test.tsx`.
- **Transformers**: `useFieldTransformedStore` keeps inputs human-friendly. Tests under `tests/integration/field-transformers.test.tsx`.

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
