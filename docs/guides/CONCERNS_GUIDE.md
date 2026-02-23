---
title: Concerns Agent Guide
updated: 2026-01-29
audience: contributors working on concerns
---

# Concerns System Guide

Read the shared workflow rules in `docs/guides/WORKFLOW_RULES.md` before touching this area.

Concerns are synchronous computations that keep UI state in sync with `store.state` by writing their results into `store._concerns`. Everything below focuses on how to build, register, and verify them without wading through redundant reminders.

## Concerns in 90 Seconds

| What                         | Why it matters                                                                               |
| ---------------------------- | -------------------------------------------------------------------------------------------- |
| Two proxies                  | Read from `store.state`, write to `store._concerns` to avoid feedback loops.                 |
| `effect()`                   | `valtio-reactive` tracks every deep read during `evaluate`, so re-runs happen automatically. |
| Pure evaluate                | No mutations, no async. Concerns only compute values.                                        |
| Registration handles cleanup | `registerConcerns()` wires effect + disposers; always return the cleanup it hands you.       |

Reference implementations live in `src/concerns/registration.ts` and `src/concerns/prebuilts/`.

## Quickstart

```ts
// src/concerns/prebuilts/example.ts
import type { ConcernType } from "../types"

export const exampleConcern: ConcernType<{ threshold: number }, boolean> = {
  name: "aboveThreshold",
  description: "True when value exceeds configured threshold",
  evaluate: ({ state, path, config }) => state[path] > config.threshold,
}

// usage in React component
store.useConcerns("legs.0.strike", ({ register }) => register(exampleConcern, { threshold: 100 }))

const { aboveThreshold } = store.useFieldConcerns("legs.0.strike")
```

Lifecycle:

1. Define concern (pure evaluate, typed config).
2. Add to `src/concerns/index.ts` if it ships as a default.
3. Register via `store.useConcerns(path, fn)`.
4. Read results with `store.useFieldConcerns(path)`.
5. Cover behavior with concern/unit tests + integration tests.

## Built-in Concerns

| Concern                                                  | Returns                 | Primary dependency          | Notes                                                             |
| -------------------------------------------------------- | ----------------------- | --------------------------- | ----------------------------------------------------------------- |
| `validationState`                                        | `ValidationStateResult` | Zod schema + optional scope | Aggregates errors; see tests under `tests/concerns/validation`.   |
| `disabledWhen` / `visibleWhen` / `readonlyWhen`          | `boolean`               | BoolLogic tree              | See `src/utils/boolLogic.ts` for available operators.             |
| `dynamicTooltip` / `dynamicLabel` / `dynamicPlaceholder` | `string`                | Interpolation template      | Uses `interpolateTemplate`; references are tracked automatically. |
| `prefillValue` (if registered)                           | `unknown`               | explicit config             | Example of pure compute without BoolLogic.                        |

Treat the source files as documentation; this table is just a routing map.

## Key Files

| File                           | Role                                                           |
| ------------------------------ | -------------------------------------------------------------- |
| `src/concerns/registration.ts` | Wraps concern evaluation with `effect()` and provides cleanup. |
| `src/concerns/types.ts`        | Source of `ConcernType` definitions.                           |
| `src/concerns/index.ts`        | Default concern registry.                                      |
| `src/concerns/registry.ts`     | Lookup by concern name.                                        |
| `src/utils/bool-logic.ts`      | BoolLogic evaluator used by conditional concerns.              |
| `src/utils/interpolation.ts`   | Template interpolation for dynamic text concerns.              |

## Dependency Tracking Primer

`effect()` captures every property access on `state` during `evaluate()`. Use helpers like `deepGet` or plain property reads; both are tracked. Writes go exclusively to `_concerns`. If you need to read another concern's output, do so through BoolLogic conditions on state paths rather than touching `_concerns` directly. Detailed reasoning lives in `docs/guides/ARCHITECTURE.md` and `CONCERNS_REFERENCE.md`.

## Testing

- Unit tests: start from `tests/concerns/*.test.ts` (one per concern).
- Integration: see `tests/integration/concerns-*.test.tsx` for React wiring.
- Utilities: `tests/concerns/test-utils.ts` provides evaluators and spies.

## Working Checklist

| Build It Right                                | Debug It Fast                                   |
| --------------------------------------------- | ----------------------------------------------- |
| `evaluate` is pure + synchronous              | Registered in `defaultConcerns` (if default)    |
| Config typed via `ConcernType<{...}, Return>` | `store.useConcerns` invoked before reading      |
| Added to registry when shipping default       | Cleanup from `useConcerns` executes             |
| Tests cover happy + failure paths             | Dependency actually changes (watch the tracker) |
| Documentation updated only where needed       | Return type matches `ConcernType` declaration   |

Stick to these checkpoints and the system self-heals: dependency tracking handles the rest.
