---
title: P0 Test Coverage Snapshot
updated: 2026-01-29
audience: QA, maintainers
---

# P0 Suite Coverage

The P0 regression suite covers every critical concern scenario. All tests pass as of 2026-01-29.

| Scenario                            | Test file                                   | What it verifies                                              | Perf target                                           |
| ----------------------------------- | ------------------------------------------- | ------------------------------------------------------------- | ----------------------------------------------------- |
| TEST-001 – Selective Re-calculation | `tests/concerns/selective-recalc.test.ts`   | Only touched concerns re-evaluate; others stay idle.          | < 15 ms per update (adjusted for `effect()` overhead) |
| TEST-002 – Cross-Field Dependencies | `tests/concerns/cross-field-deps.test.ts`   | BoolLogic precision and isolation between legs.               | < 12 ms single concern                                |
| TEST-003 – Batch Updates            | `tests/concerns/batch-updates.test.ts`      | Batched updates evaluate once and land final state correctly. | < 30 ms end-to-end                                    |
| TEST-007 – React Integration        | `tests/concerns/react-integration.test.tsx` | React 18 batching + UI stays in sync with concerns.           | < 16 ms render                                        |

**Suite status:** 19/19 tests passing · Duration ≈ 0.4 s (Vitest).

## When to Re-run

Run the suite whenever you change:

- Concern evaluation logic or registration plumbing
- Side-effect executor behavior
- Store hooks that call `processChanges`

Command:

```bash
npm test -- tests/concerns/selective-recalc.test.ts \
  tests/concerns/cross-field-deps.test.ts \
  tests/concerns/batch-updates.test.ts \
  tests/concerns/react-integration.test.tsx
```

## Infrastructure Notes

- Utilities live in `tests/concerns/test-utils.ts` (performance harness, concern trackers, render spies).
- Performance thresholds include ~10 ms overhead from `valtio-reactive` scheduling; keep this in mind when evaluating regressions.

No coverage gaps identified. If a new P0 scenario emerges, add it to the table above and extend the suite accordingly.
