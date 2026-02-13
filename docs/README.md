---
title: Documentation Index
audience: all contributors
created: 2026-01-29 (d667b86)
updated: 2026-02-05 (c2c957e)
status: active
---

# Documentation Index

Use this index to pick the right reference quickly. Each entry links to the canonical Markdown file and calls out the intended reader.

## Active Guides

| Document                        | Audience                   | Why you read it                                                                       |
| ------------------------------- | -------------------------- | ------------------------------------------------------------------------------------- |
| `docs/guides/WORKFLOW_RULES.md` | Everyone touching the repo | Shared editing rules: minimal changes, formatting ritual, communication expectations. |
| `docs/guides/ARCHITECTURE.md`   | Store/concerns maintainers | Core data flow, invariants, and pointers for deeper dives.                            |
| `docs/guides/CONCERNS_GUIDE.md` | Concern authors            | Lifecycle, quickstart, built-in concern map, testing checklist.                       |
| `docs/guides/STORE_HOOKS.md`    | Hook maintainers           | Hook atlas (store + composable), dependency chain, testing pointers.                  |
| `docs/SIDE_EFFECTS_GUIDE.md`    | Side-effect authors        | Sync paths, flip paths, aggregations, listeners API and examples.                     |
| `docs/DEBUG_TIMING.md`          | Performance debugging      | DebugConfig, timing utilities, slow operation detection.                              |

## Expert Agent Prompts

Subagent prompts for autonomous work in specific areas. Load these as context when spawning specialized agents.

| Document                            | Domain                  | What the agent knows                                                                   |
| ----------------------------------- | ----------------------- | -------------------------------------------------------------------------------------- |
| `docs/agents/expert-concerns.md`    | Concerns system         | `effect()` wrapping, prebuilt concerns, BoolLogic, interpolation, registration flow.   |
| `docs/agents/expert-pipeline.md`    | Pipeline & side effects | `processChanges`, PathGroups, sync/flip/aggregation/listeners, single-pass processing. |
| `docs/agents/expert-store-hooks.md` | Store & hooks           | Provider, store instance, all store hooks, composable field hooks, public API.         |
| `docs/agents/expert-testing.md`     | Test suite              | `flushEffects`, fixtures, `typeHelpers`, test patterns, directory map, commands.       |
| `docs/agents/expert-architect.md`   | Architecture            | Orchestration, trade-off analysis, invariant enforcement, specialist routing.          |

## Feature References

| Document                             | Audience                        | Why you read it                                                                      |
| ------------------------------------ | ------------------------------- | ------------------------------------------------------------------------------------ |
| `docs/INTERPOLATION.md`              | Concern + side-effect authors   | Runtime + type helpers for template interpolation.                                   |
| `docs/USEFIELDCONCERNS_API.md`       | Frontend consumers              | How to read concern results from React components.                                   |
| `docs/GRAPHOLOGY_REPLACEMENT.md`     | Maintainers                     | PathGroups data structure replacing graphology for O(1) connected component lookups. |
| `docs/WILDCARD_UTILITIES_EXAMPLE.md` | Users with Record types         | Examples: `toWildcardPath()`, `toWildcardPathAuto()`.                                |
| `docs/WILD_FUNCTION_GUIDE.md`        | Users with Record types         | `Wild()` inline wildcard template string utility.                                    |
| `docs/IMPLEMENTATION_COMPLETE.md`    | Record support reference        | Full Record type support with `[*]` wildcard notation.                               |
| `docs/RECORD_MIGRATION.md`           | Users migrating to Record types | Migration patterns for `Record<string, V>` types.                                    |

## Testing & QA

| Document                          | Audience                  | Why you read it                                           |
| --------------------------------- | ------------------------- | --------------------------------------------------------- |
| `docs/WRITING_TESTS.md`           | Contributors              | Test patterns, setup, assertions.                         |
| `docs/TEST_SCENARIOS.md`          | QA / regression reviewers | Detailed P0 test scenarios and edge cases.                |
| `docs/TEST_CASES_INVENTORY.md`    | QA / regression reviewers | Complete test map organized by directory.                 |
| `docs/P0_TEST_COVERAGE_REPORT.md` | QA / regression reviewers | Snapshot of P0 coverage and scenarios; rerun guidance.    |
| `docs/REFACTORING_EXAMPLE.md`     | Contributors              | Before/after patterns for converting tests to real store. |

## Decision Records & Analysis

| Document                                 | Audience          | Why you read it                                                           |
| ---------------------------------------- | ----------------- | ------------------------------------------------------------------------- |
| `docs/ARCHITECTURE_CONCERNS_CRITICAL.md` | Core maintainers  | Deep analysis of valtio execution order and why two-proxy pattern exists. |
| `docs/VALTIO_REACTIVE_ANALYSIS.md`       | Core maintainers  | Technical decision document for valtio-reactive adoption.                 |
| `docs/DOCS_AUDIT.md`                     | Meta/housekeeping | Tracks what was deleted, updated, and moved during doc cleanup.           |

## Original Requirements (Historical)

| Document           | Audience           | Why you read it                                  |
| ------------------ | ------------------ | ------------------------------------------------ |
| `docs/INPUT.md`    | Historical context | Original requirements specification.             |
| `docs/INPUT_v2.md` | Historical context | Requirements addendum: BoolLogic, interpolation. |
| `docs/INPUT_v3.md` | Historical context | Requirements addendum: aggregations testing.     |

---

If you add or retire documentation, update this list so the next person can find it without spelunking the tree.
