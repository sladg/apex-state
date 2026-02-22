---
title: Documentation Index
updated: 2026-02-22
audience: all contributors
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

| Document                       | Audience                      | Why you read it                                        |
| ------------------------------ | ----------------------------- | ------------------------------------------------------ |
| `docs/INTERPOLATION.md`        | Concern + side-effect authors | Runtime + type helpers for template interpolation.     |
| `docs/USEFIELDCONCERNS_API.md` | Frontend consumers            | How to read concern results from React components.     |
| `docs/WILD_FUNCTION_GUIDE.md`  | Users with Record types       | `_()` hash key utility for Record path type safety.    |
| `docs/TESTING_MOCK.md`         | Consumer test authors         | `@sladg/apex-state/testing` mock setup and API.        |
| `docs/CLEAR_PATHS.md`          | Side-effect authors           | ClearPaths side effect architecture and implementation. |

## Testing

| Document                | Audience     | Why you read it                    |
| ----------------------- | ------------ | ---------------------------------- |
| `docs/WRITING_TESTS.md` | Contributors | Test patterns, setup, assertions. |

## Proposed Features

| Document                    | Audience         | Why you read it                                           |
| --------------------------- | ---------------- | --------------------------------------------------------- |
| `docs/VALUE_LOGIC_ENGINE.md` | Core maintainers | Proposed ValueLogic engine for conditional value selection. |
| `docs/FORMULA_ENGINE.md`    | Core maintainers | Proposed formula engine for computed expressions.          |

## WASM Architecture

| Document                    | Audience         | Why you read it                                       |
| --------------------------- | ---------------- | ----------------------------------------------------- |
| `docs/WASM_ARCHITECTURE.md` | Core maintainers | JS/WASM boundary spec, pipeline, ownership, data flow. |

## Original Requirements (Historical)

| Document           | Audience           | Why you read it                                  |
| ------------------ | ------------------ | ------------------------------------------------ |
| `docs/INPUT.md`    | Historical context | Original requirements specification.             |
| `docs/INPUT_v2.md` | Historical context | Requirements addendum: BoolLogic, interpolation. |
| `docs/INPUT_v3.md` | Historical context | Requirements addendum: aggregations testing.     |

---

If you add or retire documentation, update this list so the next person can find it without spelunking the tree.
