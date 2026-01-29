---
title: Documentation Index
updated: 2026-01-29
audience: all contributors
---

# Documentation Index

Use this index to pick the right reference quickly. Each entry links to the canonical Markdown file and calls out the intended reader.

| Document                             | Audience                      | Why you read it                                                                       |
| ------------------------------------ | ----------------------------- | ------------------------------------------------------------------------------------- |
| `docs/agents/WORKFLOW_RULES.md`      | Everyone touching the repo    | Shared editing rules: minimal changes, formatting ritual, communication expectations. |
| `docs/agents/ARCHITECTURE.md`        | Store/concerns maintainers    | Core data flow, invariants, and pointers for deeper dives.                            |
| `docs/agents/CONCERNS_GUIDE.md`      | Concern authors               | Lifecycle, quickstart, built-in concern map, testing checklist.                       |
| `docs/agents/STORE_HOOKS.md`         | Hook maintainers              | Hook atlas, dependency chain, testing pointers.                                       |
| `docs/INTERPOLATION.md`              | Concern + side-effect authors | Runtime + type helpers for template interpolation.                                    |
| `docs/P0_TEST_COVERAGE_REPORT.md`    | QA / regression reviewers     | Snapshot of P0 coverage and scenarios; rerun guidance.                                |
| `docs/USEFIELDCONCERNS_API.md`       | Frontend consumers            | How to read concern results from React components.                                    |
| `docs/INPUT.md` / `docs/INPUT_v2.md` | Historical context            | Early requirements; marked as legacy for reference only.                              |

If you add or retire documentation, update this list so the next person can find it without spelunking the tree.
