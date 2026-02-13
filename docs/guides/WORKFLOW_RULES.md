---
title: Shared Agent Workflow Rules
audience: contributors, automation
created: 2026-01-29 (d667b86)
updated: 2026-02-05 (c2c957e)
status: active
---

# Shared Agent Workflow Rules

These rules apply to every task in this repository. Link here instead of restating them in individual guides.

## Minimal-Change Doctrine

- Make only the edits the user explicitly requested.
- If something bigger is required, finish the task, then call it out in the handoff.
- Never restructure, rename, or refactor without written approval.

## Post-Change Ritual

```bash
npm run code:fix
```

- Run after every code change; it applies ESLint + Prettier.
- Do not read or copy the formatter output back into chat.

## Source of Truth

- Treat implementation files in `src/` as canonical.
- Use existing patterns and TSDoc before inventing new abstractions.
- Prefer characterization tests before modifying legacy behavior.

## Communication

- Record significant discoveries in semantic memory.
- Surface blockers immediately instead of waiting for the next update.

Keep this file lean. If a new global rule appears, add it here and delete duplicates elsewhere.
