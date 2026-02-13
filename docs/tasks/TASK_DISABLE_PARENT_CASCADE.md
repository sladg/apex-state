---
created: unstaged
updated: unstaged
status: active
---

# Task: Disable Parent Cascade

**Priority**: P3
**Type**: Feature
**Effort**: High (architecture decision needed)

## Goal

When a parent path is disabled (via `disabledWhen`), child paths should inherit the disabled state.

Example: Disabling `form` should also disable `form.name`, `form.email`, etc.

## Current state

`disabledWhen` (`src/concerns/prebuilts/disabledWhen.ts`) evaluates BoolLogic per individual path. No parent-child propagation exists.

## Options (need design decision)

### Option A: Consumer-side composition (no lib changes)

The consumer uses wildcard paths to achieve this manually:

```typescript
concerns: {
  'form': { disabledWhen: { condition: { IN: ['LOCKED'] } } },
  'form.[*]': { disabledWhen: { condition: { IN: ['LOCKED'] } } },
}
```

**Pros**: No lib changes, explicit, no magic
**Cons**: Verbose, easy to forget, must duplicate conditions

### Option B: Built-in cascade flag

Add `cascade: true` to concern config:

```typescript
concerns: {
  'form': { disabledWhen: { condition: { IN: ['LOCKED'] }, cascade: true } },
}
```

The concern system auto-applies the result to all child paths.

**Pros**: Clean API, hard to forget
**Cons**: Adds complexity to concern resolution, needs careful ordering

### Option C: Hook-level resolution

`useFieldConcerns('form.name')` checks both the field's own concerns AND all ancestor paths. Resolution happens at read time, not registration time.

**Pros**: No extra registration, works automatically
**Cons**: Performance cost on every hook read, implicit behavior

## Key files

- `src/concerns/prebuilts/disabledWhen.ts` - current implementation
- `src/concerns/registration.ts` - concern registration
- `src/store/createStore.ts` - hooks that read concerns

## Acceptance criteria (TBD after design decision)

- Disabling a parent path disables all children
- Explicit child override can still enable (opt-out)
- Existing tests still pass
