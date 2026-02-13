---
title: Initial Requirements (Legacy)
audience: historical reference
created: 2026-01-28 (2995319)
updated: 2026-01-29 (d667b86)
status: obsolete
superseded_by: docs/INPUT_v2.md
---

> **Legacy specification** – preserved for context only. The architecture described below predates the current two-proxy concern system. For current guidance see `docs/agents/ARCHITECTURE.md` and `docs/agents/CONCERNS_GUIDE.md`.

Goal: NPM package as valtio wrapper with side-effect extensions and simplicity improvements.

## Overview

- valtio
- zod for validation
- typescript for types
- react for hooks
- lodash for \_get and \_set operations
- deepDash for deep operations if needed

## Necessary hooks _(legacy proposal)_

`createGenericStore<DATA,META>()` function which takes type and initializes react context.

It returns:

- Provider --> react context, it accepts initialState of type DATA,
- useStore(path: DeepKey<DATA>) --> hook which returns [state, setState] similar to react's useState,
- useJitStore() --> Just-In-Time data access. It returns proxyValue: DATA (useSnapshot's returned), setChanges(changes: ArrayOfChanges<DATA,META>) --> void, getState() -> DATA (returns current state snapshot, not reactive).
- useSideEffects(id: string, effects: SideEffects<DATA>) --> this is a useLayoutEffect wrapper which registers and unregisters side effects for our store

Additionally, we want react-specific quality of life improvements:

- useFieldStore<VAL>(path: string) which returns {value: VAL, setValue: (newVal: VAL, meta?: META) => void} for easier form field management.
- useFieldTransformedStore<VAL, CTX>(path: string, {toTemporary: (val: VAL) => CTX, fromTemporary: (ctx: CTX) => VAL, context?: CTX}) which returns {value: CTX, setValue: (newCtx: CTX) => void} for easier form field management with transformations. The context is passed as second argument to toTemporary and fromTemporary functions.

Additionally, we want to store and provide defaultValue for components sometimes. First time the path is accessed, we should save it's value as defaultValue to separate Map or similar and provide in hook.

## Types _(legacy proposal)_

We will need following types:

- DeepKey<DATA> --> returns union of dot.path.style strings for given DATA type
- DeepValue<DATA, DeepKey<DATA>> --> returns actual value type for given path
- DeepKeyFiltered<DATA, TYPE> --> same as DeepKey, but only returns paths which resolve to TYPE
- GenericMeta {} with isSyncPathChange?: boolean, isFlipPathChange?: boolean, isProgramaticChange?: boolean, sender?: string which is base for META
- ArrayOfChanges<DATA, META> --> which is [DeepKey<DATA>, DeepValue<DATA, DeepKey<DATA>>, META][]
- PathsOfSameValue<DATA> --> which is Record<DeepKey<DATA>, Array<DeepKey<DATA>>> mapping each path to other paths which have same value (for sync changes)
- OnStateChangesListenerFunction<DATA, META> --> it is specified as {key: null|DeepKey<DATA>, fn: (changes: key===null? ArrayOfChanges<DATA,META> : ArrayOfChanges<DeepValue<key>, META>, currentState: key===null? DATA : DeepValue<DATA>) => void} which is called on state changes.

## Side-effects _(legacy proposal)_

As part of side-effects, we want to register couple of things (can be called multiple times at different parts of application).

- syncPaths,
- flipPaths,
- onStateChangesListeners.
- validators {scope: null|DeepKey<DATA>, schema: ZodSchema, path: DeepKey<DATA> } which return error message or void if valid.
- aggregation
- clearPaths

### Sync paths \*(legacy)

they work by taking pairs of paths which should always have same value. When one changes, the other is updated to same value automatically.
they should contruct a graph of dependencies on register/unregister for quick access. when one things change, all direct and indirect nodes should change.

### Flip paths \*(legacy)

they work by taking pairs of paths which should always have opposite boolean value. When one changes, the other is updated to previous value of the other one. we can assume only two options (enum/boolean).

### On state changes listeners \*(legacy)

they work by taking listener functions which are called on state changes. they can be scoped to specific path or global (null key).
these can listen on a nested paths, same as sync and flip paths. In case we listen on `some.value.path` and change `some.value` to `{path: {nested: 5}}`, the listener should be called with `some.value.path` change.
however! this break down of change should happen only in case necessary and should be "virtual" to trigger side-effects only. In the end, the state value should change as specified by the user + possible side-effects changes should be overlayed.

(we want to avoid the need for setting all nested properties to undefined when we want to clear them).

### Validators \*(legacy)

we use zod schemas to validate data on specific scope. when data in scope changes, we run validation and store possible error message to specified path.
keep in mind that given path can have multiple errors stored. we should register the errors with relevant unique ID as provided in registration and unregister them when validator is unregistered.

our hooks should return errors as array of strings for given path when requested. we should store these errors in specified path (ideally, createGenericStore should accept argument path of where errors should be stored).

### Aggregations \*(legacy)

Similarly to sync-paths, however, this works only one-way.
If all paths in sourcePaths have same value, we set targetPath to that value. If they differ, we set targetPath to undefined.
When targetPath changes, all sourcePaths are set to that value.
When any of sourcePaths change, we re-evaluate the aggregation.

Keep in mind that we might have multiple registrations for same targetPath with different sourcePaths. We should contruct a graph and validate that no cyclic dependencies are created.

### Clear paths \*(legacy)

this is a simple configuration on which paths should be cleared (set to undefined) when specific path changes.
When the specified path changes, all clearPaths are set to undefined. Also, when something changes INSIDE (should be configurable) changes, we also clear the specified paths.

## Synchronizers _(legacy)_

We need a way to pipe-like process the ArrayOfChanges before they are applied to the state.
We want everything to be applied at once together to avoid intermediate states and excessive re-renders.

## Derived _(legacy idea)_

> **Use today**: The concern + side-effect systems in `src/concerns` and `src/sideEffects` replace most of these bullet points. Treat this document as historical reasoning, not an implementation plan.

When passing initialObject OR setting objects to paths which include `get myValue(){ this.some.path + this.other.path }` we want these to be derived automatically. These have to be optimized! we should automatically wrap `this.some.path` and `this.other.path` accesses to track dependencies and re-evaluate only when necessary with valtio.
