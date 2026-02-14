# INPUT v5: Record<Enum, ...> Support in DeepKey

## User Request

Check the HASH_KEY implementation. Allow for `Record<Enum, ...>` which should be correctly inferred in DeepKey. The idea is to limit the union type which can explode if we have an enum of 50 options with nested object for each of these.

## Problem

Given a type like:
```typescript
enum Status { Active = 'active', Inactive = 'inactive', Pending = 'pending' }
type State = { items: Record<Status, { name: string; count: number }> }
```

Current DeepKey would enumerate ALL enum keys as literal paths:
- `items.active.name`
- `items.active.count`
- `items.inactive.name`
- `items.inactive.count`
- `items.pending.name`
- `items.pending.count`

With 50 enum options × nested fields, this union explodes combinatorially.

## Goal

Support a collapsed/wildcard representation for enum-keyed records, similar to how HASH_KEY (`_()`) handles dynamic string keys, so the type system treats enum-indexed records as a single path group rather than expanding every combination.

## Deliverable

PoC exploring feasibility and approach.
