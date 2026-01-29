---
title: Requirements Addendum (Legacy)
updated: 2026-01-29
audience: historical reference
status: legacy
---

> **Legacy addendum** – kept for traceability. Current implementations have dedicated documentation in `docs/agents/ARCHITECTURE.md`, `docs/agents/CONCERNS_GUIDE.md`, and `docs/INTERPOLATION.md`.

Extensions and additions:

Ability to specify `options` for DeepKey<DATA> paths, which lead to strings/enums.

We want an generic ability to pass extra data to certain fields. These can be things like isDisabled: Boolean | BoolLogic<DATA>, options: Array<DeepValue<DeepKey<DATA, PATH>>>[], etc.
This would allow for more dynamic and flexible configurations, especially when dealing with forms or UI components that require additional metadata.

These "common" things should be available on opt-in basis unless no performance impact. We should automatically allow for BoolLogic definitions which automatically convert into true/false for component so that we can have dynamic enabling/disabling of fields based on other data points.

The components consuming the provided hooks should be able to focus solely on rendering logic, while the hooks handle the complexity of data retrieval and option management.

I would also like for options (if applicable) to be passed with BoolLogic support so that we can have dynamic option lists based on other data points.

BoolLogic type is a nested object with generic DATA. It allows combining and nesting things like these:

- IS_EMPTY: DeepKeyFiltered<DATA, object|unknown[]>
- EXISTS: DeepKey<DATA>
- AND: BollLogic<DATA>[]
- OR: BoolLogic<DATA>[]
- IN: [DeepKey<DATA>, DeepValue<DATA, DeepKey<DATA, any>>[]]
- every
- length_gt
- contains
- etc.

---

Interpolation, where strings can contain placeholders that reference other data points within the DATA structure. For example, a string like "Hello, {user.name}!" would dynamically replace "{user.name}" with the actual name from the DATA object. This feature enhances flexibility and allows for more dynamic content generation based on the underlying data. This should be typed (i'm okay with splitting interpolated bits one-by-one if needed for type safety).

Again, i would like components to focus on rendering logic while hooks handle interpolation.

---

**Modern mapping:**

- BoolLogic support now lives in `src/utils/boolLogic.ts` and the prebuilt concerns referenced in `docs/agents/CONCERNS_GUIDE.md`.
- Interpolation requirements are satisfied by `src/utils/interpolation.ts`; see `docs/INTERPOLATION.md`.
- `$LEG_ONE`-style dynamic segments inspired the current scoped registration system (`store.useConcerns`).
- Undo/history ideas remain out of scope; track future decisions separately.

Treat the remaining bullet points as historical context for why the current system exists, not as open tasks.

---

3 - this is valid, we should prefer a specific paths tho, i don't like the asterix paths as it won't be type-safe. We can assume that strategies will have paths such as p.$LEG_ONE.data.optionsCommon.strike. It will be hard-typed and replaced on fly so that $LEG_ONE matches the uuid used in store's data.

4- let's ignore virtualization and keyboard shortcuts, let's keep them outside of scope for now for simplicity as it's deep on rendering teritory

5- i extremely like the idea with dolar sign and refering itself when concerns are doing something

6- this is concerning indeed. we can expect (in future) that certain things will be handled exclusively on UI, in case we make simple change to number of rolls, we want to add/remove array rows. there might also be operations such as strike invalidating other column. or change to one row breaks the sequence and values in subsequent rows should be cleared

7- fair point, how does react deal with such larges amount of data? would considering sth like service workers make sense in future? for now, we can assume 1 deal = 1 store and we can have multiple tabs/stores on page

8- this is for a future, we can assume that spot is ticking now, however, it does not invalidate data. also, when we invalidate first, we can change flag to indicate that invalidation occurs and does not have to occur on every subsequent tick

9- this is nice to have, we can considering using valtio's proxyWithHistory. we are expecting to use Redux's devtools with valtio. on this note, it would be nice to have second view with debugging information to see flow of data (sync paths, listeners, why listener was called, what were the arguments passed to that function, etc.)

10- the schema is dynamic, HOWEVER, we can assume that we will always register with sub-type. aka. we will register product-specific side-effects and concerns on top of Schema valid for that given product. Deal as whole will have union of all products, but we will sub-type where possible to avoid developer errors and typos.
