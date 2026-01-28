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
