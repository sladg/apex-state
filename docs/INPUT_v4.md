---
created: unstaged
updated: unstaged
status: active
---

# Concerns extensions

We need to allow for following usecases:

- Globally defined concerns which merge/override local ones.
- General ability to have multiple levels of concerns to avoid duplication of definitions across hudreds of fields and components. We want to re-use the definition if it applies to whole cart for example. Instead of needing to define same BoolLogic condition for each field (assume multiple levels of definitions, allow for wildcards while limiting the union types to avoid typescript throwing complexity errors - we can expect thousands of paths in object by default with DeepKey).
- Allow for referencing itself. Aka. I want the concerns functions to be able to accept things like path via $ variable? maybe. The idea being that I might have an array of paths inside object which should be disabled (such as carts.123, carts.abc, carts.000.shoes) and I might want to do BoolLogic with it.
