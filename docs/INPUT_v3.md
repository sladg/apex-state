## In tests (aggregations)

i want to make sure that:

- correctly calculated from 0 existing paths - undefined,
- currenctly calculated and always synced from 1 path,
- when we have 3 existing agg paths with given value (all sync) and we add new path with different value, we should
  change to undefined,
- if we have 4 different paths and 4 different values, changing agg path sets 4 values, is flip path is present between
  two of these paths, the agg path should end up as undefined as flip paths will ensure that the 4 values will never be
  same
