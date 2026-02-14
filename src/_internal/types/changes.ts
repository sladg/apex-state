/**
 * Internal Change Tuple Type
 *
 * Internal variant of ArrayOfChanges with plain strings.
 * Matches the generic signature of ArrayOfChanges but ignores type params,
 * avoiding expensive DeepKey/DeepValue resolution for internal pipeline plumbing.
 * Public API uses ArrayOfChanges<DATA, META> for type safety.
 */

import type { GenericMeta } from '~/types/meta'

export type ChangeTuple<
  _DATA = unknown,
  _META extends GenericMeta = GenericMeta,
> = [string, unknown, GenericMeta][]
