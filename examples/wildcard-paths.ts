/**
 * Wildcard path examples — _() hash key utility for Record paths.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import { _ } from '@sladg/apex-state'

// @llms-example: Mark Record segments with _() for hash key typed paths
const NOTIONAL_PATH = `portfolio.books.b1.products.p1.legGroups.g1.legs.${_('l1')}.notional`
// → "portfolio.books.b1.products.p1.legGroups.g1.legs.l1.notional" (typed as containing HASH_KEY)
// @llms-example-end

// @llms-example: Build deeply nested Record paths with multiple _() hash keys
const commentTextPath = `users.${_('u1')}.posts.${_('p1')}.comments.${_('c1')}.text`
// → "users.u1.posts.p1.comments.c1.text" (typed as containing HASH_KEY)
// Used with useConcerns for wildcard path registration
// @llms-example-end

void NOTIONAL_PATH
void commentTextPath
