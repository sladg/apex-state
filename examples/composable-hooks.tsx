/**
 * Composable hooks examples — buffered, throttled, transformed fields.
 *
 * These examples type-check against the real @sladg/apex-state exports.
 */

import {
  createGenericStore,
  useBufferedField,
  useTransformedField,
} from '@sladg/apex-state'

interface PriceState {
  price: number
}

const store = createGenericStore<PriceState>()

// @llms-example: Chain useBufferedField and useTransformedField for cents-to-dollars editing
const PriceField = () => {
  const raw = store.useFieldStore('price')
  const buffered = useBufferedField(raw)
  const formatted = useTransformedField(buffered, {
    to: (cents: number) => (cents / 100).toFixed(2),
    from: (dollars: string) => Math.round(parseFloat(dollars) * 100),
  })
  // formatted has: value, setValue, commit, cancel, isDirty

  void formatted
  return null
}
// @llms-example-end

void PriceField
