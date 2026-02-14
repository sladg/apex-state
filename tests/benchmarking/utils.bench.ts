import { bench, describe } from 'vitest'

import { dot } from '~/utils/dot'
import { is } from '~/utils/is'

// =============================================================================
// Test Data
// =============================================================================

const shallowObj = { a: 1, b: 2, c: 3 }
const deepObj = {
  a: {
    b: {
      c: {
        d: {
          e: 'value',
        },
      },
    },
  },
  x: {
    y: [{ z: 10 }, { z: 20 }],
  },
}

// =============================================================================
// Dot Utils Benchmarks
// =============================================================================

describe('dot utils', () => {
  describe('dot.get', () => {
    bench('shallow get', () => {
      dot.get(shallowObj, 'a')
    })

    bench('deep get (5 levels)', () => {
      dot.get(deepObj, 'a.b.c.d.e')
    })

    bench('array access get', () => {
      dot.get(deepObj, 'x.y.0.z' as never) // Note: dot implementation uses split('.') so '0' is key
    })

    bench('missing shallow', () => {
      dot.get(shallowObj, 'z' as never)
    })

    bench('missing deep', () => {
      dot.get(deepObj, 'a.b.x.y' as never)
    })

    bench('unsafe deep', () => {
      dot.get__unsafe(deepObj, 'a.b.c.d.e')
    })
  })

  describe('dot.has', () => {
    bench('shallow has', () => {
      dot.has(shallowObj, 'a')
    })

    bench('deep has', () => {
      dot.has(deepObj, 'a.b.c.d.e')
    })

    bench('missing has', () => {
      dot.has(deepObj, 'a.b.x.y' as never)
    })
  })
})

// =============================================================================
// Is Utils Benchmarks
// =============================================================================

describe('is utils', () => {
  describe('is.empty', () => {
    bench('null', () => {
      is.empty(null)
    })

    bench('undefined', () => {
      is.empty(undefined)
    })

    bench('number (0)', () => {
      is.empty(0)
    })

    bench('boolean (false)', () => {
      is.empty(false)
    })

    bench('empty string', () => {
      is.empty('')
    })

    bench('non-empty string', () => {
      is.empty('hello')
    })

    bench('empty array', () => {
      is.empty([])
    })

    bench('non-empty array', () => {
      is.empty([1, 2, 3])
    })

    bench('empty object', () => {
      is.empty({})
    })

    bench('non-empty object', () => {
      is.empty({ a: 1 })
    })
  })

  describe('is.primitive', () => {
    bench('string', () => {
      is.primitive('hello')
    })

    bench('number', () => {
      is.primitive(123)
    })

    bench('object (false)', () => {
      is.primitive({})
    })
  })
})
