# Performance Benchmarks

This directory contains performance benchmarks for apex-state optimizations.

## Available Benchmarks

### sync-flip-paths.js

**Purpose**: Measures the performance difference between 2-pass vs 1-pass processing of sync and flip paths.

**Run**:
```bash
node benchmarks/sync-flip-paths.js
```

**Simulates**:
- Realistic FX Options trading system with 8 product groups (G10 currencies)
- 48 products (6 per group: 3 currencies × 2 tenors)
- 12+ level deep nested paths
- 1000+ sync path relationships (value propagation, aggregation, hedging)
- 200+ flip path relationships (enable/disable cascading)

**Real Trading Relationships**:
1. **Currency pair → Notional currency**: ccyPair change propagates to notionalCcy and spot rates
2. **Multi-level risk aggregation**: Strike → Product → Group → Enterprise
3. **Hedging cascades**: Delta → hedge ratio → hedge notional
4. **Market data propagation**: Spot/vol/IR updates cascade to all products
5. **Enable/disable cascades**: Group trading state affects all products and features
6. **Risk breach cascades**: Enterprise limit breaches suspend all groups
7. **Cross-currency relationships**: EURUSD + GBPUSD → EURGBP synthetic rate

**Scenarios**:
1. Single strike greek update
2. Currency pair change (propagation test)
3. Market data shock (large batch volatility update)
4. Risk breach cascade (enterprise limit → all groups disabled)
5. Cross-group hedging update
6. Add new product group (bulk registration)
7. Mixed real-world batch (combination of all update types)

**Expected Results**:
- Single updates: ~1.5-2x speedup
- Complex scenarios: ~2-3x speedup (best gains on cascading updates)
- Market data shocks: ~2.5x speedup
- Average: ~1.8x speedup

### listener-filtering.js

**Purpose**: Tests whether ListenerConfig filtering is faster than calling listener functions directly and letting them return early.

**Run**:
```bash
node benchmarks/listener-filtering.js
```

**Simulates**:
- 5 scenarios with 10-100 listeners
- Various filter types (path matching, metadata, complex combinators)
- High miss rate scenarios (realistic production usage)

**Filter Types Tested**:
1. Simple path matching (HAS_PATHS)
2. Metadata matching (HAS_META_OF)
3. Complex boolean combinators (AND/OR/NOT)
4. Real-world mix (100 listeners, 50 changes)
5. High miss rate (most listeners don't fire)

**Results**:
- **Direct calls win ALL scenarios** (0% filter wins, 100% direct wins)
- Average overhead with filtering: **21.3%**
- Worst case (real-world mix): **46.8μs overhead per operation**
- Even in high miss rate scenarios: **direct calls are 36% faster**

**Recommendation**:
❌ **REMOVE listener filtering** - it adds overhead without benefit. Let listener functions handle early returns internally. The filter evaluation overhead exceeds the cost of function calls + early returns.

## Adding New Benchmarks

Follow the pattern in `sync-flip-paths.js`:

1. **Setup**: Mock store with realistic data structure
2. **Implementations**: Current vs Optimized approaches
3. **Scenarios**: Real-world usage patterns
4. **Measurement**: Use `performance.now()` with multiple iterations
5. **Summary**: Table with speedups and absolute time savings
6. **Recommendation**: Clear decision based on data

## Notes

- Keep benchmarks focused on hot paths (frequently executed code)
- Test with realistic data volumes (not toy examples)
- Show both relative speedup (X times faster) and absolute savings (microseconds)
- Include real-world scenarios that match production usage
