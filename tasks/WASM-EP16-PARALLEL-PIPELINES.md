# WASM-EP16: Parallel Pipeline Execution — Discovery & Analysis

**Epic Key**: WASM-EP16
**Status**: 🔍 Discovery
**Type**: Architecture Analysis — no implementation commitment yet

---

## Context

At target scale (20 pipelines × 5-50 FX Options products per pipeline), each pipeline is
entirely independent. Currently all pipelines share one WASM module and run serially — a
market data tick affecting all 20 pipelines runs them one after another on the main thread.

This task is a discovery to determine whether parallel pipeline execution is viable, what
the trade-offs are, and what the implementation path would look like.

---

## Problem Statement

20 independent pipelines with no shared state are processed sequentially. Throughput is
bounded by single-thread WASM performance. On a market data tick that affects all pipelines,
latency = sum(pipeline_1_latency + ... + pipeline_20_latency) instead of max().

---

## Approaches to Evaluate

### Option A: Web Workers (one pipeline per worker)

Each `createStore()` instance lives in its own Web Worker. The main thread dispatches
changes and collects results via `postMessage`. JS listener callbacks run in the worker,
with state applied back to a main-thread proxy via structured transfer.

**Questions to answer:**
- Can valtio proxies live in a worker (no DOM access, no React context)?
- How does React rendering integrate — worker posts state snapshot, main thread applies?
- What is the `postMessage` overhead per tick vs the computation savings?
- Does structured clone of 30,000-path state snapshots negate the parallelism gain?
- Can `WebAssembly.Module` be shared across workers (avoid 20× WASM compile cost)?

**Known complication:** Listener handlers are user-defined JS functions — they can't
cross worker boundaries. Either they run in the worker (requires JS function serialization,
not possible) or the listener execution protocol changes significantly.

### Option B: `wasm-bindgen-rayon` (intra-WASM thread pool)

Single WASM instance, internal Rayon thread pool. Parallel execution inside `processChanges`
for the BoolLogic fan-out case (50 products × N concerns evaluated in parallel).

**Requirements:**
- `SharedArrayBuffer` → requires `Cross-Origin-Opener-Policy: same-origin` +
  `Cross-Origin-Embedder-Policy: require-corp` headers
- `wasm-bindgen-rayon` crate + build changes

**Questions to answer:**
- Are COOP/COEP headers feasible for target deployment environments?
- What is the synchronization overhead for small payloads (50 logic evaluations)?
- Does `LogicReactor.process()` parallelize cleanly with rayon `par_iter()`?
- What thread count is optimal for the 20 × 50 product case?

### Option C: Single-threaded but pipelined (no parallelism)

Process pipelines in sequence but with tighter per-pipeline budgets — profile first
to establish whether parallelism is actually needed.

**This should be the baseline benchmark** before committing to either A or B.

---

## Related: Streaming Serialization Cost (Opportunity 3 from perf analysis)

Separate from parallelism — if 50 products stream market data every second, the cost is
not compute but **serialization volume crossing the WASM boundary**.

If the streaming subscription delivers full product snapshots on every tick (all 30 fields),
you're serializing 50×30=1,500 path/value pairs per second into `processChanges`. The
WASM diff rejects most as no-ops, but the serialization and boundary-crossing cost is real.

**The rule**: streaming integrations must send delta-only updates — only paths whose values
actually changed since the last tick. `processChanges` is the correctness guard, not a
substitute for over-sending.

**What to explore**: Whether the JS streaming layer needs explicit delta-tracking helpers,
or whether the subscription API surface should enforce delta-only dispatch before WASM is
called. Decision deferred until WASM-EP5 (streaming) is revisited.

---

## Discovery Tasks

1. **Benchmark baseline** — measure single-pipeline `processChanges` latency for a
   representative 50-product FX pipeline with realistic BoolLogic registrations.
   Determine if 20 × this latency is actually a problem at target tick rate.

2. **Worker feasibility spike** — prototype a single pipeline in a Web Worker:
   - Can JS listener handlers execute in worker context?
   - What's the state-sync story between worker and React?
   - Estimate `postMessage` round-trip overhead.

3. **rayon feasibility spike** — add `wasm-bindgen-rayon`, parallelize
   `LogicReactor.process()` over `affected_logic_ids`, benchmark vs sequential.
   Check COOP/COEP header feasibility.

4. **Profile BoolLogic fan-out specifically** — for a market data tick (spot changes,
   affects all 50 products): how much time is BoolLogic evaluation vs serialization vs
   JS application? Parallelism only helps the computation portion.

---

## Dependencies

- **Requires**: WASM-EP6 (Reactor refactor) — LogicReactor must be isolated before
  parallelizing it
- **Requires**: WASM-EP12 stories complete (baseline micro-opts applied first)
- **Blocks**: None — discovery only

---

## Success Criteria for Discovery Phase

- Baseline latency numbers for 20-pipeline × 50-product scenario at 1 tick/sec and
  10 ticks/sec market data rate
- Clear recommendation: Option A, B, C, or "not needed at current scale"
- If viable: implementation plan with story breakdown

---

## Notes

- Listener handlers being user JS functions is the hard constraint for Option A.
  Workers can't hold arbitrary closures from the main thread. This may be a blocker
  for the full per-pipeline-worker approach, but partial parallelism (WASM computation
  in worker, listener execution on main thread) is still possible with round-trip cost.
- Option B is lower-risk — same threading model, just internal parallelism. Better
  starting point for the discovery spike.
