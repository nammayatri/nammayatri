# Performance and profiling toolkit

Tools for answering four questions about a backend service:

- why is this API slow?
- why does memory keep growing?
- why is an API fine on its own but slow under many concurrent requests?
- where is the ceiling (throughput, pools, GC, CPU) of the current implementation?

Everything runs from the backend dev shell (`nix develop .#backend` / direnv).

| Symptom | Start with | Then |
|---|---|---|
| "Is it CPU, GC, DB, Redis or an external call?" | `, perf-watch` while the app serves traffic | the matching mode below |
| API fine alone, slow under load | `, perf-ramp <url>` | `, prof run <exe> rts` during a ramp |
| One API is slow even at concurrency 1 | `, perf-watch` datastore / external tables | `, prof run <exe> time` |
| Memory keeps growing | `, perf-watch` (live-after-major trend) | `, prof run <exe> heap-info`, then `heap-cc` / `heap-retainer` |
| Trying an RTS setting | `, prof run <exe> rts -- -A64m` | compare `gc.log` summaries |

## 1. Live telemetry without rebuilding: `, perf-watch`

Every service exposes Prometheus metrics on its `metricsPort` (rider-app `9999`, driver-app `9997`,
allocator `8056`). The kernel registers GHC runtime metrics there, and all service exes run with
`-with-rtsopts=-N -T`, so they are populated.

```bash
, perf-watch                 # rider-app on :9999, every 5 s
, perf-watch -p 9997 -i 2    # driver-app, every 2 s
, perf-watch --csv /tmp/rider.csv   # also keep a time series
```

Each window prints:

- **load**: requests/s, 5xx/s, CPU cores used by the process.
- **gc**: share of wall time the process was stopped for GC (every capability stops, so this adds
  straight to latency), GC share of CPU, allocation rate, GCs/s (major GCs/s), last pause.
- **memory**: live data after the last GC, peak live at a major GC, heap in use, RSS, and the trend of
  live data after major GCs (MB/min). A steady positive trend after warm-up means data is retained.
- **handlers**: per-endpoint rate, average, p95/p99 upper bounds (histogram buckets), share of total
  request time. The p95/p99 columns show the bucket bound (`<=250` = at most 250 ms).
- **datastore ops** (`datastore_operation_duration`) and **external calls** (`external_request_duration`):
  the same table for DB/Redis operations and outbound HTTP, which is where request time usually goes.

**macOS caveat.** `Kernel.Tools.Metrics.Init.serve` also registers `procMetrics` from `prometheus-proc`,
which reads `/proc/<pid>/stat`. That file does not exist on macOS, the collector throws, and every scrape
of `/metrics` returns HTTP 500 (perf-watch says so when it happens). On Linux (dev boxes, pods) it works.
The fix belongs in shared-kernel: register `procMetrics` only when `/proc/self/stat` exists.

## 2. Finding the concurrency limit: `, perf-ramp`

Runs the endpoint at increasing concurrency (closed loop, [oha](https://github.com/hatoo/oha)),
scraping the service's metrics around every level.

```bash
, perf-ramp http://localhost:8013/v2/rideBooking/list -H 'token: <token>'
, perf-ramp http://localhost:8013/v2/serviceability/origin -m POST -T application/json \
    -D body.json -H 'token: <token>' -c 1,4,16,64,128 -z 30s
, perf-ramp http://localhost:8016/ui/... -p 9997        # driver-app metrics port
```

It prints one row per level (client-side req/s, p50/p90/p99, success, non-2xx; server-side CPU, GC share,
allocation, live data), then:

- **throughput ceiling**: the highest req/s reached.
- **saturation**: the first level where more concurrency stops buying throughput while latency keeps
  rising. Past it, requests queue: by Little's law, `concurrency = req/s x latency`, so with req/s flat
  every extra client only adds waiting time.
- **verdict** at the top level:
  - *CPU-bound*: the service used about all cores. Profile with `time` mode.
  - *GC-bound*: the service was stopped for GC for a large share of the time. Tune the nursery
    (`-A`), cut allocation.
  - *waiting-bound*: low CPU and GC, yet latency grew. Requests wait on a shared resource: a
    connection pool, an external dependency, or a lock. The server-side breakdown shows which
    datastore/external operations got slower between the first and the last level.
- `results.json` in `Backend/perf-runs/ramp/<timestamp>/` for comparing runs.

The load generator runs on the same machine as the service, so near the CPU limit both compete.

### Limits that exist in this codebase

These are the usual reasons for "fine alone, slow under load":

| Resource | Where it is set | Default |
|---|---|---|
| Beam / KV Postgres pool | env `POSTGRES_POOL_MAX` x `POSTGRES_POOL_STRIPES` (mobility-core `Kernel.Beam.Connection.EnvVars`) | 5 x 1 connections |
| Esqueleto Postgres pool | `esqDBCfg.connectionPoolCount` in the dhall config | 10 |
| Redis (hedis) pool | `connectMaxConnections` in the dhall Redis configs | 50 |
| Outbound HTTP | `http-client` managers keep at most 10 idle connections per host, extra ones are opened and closed per request | |
| Request timeout | `incomingAPIResponseTimeout` (rider-app dhall) | 15 s, then 408 |
| Logging | dev dhall: level DEBUG, raw SQL logging, console and file | every line is JSON-encoded, twice |
| Capabilities | `-with-rtsopts=-N` | all cores (parallel GC syncs all of them) |

Both Postgres pools and the Redis pool are `resource-pool` 0.4, which parks a waiting request on an
MVar. Pool exhaustion therefore shows up in `rts` mode as time `blocked on an MVar` while the capabilities
stay idle. Dev logging can dominate allocation under load; if the alloc flamegraph is full of logging,
lower the level in your local dhall before drawing conclusions.

## 3. Instrumented runs: `, prof`

```bash
, prof run <exe> <mode> [-- <extra RTS flags>]
, prof build <exe> [eventlog|profiling]
, prof report <run-dir>          # regenerate the reports of a run
, prof list                      # recent runs
, prof modes
```

`, prof run` builds the exe in a separate build dir, runs it from `Backend/` with the mode's RTS flags,
and writes everything to `Backend/perf-runs/<exe>/<timestamp>-<mode>/`. The app's output goes to
`app.log` in that directory. Drive traffic, then press **Ctrl-C once**: the app shuts down gracefully (up
to its `graceTerminationPeriod`), the RTS writes the profile, and the reports are generated and printed
(`SUMMARY.txt`). `.prof` and `.hp` files are only complete after a clean exit, so do not `kill -9`.

### Modes

| Mode | Build | Records | Use it for |
|---|---|---|---|
| `time` | profiling | cost-centre time and allocation (`.prof`) | which functions burn CPU or allocate |
| `heap-cc` | profiling | live heap by cost-centre stack | which code produced the data that stays live |
| `heap-type` | profiling | live heap by type | what kind of data is growing |
| `heap-retainer` | profiling | live heap by retainer set (slow) | who keeps that data alive |
| `heap-info` | eventlog | live heap by info table, with source locations | memory growth at close to normal speed |
| `heap-closure` | eventlog | live heap by closure type | cheapest first look at memory |
| `rts` | eventlog | GC and scheduler events | slow under load: GC, idle cores, blocked threads |

Every run also records one line per GC (`gc.log`, from `+RTS -S`) and the RTS summary.

### Build flavours

Both flavours are a full cabal project on top of `cabal.project` in their own build dir, so switching
never touches `dist-newstyle`. The first build of an exe compiles all its local packages (rider-app: 21
components) and takes a long time; later builds are incremental. `PROF_JOBS` sets `cabal -j` (default 1,
because parallel builds get killed for memory on laptops). A `cabal.project.local` is picked up
automatically (it is symlinked to `cabal.project.<flavour>.local`). cabal ignores `ghc-options` at the
top level of a project file ("Unrecognized field 'ghc-options'"); put them under a `package *` stanza,
as these two files do.

- **eventlog** (`cabal.project.eventlog`): `-O1 -finfo-table-map -eventlog -j2`, `Local` flag off.
  `-fdistinct-constructor-tables` is left out: on this codebase it makes the build run out of
  memory on a laptop, and heap-info still attributes closures to source locations without it
  (constructors per type). No profiling libraries needed, runs at close to normal speed, so it is
  the one to use under load.
- **profiling** (`cabal.project.profiling`): cost-centre profiling, `profiling-detail:
  toplevel-functions`, `-O1`, `Local` flag off. Needs profiling libraries for every dependency. nixpkgs
  ships them; the libraries built by haskell-flake (mobility-core, euler-hs, beam, prometheus, ...) do
  not, so enter the profiling shell first:

  ```bash
  NY_PROFILING=1 nix develop --impure .#backend
  ```

  `NY_PROFILING` is read by `Backend/default.nix` (like `NIX_LOCAL_BUILD`) and enables library profiling
  for the packages in `profilingDeps`. The first entry builds those libraries (about 47 derivations),
  later entries come from the nix store. Without the variable, or without `--impure`, nothing changes.
  If a profiling build fails with *"Perhaps you haven't installed the profiling libraries for package
  X"*, add X to `profilingDeps`.

Both flavours use `-O1` rather than the `-O0` of the `Local` flag, because an unoptimised build
allocates and spends time in different places than production (`-O2`).

### Running next to the stack

`, prof run` starts the exe itself; it does not go through process-compose (which stops the Haskell exes
with SIGKILL, and that would lose the profile). Two ways to use it with the local stack
(`, run-mobility-stack-dev`):

- **Replace the stack's copy**: stop that process in the process-compose TUI, then `, prof run` it. It
  binds the same ports, so callbacks and other services reach the instrumented copy.
- **Run beside it**: give it other ports and load it directly:
  `SERVICE_PORT=18013 METRICS_PORT=19999 , prof run rider-app-exe rts`.

The dhall configs default to the fixed ports of `ports.nix`, so no other environment is needed with
`, run-mobility-stack-dev`. With the `-on-available-ports` stack, export the same `*_PORT` variables the
stack sets (see `common.environment` in `nix/services/nammayatri.nix`). Exes that need extra environment
(for example `CONSUMER_TYPE` for kafka-consumers) take it from your shell.

`PROF_BIN=/path/to/binary , prof run <name> <mode>` skips the build and runs a binary you already have.

## 4. Reading the output

**`SUMMARY.txt`** collects everything below and lists the artifacts.

**GC section (`gc.log`)**, every mode:

- collections per generation with p50/p99/max pause. Minor (gen0) pauses scale with the data that
  survives the nursery; major (gen1) pauses scale with all live data, since the copying collector copies
  it.
- *paused*: the share of wall time every capability was stopped. This is added to every in-flight
  request.
- *allocation*: total and per second. High allocation means frequent minor GCs.
- *live data after major GCs*: first, last, max and trend (the final GC at exit is excluded). Rising
  after warm-up means a leak or an unbounded cache.
- *worst pauses* with their time offset, to line up with latency spikes.
- *hints*: which knob or mode to try next.

**Scheduler section (`rts` mode, from the eventlog):**

- *capability busy*: the share of time each capability ran Haskell code. All near 100% means CPU-bound;
  low while requests are slow means the threads are waiting.
- *why threads stopped*: `heap overflow` (nursery full, GC coming), `thread yielding` (preemption),
  `making a foreign call` (for example libpq), `blocked on ...`.
- *time spent blocked, by reason*: count, total, p50/p99/max. `blocked on an MVar` covers socket I/O
  (the I/O manager parks threads on MVars), `resource-pool` waits and locks; waits under 1 s are shown
  separately, because the long ones are idle keep-alive connections and timers. `blocked on black hole`
  means many threads are forcing the same unevaluated thunk: a lazily built shared value.
- *GC pauses and sync*: sync is how long it took every capability to stop. Long syncs mean a thread ran
  without reaching a safe point (a tight non-allocating loop or an unsafe foreign call).

**Cost-centre profile (`time` mode):**

- `<exe>.prof`: the header table lists the cost centres with the most *individual* time and allocation;
  the tree below has *inherited* numbers per call path (everything under that node). `entries` is the
  call count. Only top-level functions of local packages and exported functions of dependencies are cost
  centres; the rest is charged to the enclosing one.
- `flamegraph-time.svg`, `flamegraph-alloc.svg`: the same tree as flame graphs. Wide frames matter.
  Click to zoom.
- `<exe>.eventlog.json`: drag it into <https://www.speedscope.app>. *Left Heavy* merges identical stacks,
  *Sandwich* ranks functions by self and total time with their callers and callees.
- The profile covers the whole run, including startup (migrations, cache warm-up). Keep the load running
  long enough that it dominates, or use speedscope's time-ordered view to look at the load window only.

**Heap profile (`heap-*` modes):** `heap.html` is an interactive area chart over time. Each band is one
of the largest consumers; the *Detailed* tab lists all of them. What a band is depends on the mode:
cost-centre stack (`heap-cc`), type (`heap-type`), retainer set (`heap-retainer`), closure type
(`heap-closure`), or info table with the source location that allocated it (`heap-info`; locations exist
for code built with `-finfo-table-map`, which is the local packages in the eventlog build). A band that
only grows is the leak. `heap-retainer` then says what holds it. If heap in use or RSS is much larger than
live data while the bands are flat, the growth is fragmentation (often pinned `ByteString`s, which show
as `ARR_WORDS` in `heap-closure`), not retained data.

## 5. Playbooks

**One API is slow, even with no load**

1. `, perf-watch` while calling it: is the time in a datastore op, an external call, or neither?
2. Neither, or it is CPU: `, prof run <exe> time`, call the API a few hundred times
   (`, perf-ramp <url> -c 1 -z 60s`), Ctrl-C, read the flamegraphs / speedscope.
3. Datastore: check the query (`logRawSql` in `app.log`), its plan, and how many round trips one
   request makes (entries per query in the `.prof`, or the calls/s in the datastore table vs req/s).

**Memory keeps growing**

1. `, perf-watch`: live-after-major trend vs heap in use vs RSS. Live rising = retained data; live flat
   but heap/RSS rising = fragmentation or non-Haskell memory.
2. `, prof run <exe> heap-info` under a steady load for several minutes: which source location's band
   grows?
3. For who allocated it and who keeps it: `heap-cc`, then `heap-retainer` (profiling build).
4. Typical culprits: in-memory caches without eviction, `IORef`/`TVar` maps that only grow, lazy
   accumulators (thunks building up: look for `THUNK` in `heap-closure`), forked threads that never end
   (the `threads created / finished` line in `rts` mode).

**Fine alone, slow under load**

1. `, perf-ramp <url>` to find the saturation level and the verdict.
2. Repeat the top of the ramp while `, prof run <exe> rts` records. Idle capabilities + `blocked on an
   MVar` growing with concurrency = pool or lock; compare the pool sizes above with the concurrency.
   High GC share = allocation; see the gc section hints. Busy capabilities = CPU; switch to `time` mode.
3. Change one thing and ramp again: a pool size, or an RTS flag via `, prof run <exe> rts -- <flags>`.

## 6. RTS flags worth trying

All service exes are linked with `-rtsopts`, so any RTS flag can be given at run time (after `--` in
`, prof run`, or through the `GHCRTS` environment variable for a normally started exe).

| Flag | Effect |
|---|---|
| `-A64m` | bigger nursery: far fewer minor GCs for allocation-heavy servers, at the cost of memory per capability |
| `-n4m` | split the nursery into chunks, so capabilities share it better when used with a large `-A` |
| `-qn2` / `-qg` | parallel GC on only 2 threads / sequential GC: less GC sync overhead with many capabilities |
| `-N4` | fixed capability count instead of all cores |
| `--nonmoving-gc` | concurrent old-generation collector: short major pauses for a large live heap |
| `-I0` | no idle GC |
| `-c` | compacting instead of copying for the old generation: less memory, slower major GCs |

Measure each change with a ramp and the gc.log summary; the right values depend on the service's
allocation rate and live data size.
