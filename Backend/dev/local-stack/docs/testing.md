# Tests and CI

Every automated test, the three workflows, and what a red tick does and does not mean.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Tests, and what CI actually runs

Three workflows, none of which deploys anything:

| Workflow | What it proves | When |
|---|---|---|
| `algeria: node tests` | **Every** test in `tests/` (`run-all.sh` globs), on Node 20 and 22: sign-in rules, the push relay, the release rehearsal — and the money path (`wallet`, `restricted`, `deletion`, the guard's `WALLET_EMPTY`) with its SQL run in a real Postgres (PGlite, in-process) | Every push to `algeria/**`, and pull requests |
| `algeria: ride regression` | A whole backend, brought up from nothing on a throwaway runner, with real routing on an Algiers map, signs a `+213` number in and answers a ride search **with a price** | Every push to `algeria/**`, and on demand |
| `algeria: build backend` | The Haskell binaries. 44 minutes; nothing else triggers it | Push to `algeria/build-backend` |

```bash
(cd tests && npm ci) && bash tests/run-all.sh   # every test, as CI runs them
node tests/wallet.test.js              # the money path: canWork, top-up, credit, the day
./setup.sh price                       # sign in, ask for a priced ride
```

**The ride regression was red from its first run (2026-09-20) to phase 4
(2026-10-06), and never about the change that triggered it.** It ran with
`SKIP_OSRM=1`, on the theory that mock-google would stand in for routing. It
cannot: the mock in the image we run (upstream `03a7531`) has **no
`/directions/json`**, answers 404, and the rider turns that into
`E500 GOOGLE_MAPS_API_ERROR` — so every search failed with *"ride search
returned no searchId"* before a price was ever computed. Read in the rider's
log, not the job's: the job only said FAILED.

Now the job routes as the server does — rider → `maps-shim` → OSRM. It cuts
Algiers out of the Geofabrik extract with `osmium` (cached a week; a 93 MB
graph built in seconds), so a red tick means **a ride in Algiers could not be
priced** by a stack built from this repository. `SKIP_OSRM=1` still brings a
stack up without the graph, but it now skips the price check and says so.

Its trigger was a list of paths; four days of commits went by without it
running. It now runs on **every push to `algeria/**`**, and every Monday at
06:00 UTC since 2026-10-08, when `algeria/osrm-routing` became the default
branch of both repositories (GitHub runs schedules only from the default
branch; until then it was upstream's `main`, which lacks this file). The same
day the "Run workflow" button appeared for all three workflows, and upstream's
`stale.yaml` — which would otherwise have started labelling pull requests every
night — was disabled on both.

`preflight` accepts a **pulled** image in place of the loose binaries in `bin/`,
which is what the regression job and `deploy-backend.sh` both do.
