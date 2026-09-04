# Test Stack

This directory is the entire integration-test surface for nammayatri: the React
dashboard you point a browser at, the Python HTTP servers that back it, and the
process-compose entries that boot them. The Postman collections + mock servers
they drive live one level up, in `Backend/dev/`.

If you're trying to run an integration test against a fresh dev stack, this is
the one file you need to read.

## Architecture

The dashboard + test-local-api run on your laptop (`, run-local-test-dashboard`);
the backend stack, test-context-api and mock-server come up together via
`, run-mobility-stack-dev` — so when that stack runs on a devbox, so do
test-context-api and the mocks. The dashboard just points its context-api base
at whichever host the stack runs on (`localStorage.ny.contextApiBase`).

```
  ┌──────────────────────────────────────────────────────────────┐
  │  , run-local-test-dashboard   (your laptop)                   │
  │                                                                │
  │   test-dashboard (React)  http://localhost:7070                │
  │   tabs: Collections │ Custom Flows │ Finance │ Remote Stack     │
  │        │                                     │                 │
  │        │ HTTP/SSE                            │ HTTP/SSE         │
  │        │                           ┌─────────▼──────────────┐  │
  │        │                           │  test-local-api (7083) │  │
  │        │                           │  • /api/control-center │  │
  │        │                           │  • /api/ny-react-native│  │
  │        │                           │  • /api/remote/{...}   │  │
  │        │                           │  • /api/git/refs       │  │
  │        │                           └─────────┬──────────────┘  │
  └────────┼─────────────────────────────────────┼─────────────────┘
           │ HTTP/SSE                             │ subprocess + ssh/rsync
           │ (context-api base = localhost        ▼
           │  OR a devbox)              ┌────────────────────────────┐
           │                            │ Remote host (or localhost) │
           │                            │ runs `, run-mobility-stack-│
           │                            │ dev` inside a PTY,         │
           │                            │ streamed back via SSE      │
           │                            └────────────────────────────┘
           ▼
  ┌──────────────────────────────────────────────────────────────┐
  │  , run-mobility-stack-dev   (local OR devbox)                 │
  │                                                                │
  │   test-context-api (7082)        mock-server (8080)            │
  │   • /api/collections             Juspay/Stripe/FCM/SMS/…       │
  │   • /api/config-sync                                           │
  │   • /api/terminal/{...} (PTY)    rider-app (8013)              │
  │   • DB resets / log tailing      driver-app (8016)             │
  │     (psycopg2 + backend HTTP)    dashboards (8017/8018)        │
  │                                  mock-registry (8020)          │
  │                                  postgres/redis/kafka          │
  └──────────────────────────────────────────────────────────────┘
                     ▲
                     │ HTTP calls under test
                     │
        ┌──────────────────────────┐
        │ integration-tests/       │
        │ collections/<Suite>/     │
        │   <NN>-*.json (Postman)  │
        │   Local/   ← env vars    │
        │   Master/  ← env vars    │
        └──────────────────────────┘
```

## What each piece is for

### `integration-tests/` — the test corpus
Path: `Backend/dev/integration-tests/collections/<Suite>/`

- Postman v2.1 collection JSONs grouped by business flow (ride / bus / metro /
  subway / scheduler / loyalty / membership / etc.).
- Each suite has `Local/` + `Master/` subfolders containing environment files
  (`Local_<env>.postman_environment.json`). The dashboard's **Env Type**
  picker chooses between them; the `envType` variable inside each env file is
  what the per-collection prerequest reads to decide whether to call
  `pm.execution.skipRequest()` for mock-only requests.
- Conventions and the auto-skip mechanism: `Backend/dev/integration-tests/Rules.md`.

### `ny-qa-automation` — a second collection source (NY/MSIL/YS)
A **private** repo, checked out on disk rather than baked into this repo or
any image — resolved fresh on every scan/run (`_qa_automation_dir()` in
context-api, `_qa_dir()` in qa-collections-service), in this order:

1. `$QA_AUTOMATION_DIR` env var, if set (must point at its `src/api_tests`).
2. `<repo-root>/data/ny-qa-automation/src/api_tests` — the managed clone the
   dashboard's **🔄 Sync ny-qa-automation** button creates/updates (same
   gitignored-`data/`-dir convention as `data/control-center`).
3. `<repo-root>/../ny-qa-automation/src/api_tests` — a sibling checkout, for
   anyone who already had it cloned there.

`context-api/server.py`'s `_scan_qa_collections()` walks its `NY/`, `MSIL/`,
`YS/` folders the same way `scan_collections()` walks
`integration-tests/collections/<Suite>/`, so they show up as three more
entries in the dashboard's **Collection** dropdown — Env Type still offers
**Local** / **Master**, sourced from that repo's shared
`Local.postman_environment.json` / `Masterc2.postman_environment.json` (and
`MSIL/Master.postman_environment.json` for MSIL specifically, since that's
the one suite with its own).

These three groups are marked `backendOnly: true` in the `/api/collections`
response: their collections use `pm.execution.setNextRequest()` for branching,
which the dashboard's in-browser engine (`services/postman-runtime.ts`) does
not implement, so `CollectionRunner.tsx` routes their **Run** button through
`qa-collections-service` (below) instead of running steps in-browser. The
step-tree still renders as a read-only preview either way. They're excluded
from **Run All Collections**' bulk sweep.

**🔄 Sync ny-qa-automation** (always visible in the action bar, not gated on
NY/MSIL/YS being selected — they won't even be in the Collection dropdown yet
on a machine that's never synced) calls `POST /api/qa-collections/sync` on
test-local-api, which
clone-or-pulls into `data/ny-qa-automation` and — on success — the dashboard
immediately re-fetches `/api/collections` so a teammate's update to the
automation repo shows up without restarting anything. It uses whatever git
credentials (SSH agent, HTTPS credential helper) are already configured on
the host doing the sync — nothing in this dashboard handles auth for that
private repo itself, so a container running this needs its own git access
(e.g. a read-only deploy key mounted in) for the button to work there.

Missing checkout → the dashboard just shows the 46 native suites; nothing
breaks — click Sync (or point `QA_AUTOMATION_DIR` elsewhere) once you have
access.

### `test-tool/qa-collections-service/` — backend Newman engine (port 7083, via local-api)
Runs one ny-qa-automation collection per Newman subprocess
(`qa_newman_runner.js`, using newman's programmatic API so it can stream a
JSON event per request/assertion instead of waiting for a full report) and
exposes the same start/events/stop + SSE shape as `/api/load-test` on
test-local-api:

- `POST /api/qa-collections/run` — `{collections: [{directory, filename}],
  envFile, concurrency}` → `{runId}`. What `CollectionRunner.tsx`'s Run button
  calls for a `backendOnly` group. A `collections` entry with `directory` but
  no `filename` expands to every collection currently in that directory
  (`_expand_collections` in `qa_runner.py`) — e.g. `{directory: "NY"}` runs
  the whole NY suite without naming every file, which is what an external
  caller's "run all of NY/MSIL/YS" config uses.
- `GET /api/qa-collections/events/<runId>` — SSE stream of NDJSON events.
- `GET /api/qa-collections/runs/<runId>` — full persisted detail for one run
  (status, pass/fail, and every event recorded, capped at 2000) — survives
  after the SSE stream closes, for a caller that only checks in after the run
  finished. Failed requests carry their response body/headers and the
  request body/headers that produced them (`qa_newman_runner.js` only
  captures these on failure, to keep passing runs' event stream light).
  404 if the run id is unknown (includes runs evicted past the last 20).
- `POST /api/qa-collections/stop/<runId>`.
- `GET /api/qa-collections/runs` — recent + active runs, summary only (id,
  status, triggeredBy, collections, pass/fail) — useful for polling a
  webhook-triggered run's outcome from outside the dashboard (e.g. CI).
- `POST /api/qa-collections/webhook` — triggers a run from outside the
  dashboard entirely. Requires `QA_WEBHOOK_TOKEN` to be set (the route is
  503 until it is); the caller sends it back as `X-QA-Webhook-Token`. Body,
  in priority order: (1) its own `{collections: [{directory, filename?,
  envFile?}], envFile?, concurrency?}` — run exactly that, for a caller that
  keeps its own idea of "which flows" (e.g. System Control Centre's
  per-app-group config); (2) `{directory, filename[, envFile]}` to run just
  one collection; (3) empty/omitted — run whatever
  `qa-collections-service/webhook-config.json` lists (this dashboard's own
  local "configured" set, ships with an empty `collections: []`). Any
  `filename`-less entry expands to "every collection in that directory" per
  the shorthand above, in all three forms.
- Needs `npm install` once inside `qa-collections-service/` (installs
  `newman` locally — not a nix devshell dependency).
- **Deep link**: `http://<dashboard-host>:7070/?qaRunId=<runId>` opens a
  standalone overlay (`QaRunViewer.tsx`, mounted in `App.tsx` regardless of
  which tab is selected) showing that run's live progress (SSE, while
  running) or its persisted detail (once finished) — this is the link an
  external trigger (e.g. System Control Centre) hands back to whoever needs
  to watch a run it kicked off.

### `mock-servers/` — request mocks for external services
Path: `Backend/dev/mock-servers/`, port `8080`, process namespace `test`.

A single Python service that mocks Juspay / Stripe / PayTM / Acko / SOS /
WhatsApp / CMRL / CRIS / FCM / SMS / etc. The Postman collections target it
via `{{mockServerUrl}}` and `{{mock_fcm_url}}`. Comes up with the backend stack
(`, run-mobility-stack-dev`, profiles `backend`/`full`); on Master/cloud envs it
isn't running and the auto-skip ensures those steps don't try to call it.

### `test-tool/context-api/` — the control plane (port 7082)
Process: `test-context-api`.

Backs almost every dashboard action. Endpoints (selected):

- `GET /api/collections` — scans the integration-tests directory and returns
  the Suite × EnvType × Env × Suite grid, plus the ny-qa-automation groups
  (NY/MSIL/YS — see below) appended the same shape, `backendOnly: true`.
- `GET /api/collection/<dir>/<filename>` — raw Postman collection JSON (checks
  both integration-tests/collections and ny-qa-automation).
- `POST /api/config-sync/import` — download + apply an upstream DB bundle
  (`master`, `prod`, or `prod_international` — keys of `CONFIG_SYNC_BUNDLE_URLS`)
  into atlas_dev so the local stack reflects production-shaped data. The last
  successful source is persisted at `data/config-sync/.last-synced-env` and
  re-exposed via `GET /api/config-sync/status` as `last_synced`. The dashboard's
  **Sync From** dropdown uses this to skip re-syncing when the local DB is
  already aligned with the selected upstream env. Per-collection compatibility
  (Helsinki ⇒ `prod_international`, others ⇒ `prod`, `master` everywhere) is
  derived inside `_derive_compatible_envs()` in `server.py`.
- `POST /api/terminal/{start,input,resize,kill}` + `GET /api/terminal/stream` —
  PTY sessions used to run Postman prerequest scripts and other helper shells.
- Service-log tailing (`tail -f` over the Haskell process log files).

It needs Postgres (5434), Redis, Kafka, Passetto and the backend HTTP endpoints
up, so it is brought up **as part of** `, run-mobility-stack-dev` (profile
`backend`) on the same host as the stack. When that stack runs on a devbox,
test-context-api runs there too; the local dashboard reaches it by setting its
context-api base to that host (see Remote Stack below).

### `test-tool/local-api/` — host-side launcher (port 7083)
Process: `test-local-api`.

Handles things the browser can't do for itself:

- Launches the **control-center** webapp and the **ny-react-native** rider /
  driver mobile apps locally; streams setup logs back.
- Lists git refs for repos under `data/`.
- **Remote stack** (this directory): `POST /api/remote/deploy` rsyncs the repo
  to an SSH target (skipped for `localhost`), `POST /api/remote/start` opens
  a PTY over SSH and runs `, run-mobility-stack-dev` there. The dashboard's
  **Remote Stack** tab is a thin wrapper over these endpoints and reuses the
  same xterm.js `Terminal` component used by `context-api`'s PTY API.
- **Port discovery** (single source of truth): `GET /api/devbox/ports
  [?refresh=1][&host=…]` returns `{host, ports, caddyPort, contextApiPort,
  caddyRoutes}` by reading `<workspace>/data/ports.json` off the stack host —
  directly when the stack is local, over SSH otherwise, using the `host`,
  `sshUser`, `sshPort` and `remoteDir` recorded in `<repo-root>/.devbox-id.json`
  by `GET /api/devbox/resolve`. Nothing is cached on disk locally; the dashboard
  port table, launcher-spec `${ports.*}` / `${host}` and **Tools → Service
  Ports** all go through this one endpoint.

Pure stdlib — no `paramiko`, no extra runtime deps. The included nix
`test-local-api` process runs `python3 dev/test-tool/local-api/server.py`.

### `test-tool/dashboard/` — the React UI (port 7070)
Process: `test-dashboard`.

Where you actually drive a test run. Tabs:

- **Collections (Integration Tests)** — pick Suite → Env Type → Env → Test
  case, run all steps in the browser via an embedded Postman runtime
  (`services/postman-runtime.ts`). Mock-only steps are auto-hidden when the
  selected env type is not `Local`.
- **Custom Flows** — bespoke step trees not modelled as Postman collections.
- **Finance Visualization** — read-only view over the finance side-effects of
  a recent ride / booking.
- **Remote Stack** — SSH deploy + run `, run-mobility-stack-dev` against a
  remote host. The dashboard re-points its own `context-api` base at that host's
  resolved port automatically on boot (`syncContextApiBase()` in `config.ts`,
  cached in `localStorage.ny.contextApiBase`) — there is nothing to click.
- **Tools → Service Ports** — modal listing every resolved port of the stack
  currently targeted: dashboard endpoints, the Caddy `<host>:<caddyPort>/<service>/`
  routes and the direct `http://<host>:<port>` URLs, each with a copy button.
- **Remote Stack → 🗒** — full-screen log viewer. Each pane picks its own file and
  can be split horizontally (▥) or vertically (▤); the split direction and open
  files persist in `localStorage.ny.remoteStack.logLayout`.

The dashboard talks to test-context-api for data, test-local-api for
host/remote actions, and the backend services directly via the local proxy
for live API calls.

## Setup: two-terminal workflow

`, run-mobility-stack-dev` brings up the backend stack (`ny` + `tools`)
**together with** test-context-api (7082) and mock-server (8080).
`, run-local-test-dashboard` brings up just the browser-facing pieces
(test-dashboard + test-local-api), which can point at a backend stack running
locally or on a devbox.

```bash
# Terminal 1 — backend stack + test-context-api (7082) + mock-server (8080)
, run-mobility-stack-dev

# Terminal 2 — test dashboard (7070) + test-local-api (7083)
, run-local-test-dashboard
# → open http://localhost:7070
```

### Legacy one-shot
If you want the previous "everything in one process-compose UI" experience:

```bash
, run-mobility-stack-full
```

This is the same set of processes as before, just under the new name.

### Profiles in nix
Each command maps to a single `services.nammayatri.profile` value, applied to
the same `Backend/nix/services/nammayatri.nix` module:

| Command                       | profile         | Processes                                                   |
|-------------------------------|-----------------|-------------------------------------------------------------|
| `, run-mobility-stack-dev`    | `backend`       | `ny` + `tools` + `test-context-api` + `mock-server`         |
| `, run-local-test-dashboard`  | `testDashboard` | `test-dashboard`, `test-local-api`, `config-sync-server`    |
| `, run-mobility-stack-full`   | `full`          | everything (`ny` + `tools` + all test procs)                |

Disabled processes are excluded from process-compose entirely, so cross-profile
`depends_on` entries don't block startup. Infra services (postgres / redis /
kafka / clickhouse / nginx / passetto) are only brought up under `full` and
`backend`.

## Remote stack — running on another machine

The **Remote Stack** tab in the dashboard lets you target an SSH-reachable host.

1. Pick **Host** (use `localhost` for a local PTY; no SSH, no rsync), **User**,
   **Port**, optional **Identity file**, and **Remote dir** (default
   `/tmp/nammayatri`).
2. Choose **Copy mode**: `rsync` (default) or `skip`. Skip is useful when the
   remote already has the repo checked out at the right commit.
3. Click **Deploy** — rsyncs the local repo to the remote, excluding `.git`,
   `data/`, `node_modules`, `dist-newstyle`, etc. (full list lives in
   `local-api/server.py` as `REMOTE_EXCLUDES`).
4. Click **Start mobility-stack-dev** — opens an `ssh -tt` PTY into a fresh
   bash login shell that runs `cd Backend && nix develop .#backend -c , run-mobility-stack-dev`
   (the **Command** field shows this canonical command).
5. Nothing else to do: on its next boot the dashboard asks local-api for the
   stack's resolved `test-context-api` port, sets `localStorage.ny.contextApiBase
   = http://<host>:<port>` and reloads once. From then on the collection scanner,
   prerequest PTY, log tailer, etc. all hit that stack. **Tools → Service Ports**
   shows the base currently in use. The dev-box card also offers **Open Remote SSH**,
   which opens the remote workspace in VS Code (shown only when `code` is on PATH —
   `GET /api/remote/editor-available`).

The PTY is streamed back to the panel via Server-Sent Events; you can type
into it, resize the window, and stop it from the dashboard.

## Ports

| Port  | Service                            | Process              |
|-------|------------------------------------|----------------------|
| 7070  | test-dashboard (React)             | `test-dashboard`     |
| 7082  | test-context-api                   | `test-context-api`   |
| 7083  | test-local-api                     | `test-local-api`     |
| 8013  | rider-app (BAP)                    | `rider-app-exe`      |
| 8016  | driver-app proxy → 8116 / 8081     | `driver-proxy`       |
| 8017  | rider-dashboard                    | `rider-dashboard-exe`|
| 8018  | provider-dashboard                 | `provider-dashboard-exe` |
| 8020  | mock-registry                      | `mock-registry`      |
| 8080  | mock-server (Juspay/FCM/SMS/…)     | `mock-server`        |
| 5434  | Postgres (atlas_dev)               | `db-primary`         |
| 6379 / 30001 | Redis standalone / cluster  | `redis` / `cluster1` |

## See also

- `Backend/README.md` — full backend dev setup.
- `.cursor/docs/02-build-and-dev.md` — nix / cabal commands.
- `.cursor/docs/17-testing-framework.md` — testing framework deep dive.
- `Backend/dev/integration-tests/Rules.md` — collection authoring conventions,
  mock-skip semantics, "Adding a New City" checklist.
