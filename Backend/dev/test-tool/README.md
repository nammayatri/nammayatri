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
  the Suite × EnvType × Env × Suite grid.
- `GET /api/collection/<dir>/<filename>` — raw Postman collection JSON.
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
  remote host; flip the dashboard's `context-api` base to that host's `:7082`
  once it's healthy (stored in `localStorage` under `ny.contextApiBase`).

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
5. Click **Use this context-api** — sets `localStorage.ny.contextApiBase =
   http://<host>:7082` and reloads. From then on the dashboard's collection
   scanner, prerequest PTY, log tailer, etc. all hit the remote. **Reset**
   clears the override.

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
