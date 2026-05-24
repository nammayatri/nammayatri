# Testing Framework

> The canonical, regularly-updated source for the test stack is
> [`Backend/dev/test-tool/README.md`](../../Backend/dev/test-tool/README.md).
> This file is a short index — when in doubt, read that one.

## Components at a glance

- `Backend/dev/integration-tests/collections/<Suite>/` — Postman collections
  with `Local/` + `Master/` env-type subfolders. Per-collection prerequest
  auto-skips mock-only steps when `envType ≠ Local`. Each env file is also
  auto-mapped to an upstream config-sync source (`master` always, plus
  `prod_international` for Helsinki and `prod` elsewhere) — surfaced as the
  dashboard's **Sync From** dropdown. See `Backend/dev/integration-tests/Rules.md`.
- `Backend/dev/mock-servers/` — unified mock for external HTTP services
  (port 8080). Process: `mock-server`.
- `Backend/dev/test-tool/context-api/server.py` — control plane on
  port 7082. Endpoints for collection scanning, config-sync from master,
  PTY-backed prerequest execution, service-log tailing.
- `Backend/dev/test-tool/local-api/server.py` — host-side actions on
  port 7083: launches control-center, ny-react-native, and SSH-based remote
  test-context-api sessions (`/api/remote/*`).
- `Backend/dev/test-tool/dashboard/` — React UI on port 7070. Tabs:
  Collections, Custom Flows, Finance, Remote Stack.

## Three-terminal workflow

```bash
, run-mobility-stack-dev       # backend
, run-test-context-server      # mock-server + test-context-api
, run-local-test-dashboard     # dashboard + local-api → http://localhost:7070
```

Use `, run-mobility-stack-full` to start everything in one process-compose UI
(the legacy behaviour).

## Remote test-context-api over SSH

The dashboard's **Remote Stack** tab POSTs to `local-api`'s `/api/remote/*`
endpoints to:

1. `deploy` — `rsync` the repo to an SSH target (skipped for `localhost`).
2. `start` — open an `ssh -tt` PTY running `, run-test-context-server`.
3. Stream the PTY back via Server-Sent Events into an embedded xterm.js
   terminal (the same `Terminal` component used by `context-api`'s PTY API,
   generalized via `baseUrl` / `pathPrefix` props).
4. Optionally flip the dashboard's `context-api` base URL to the remote host
   (`localStorage.ny.contextApiBase = http://<host>:7082`).

All transport is stdlib-only (`subprocess` + `pty` + `fcntl`) — no `paramiko`
or other Python deps to manage in nix.

## Adding a new integration test

See `Backend/dev/integration-tests/Rules.md` — covers collection structure,
the `Local/` vs `Master/` env layout, the `envType` variable, and the
mock-skip prerequest convention.
