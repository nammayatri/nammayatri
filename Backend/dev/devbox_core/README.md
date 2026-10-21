# devbox_core

Python helpers that bring a nammayatri workspace up (and down) on a fleet
dev-box. Used by the 7070 test dashboard's local-api and by the devbox
workflow that System Control Centre's "Devbox Releases" triggers.

Run from `Backend/dev`: `python3 -m devbox_core <command> …` (Python 3.9+).

## Whole workspace

| Command | What it does |
|---|---|
| `spawn --name N --src DIR [--from master\|prod\|prod_international] [--host H --user U]` | pick a dev-box (unless `--host`) → register → rsync `DIR` → start the stack → wait until rider-app / driver-app answer → load data (latest safe unless `--version`) → start the control centre. Prints `DEVBOX_STAGE` lines and ends with `DEVBOX_RESULT`. |
| `stop --host H --user U --name N` | control centre → stack → `rm -rf /tmp/N` → registry entry. Refuses when the owner does not match. |

Owner flags (`--owner-by`, `--owner-user`, `--owner-release`) mark who made a
workspace. A workspace with an owner can only be stopped by that owner; one
made by hand (no owner) is never taken over.

## Single steps

| Area | Commands |
|---|---|
| ssh | `ping` |
| fleet | `machines`, `pick` |
| registry | `registry`, `register`, `release` |
| code | `sync [--dry-run]` |
| stack | `stack-start`, `stack-status [--tail N]`, `stack-wait`, `stack-stop` |
| data | `versions --from S`, `data --from S [--version vN] [--force]` |
| control centre | `cc-start`, `cc-status`, `cc-stop` |

## Output for the workflow

```
DEVBOX_STAGE  {"stage": "sync", "state": "started"}
DEVBOX_STAGE  {"stage": "sync", "state": "done", "ms": 82000}
DEVBOX_RESULT {"devbox": …, "workspace": …, "backend_url": …, "office_url": …, "cc_url": …, "data": "master v14"}
```

`backend_url` (and `rider_url` / `driver_url`) go through hopper's public
gateway, so testers' phones open them on any network:

```
https://service-discovery.devenv.movingtech.net/api/<devbox>/8889/<name>   + /rider-app/... or /dynamic-offer-driver-app/...
```

The dev-box's front desk serves only those two apps on `:8889` and announces
it to hopper (nix-the-builder `scripts/frontdesk`). A dev-box hopper does not
list as public is never picked. `office_url` (`http://<office ip>:8888/<name>`,
every service) and `cc_url` only answer on the office network or the tailnet.

Stages: `preparing`, `sync`, `build`, `start`, `data`, `cc`, `ready`.
States: `started`, `done`, `skipped`, `failed` (with `error`).

## Environment

| Variable | Used for |
|---|---|
| `DEVBOX_SSH_KEY_FILE` | private key for the dev-boxes (default: `~/.ssh/id_ed25519`, `id_rsa`, `id_ecdsa`) |
| `BASE_API_HOST`, `BASE_API_PORT` | service discovery (default `service-discovery.devenv.movingtech.net:8787`) |
| `HOPPER_GATEWAY_URL` | hopper's public gateway, for the public URLs and the "is it public" check (default `https://service-discovery.devenv.movingtech.net`) |
| `CONFIG_SYNC_CLOUDFRONT_URL` / `CONFIG_SYNC_PUBLIC_BUCKET_URL` | where published data versions live (read through the dev-box: the bucket only answers the office's whitelisted line) |

## On the dev-box

| Path | What |
|---|---|
| `/tmp/devbox-registry.json` | every workspace: dir, ports, caddyPort, owner (lock: `/tmp/devbox-ports.lock`) |
| `/tmp/<name>/nammayatri/` | the workspace (code, data, logs) |
| `…/data/stack.log`, `stack.json` | stack output and process group |
| `…/data/control-center.json`, `.log` | control centre pid/port and output |
| `http://<devbox>:8888/<name>/…` | front desk route to the workspace |
| `http://<devbox>:8889/<name>/{rider-app,dynamic-offer-driver-app}/…` | public front desk (tailnet only; reached through hopper's gateway) |
