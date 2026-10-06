# The server as it was on 2026-10-04 — before the restructuring

Step 2 of the plan the boss approved on 2026-10-04 (« Safety first »): a
recorded "before", so every later change can be compared with, and undone to,
this state. Nothing on the server was changed to take it, except that the
unused `/opt/ny/local-stack/backup.sh` now equals the script that runs.

## Where the way back is

| What | Where |
|---|---|
| The three repositories | tag **`snapshot-2026-10-04`** in each (app `d1003a7`, backend `89fcba5869`, website `0485530`); also `pre-restructure` in the app and the backend (the website's `pre-restructure` is its own, from September) |
| The whole of `/opt/ny` — the stack, the secrets, the website build | root-only archive on the server, `/root/snapshots/2026-10-04/opt-ny.tar.gz` (154 MB), with the systemd units, `/root/backup.sh`, the firewall rules and the container/image list beside it, and a `SHA256SUMS` |
| The databases, the encryption keys, the driver papers | the encrypted nightly backup, plus one taken by hand at 16:23 (CEST) the same day; copied off the server (see README → *Backups*, and the warning there about the Google Drive client id) |
| The app as installed by testers | iOS build 16 on TestFlight (EAS `64d00159`), approved by the owner on 2026-10-04 |

## Git against the server — every tracked file in `local-stack`, by hash

    tracked files                      136
    identical on the server             88
    differ                               6
    never deployed (laptop tools)       42
    present only on the server          88   (names not listed here: this
                                              repository is public, and they
                                              include certificates and keys)

The six that differ, in lines present on one side only:

| File | Only on the server | Only in git | Reading |
|---|---:|---:|---|
| `docker-compose.yml` | 16 | 69 | **Both ways.** The server's is a superset in places (the website's mount and volume) — never copy git's over it |
| `setup.sh` | 3 | 38 | server behind, with three lines of its own |
| `geocoder/index.sql` | 2 | 13 | server behind |
| `README.md` | 22 | 2,181 | server's copy is old; harmless |
| `.gitignore` | 0 | 22 | harmless |
| `probe-two-country-rides.py` | 13 | 25 | a laptop tool; the server holds an old copy, the current one is run from `/tmp` |

Making these agree is the server restructuring (step 3), file by file, with the
server as the truth until a release command exists. Among the server-only files
are about thirty `*.bak*` / `*.before-*` copies left by past in-place edits —
also step 3.

## The backup script

Three versions until today: git (422 lines), `/opt/ny/local-stack/backup.sh`
(407, run by nothing) and `/root/backup.sh` (488, what `movin-backup.service`
runs). Git and the unused copy now hold the running one, byte for byte
(`sha256 e76d1189…`).

## Phase 0 completed — 2026-10-05

- `bin/MANIFEST.txt` put back on the server (added, nothing replaced). The four
  binaries in `bin/` match it by sha256.
- **What actually runs is not `bin/`.** Hashed inside the containers: the rider
  and driver apps come from the image `ghcr.io/nammayatri-algeria/ny-backend:latest`
  = `ny-rider:patched`, digest `sha256:108eca6c…`, created 2026-09-14 10:10 UTC by
  **CI run #10** (`algeria-backend-build.yml`, branch commit `09dc606410`, image
  tag `03a7531-10`), deployed 14:02 UTC that day. `rider-app-exe`
  (`c1d32b83…`) and `dynamic-offer-driver-app-exe` (`c6481ff3…`) differ from
  `bin/`; the gateway (`c2052525…`) and the registry (`f9a3f3d8…`) match it.
  Older images kept for rollback: `ny-rider:rollback-20260914-1402` and three
  before it.
- `docker compose config` saved root-only beside the archive
  (`/root/snapshots/2026-10-04/compose-config.yml`) — it holds secrets, so it is
  not in this public repository.
- The server notes the same beside the manifest: `bin/WHAT-RUNS.txt`.
- **Every file, one by one** (2026-10-06): `box-inventory-2026-10-06.md` — 188
  files with hash, date and size, against git: 87 identical, 6 differ, 7 on the
  server only, 34 leftover copies from past edits, 23 the website's build, 6 in
  `bin/`; 25 not named (certificates, keys, secrets); 43 tracked files are
  laptop tools never deployed. Correction to the plan: `bin/MANIFEST.txt` is
  gitignored with the rest of `bin/` — it was on the laptop, never in git.

## Phase 1 — which copy is right, file by file (2026-10-06)

The rule: git changes, the server does not. Each of the six files was diffed
against the server's copy, line by line.

| File | Right copy | Why |
|---|---|---|
| `docker-compose.yml` | **both, merged into git** | The server's `edge-web` mount was missing from git (nginx serves the site and the console from it) — taken. The server's `movin-driver-docs:` volume is now declared by the website's overlay — not taken, a leftover. Every other difference is comments, git's being the true ones. Proven: `docker compose config` resolves byte-identical from either file (29 services). Commit `a232f1fc51`. |
| `setup.sh` | **git** | Git adds `SKIP_OSRM=1` (the CI ride regression) and skips the binaries check when the image is present. The server's "own" three lines are the same three calls git wraps in a condition. |
| `geocoder/index.sql` | **git** | A comment only. The server's still claims Arabic search works through this column; it did not, and git corrected the claim on 2026-09-10 (`arabic-search.sql`). |
| `.gitignore` | **git** | Git's is a strict superset: it adds the driver codes, the trusted phones and `.env` (the payment key). On the server it ignores nothing — there is no git checkout there. |
| `README.md` | **git** | The server's copy is from 2026-08-23. Its "own" lines are text rewritten in git since (the old tariff table, the old MANIFEST claim, the pre-iOS push note). |
| `probe-two-country-rides.py` | **git** | A laptop tool; the server holds the 2026-09-13 copy. Git's is the one that ran the six-of-six ride test on 2026-10-04 (both fleets, every row). |

`backup.sh` (item 3) was settled on 2026-10-04: git holds the 488-line script
`movin-backup.service` runs, byte for byte. systemd's `ExecStart` still points
at `/root/backup.sh` — **phase 4 moves it** to the repository's copy, and until
then a change here is not live until it is copied there.

### What still differs on the server, and when it goes

After phase 1, git is right for all six. The server still holds older copies of
five of them, and the compose file's stale comments and redundant volume line.
**None is read by anything running** — `.gitignore`, the README and the probe
are never read at all, `setup.sh` and `index.sql` only when the stack or the
place index is rebuilt, and the compose file resolves identically. They are
refreshed by the first release of the deploy command (phase 3), which by design
deploys this state and then proves every hash equal — or now, by hand, on the
owner's word.

**Done by hand, on the owner's word, 2026-10-06.** The six server copies were
first checked unchanged since they were read that morning, saved root-only to
`/root/snapshots/2026-10-06-phase1/before.tar`, and replaced in place with
git's (`cat >`, so permissions and inodes stay; `setup.sh` still executable).
No container was restarted. The resolved compose configuration hashed
`94888cfd…` before and after.

**Phase 1 done when — met:** every tracked file hashed against the server:
**94 deployed, 94 identical, 0 differ**; 44 tracked files are laptop tools and
documents that are never deployed.

## Phase 3 — the first release (2026-10-06)

`ops/deploy.sh`, on the owner's word, 09:21:59 UTC, commit `0d463c3016`:
82 files shipped — 44 already identical, 32 new (`db/` and files never deployed
before), 6 rewritten in place (the five scripts that read `db/`, and a short
`README.md` for whoever logs in), 44 removed (the old top-level `.sql`, old
probes, demos and retired scripts; saved in `/opt/ny/local-stack.prev`). **No
SQL applied, nothing restarted.** Checks: 21 containers still running, both
healthz 200, `nginx -t`, every file's sha256 as released. `.shipped` written.

Then `ops/deploy.sh tidy`: the **35** leftover copies (`*.bak*`, `*.before-*`,
one `*.broken-*`; the plan counted 30 on 2026-09-24) moved to
`/root/snapshots/leftovers-2026-10-06/`, root only. `ops/deploy.sh status`
afterwards: all 82 shipped files exactly as released.

## Phase 4 — the checks that guard a ride (2026-10-06)

**1. The ride regression, red since its first run (2026-09-20), green since
run 22.** Three causes, each hiding the next, every one read in the rider's or
the driver's log rather than the job's "FAILED":

| Run | What stopped it | Fix |
|---|---|---|
| 1–19 | `SKIP_OSRM=1` left `Maps_Google` on mock-google, which in our image (upstream `03a7531`) has **no `/directions/json`**: 404 → `E500 GOOGLE_MAPS_API_ERROR` → no `searchId` | CI cuts Algiers out of the Geofabrik extract (osmium, cached a week) and builds a real OSRM graph (93 MB, seconds); routing as on the server |
| 20 | `searchId` at last, then the price poll's `grep` found nothing on its first look and, under `pipefail`, `set -e` ended the script silently | `\|\| true` on the three pipelines in `verify_connector` |
| 21 | The BPP found all 5 cars and priced them; the rider received `on_search`; every results read failed: `column driver_offer.vehicle_desc does not exist` | `setup.sh` now applies the two columns our binary reads (`driver-offer-vehicle.sql`, `search-request-chosen-drivers.sql`) — the server got them by hand |
| 22, 23 | **green**: 4 estimates in 5 s, twice (before and after freshening the drivers); 3 min, then 2 min with the map cached | |

Still not covered: CI runs the upstream seed (one merchant, its fare: 258 for
every variant), not the server's two merchants and tariffs.

**2. Trigger:** every push to `algeria/**` (was a path list). The `schedule:`
cannot fire while the default branch is upstream's `main` — owner's decision.

**3 and 4, released on the owner's OK, 12:39 UTC, commit `37c3d6f04e`:** 2 new
files (`systemd/movin-backup.service`, `.timer`), 2 rewritten in place
(`backup.sh`, `setup.sh`), no SQL, **nothing restarted**; the units installed
into `/etc/systemd/system`, `daemon-reload`, the old ones kept in `.prev/units`.
`systemctl show movin-backup.service` → `ExecStart=/opt/ny/local-stack/backup.sh`;
timer enabled, next run 02:31. Then the outside check — the server's hashes
against the commit, recomputed from git on the laptop: **all 84 files and both
units byte for byte the commit.** `/root/backup.sh` is no longer run by anything
(left in place, identical to the 2026-10-04 copy).

## Phase 5 — the shims become software (2026-10-06, in progress)

**1. Packaging.** `package.json` + lockfile for each shim. maps-shim's lockfile
was taken from the running image (a fresh one would have moved three patch
versions), and the Dockerfile installs it with `npm ci`; the guard's has no
dependency, on purpose, and a change to it never restarts the guard.

**3. Tests for the money path** (done before 2, so Node 22 was proven by
them): `wallet.test.js` (50), `restricted.test.js` (22), `deletion.test.js`
(19), `auth-guard-wallet-gate.test.js` (12), the SQL run in PGlite against our
own migrations. Each was shown able to fail by breaking the code on purpose —
18 faults, all caught (two first missed, which led to the credit-race test).
They found one real bug: `restricted.js` `publish()` never settled when Redis
closed without answering (fixed, `40b9157fba`, in release A). CI runs every test file
(17; it named 4) on Node 20 and 22, on every push.

**Release A**, owner's OK, 13:31 UTC, `64a94a4508`: packaging + that fix.
maps-shim rebuilt from the lockfile — same Node 20.20.2, same 13 package
versions, read inside the new container; the guard untouched. The old image
kept as `ny-maps-shim:previous` (a release now does this before every rebuild
or recreate).

**2. Release B**, owner's OK, 13:39 UTC, `491087ce2d`: both shims on Node 22
(22.23.3). Guard recreated, maps-shim rebuilt; `ny-auth-guard:previous` =
`node:20-alpine`, `ny-maps-shim:previous` = release A's build. Checks passed,
outside check 88/88; routes, wallet and `/auth/channels` (+222 SMS, +213
SMS-in, WhatsApp) answer as before.

**4. The split** — next, one module at a time, each released on its own.

**4. The split, maps-shim.** Under it first, `tests/maps-shim-routes.test.js`:
32 requests through the real `server.js` process, each one's answer, SQL and
OSRM / mock-google URLs recorded as a golden file from the code that ran;
shown able to fail by two deliberate faults. Then one module per release,
each moved verbatim (checked byte for byte):

- **Release C**, owner's OK, 16:26 UTC, `b5999a1c06`: `directions.js` (+
  `reply.js`, the shared `send`). maps-shim restarted. `.shipped` says
  **checks FAILED, and it was a false alarm**: the release probed maps-shim's
  healthz once, ~1 s after `docker restart`, while the process was still
  loading (started 16:26:21.6, listening 16:26:22.9; healthz 200 and a real
  route served at 16:26:29). `ops/deploy.sh verify`: 90/90 files the commit.
  Not re-released to clear the stamp — that would replace `.prev` and lose the
  rollback. The checker now waits up to 30 s for a restarted service.
- **Release D**, owner's OK, 16:33 UTC, `c08aa72e8a`: `places.js`
  (autocomplete, details, labels, reverse geocoding). Checks passed with the
  waiting healthz; 91/91 from outside. Live afterwards: search in Nouakchott
  and Algiers, reverse geocoding naming each country, an Algiers route 13.7 km.
  maps-shim's `server.js` is now the router and start-up — 439 lines, from 886.

**4. The split, the auth guard.** Under it first, `tests/auth-guard-routes.test.js`:
a scripted day of 50 requests through the real guard (sign-ins in both
countries, the lock, resend, SMS-in, trusted phones, number change, a driver,
the wallet gate, a rating, bounds, the start limit, /healthz before and after),
each one's answer and everything said to the backend, Moorsyl, maps-shim and
admin-api recorded; deterministic; four deliberate faults caught. It records
one oddity as it is, not changed in a no-change refactor: an oversized body
gets a dropped connection, not its 413 (nginx answers first in production).
Then one module per release, each moved verbatim, each on the owner's OK:

| Release | UTC | Commit | Module |
|---|---|---|---|
| E | 16:44 | `714bb68c7e` | `driver-rules.js` — wallet gate, rating note, reply bound |
| F | 16:50 | `76f130be7b` | `limits.js` — starts per number / address, SMS budget |
| G | 16:55 | `855ccdb9e2` | `gateway.js` — the code, Moorsyl, the /healthz counters (`smsStats()`) |
| H | 16:59 | `b3a4a930da` | `personal-codes.js` — enrolled drivers' codes |
| I | 17:04 | `9f8ff3b09c` | `number-change.js` — the route's body one indent less |

Each: checks passed, outside check all files, sign-in channels and the guard's
own startup lines unchanged. `server.js` 1782 -> 1119 lines (the sign-in flow
and the router stay); maps-shim's 886 -> 439.

**Phase 5 done when — met.** Every line that decides whether a driver may
work has a test that runs in CI (wallet, restricted, the guard's gate), on
Node 20 and 22, on every push; both shims are packaged and on Node 22, with
the replaced images kept as `:previous`.
