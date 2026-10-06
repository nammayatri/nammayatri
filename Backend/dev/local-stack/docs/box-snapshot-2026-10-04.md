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
