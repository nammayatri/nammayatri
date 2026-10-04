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
