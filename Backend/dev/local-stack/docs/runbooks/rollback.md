# Runbook — undo a release

Three kinds of thing a release changes, and each has its own way back. Decide
which you need before typing anything. How a release saves what it replaces is
in [releasing.md](../releasing.md).

## 1. Files — `ops/deploy.sh rollback`

    Backend/dev/local-stack/ops/deploy.sh rollback

Puts back every file the last release changed or removed, from
`/opt/ny/local-stack.prev`; deletes the files it added (unless edited since —
those are left and named); restores `.shipped` and `.shipped.files`; puts back
the systemd units it replaced; and redoes the same restarts and reloads the
release did, so the old files are what runs.

What to know:

- **One level only.** `.prev` holds the last release. After a rollback it is
  renamed `local-stack.prev-rolled-back-<time>`, so a second rollback has
  nothing to undo. To go further back, check out an older commit and release
  that.
- **Releasing again replaces `.prev`.** Re-releasing to "fix" a release that
  only *looked* wrong loses the way back to the one before. Diagnose first.
- Afterwards, `ops/deploy.sh status` shows the restored record, and the
  [release runbook](release.md)'s step 4 checks apply as after a release.

## 2. Images — `<container>:previous`

When a release rebuilt maps-shim or recreated a service, it first tagged the
image that was running as `<container>:previous` (since phase 5). Rolling the
files back does not touch images. On the server:

    docker tag ny-maps-shim:previous ny-maps-shim:local
    cd /opt/ny/local-stack && docker compose up -d --no-deps maps-shim

The tag moves with each release, so it is the image from just before the last
one that replaced it.

## 3. Database — not undone by anything above

A release applies a new `db/*.sql` once. **Rollback does not reverse SQL.**
Each file says its own way back in its header. Two kinds:

- **Config rows** (tariffs, geofences, merchants): re-apply the previous
  version of the rows, keyed by merchant, then drop the Redis cache keys the
  backend reads them through ([gotchas.md](../gotchas.md)).
- **Dropped data**: restore the dump taken before the drop. Phase 6's, for the
  old subscription tables:

      gpg --batch --decrypt --passphrase-file /root/.movin-backup-pass \
          /var/backups/movin/subscription-final-20261007T184106Z.sql.gpg \
        | docker exec -i ny-postgres psql -U postgres -d atlas_dev

  (Run on the server, as root. The offsite copy is in Drive's
  `movin-backups-old` folder, reachable from the browser only — see
  [backups.md](../backups.md).)

Anything larger — a lost table, a broken database, a lost server — is the
nightly backup, put back with **`./restore.sh`** (since 2026-10-08), on the
server, from `stack/`:

    ./restore.sh rehearse offsite:latest   # prove it first: a throwaway copy, nothing live touched
    ./restore.sh live offsite:latest       # then for real (or a local file instead of offsite:…)

`live` refuses if anything outside the data schemas is built on them, asks you
to type `restore live`, takes a backup of what it replaces, stops the backends,
the shims, the console, the fleet and the bot, loads the data and passetto's
keys **each in one transaction** (a broken archive changes nothing), puts back
the Arabic place names, documents and drivers' codes, flushes Redis, starts it
all, and checks: every table against the dump, a sample of phone numbers
actually decrypted, the documents opened. Expect the service down for a few
minutes. Everything written since the backup is lost — that is what a restore
is; the safety backup it takes first is the way back from it.

**Rehearsed 2026-10-08 on the server, from the offsite copy: 71 seconds**, 106
tables exact, 20/20 numbers decrypted, 58 329 Arabic names, 25 documents. On a
**new server**, bring the stack up first (`./setup.sh`), rebuild the place
index (`./geocoder-prepare.sh`, then `geocoder/append-country.sql`), then
`./restore.sh live`. That path — a whole new box — has not been rehearsed.

## When to roll back, and when not

Roll back when a check after the release fails and the cause is the release:
a container that will not stay up, a healthz that is not 200, a route that
broke in one country. Do **not** roll back for a check that was wrong — the
2026-10-06 release C was stamped FAILED because healthz was probed one second
after a restart; it was the probe, and re-releasing or rolling back would have
cost the real way back.
