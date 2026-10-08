# Backups

The nightly encrypted backup: what it takes, what it skips, where it goes, and how to restore.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Backups — `./backup.sh`

```bash
./backup.sh              # take one now
./backup.sh restore F    # restore F into a scratch database and check it
./backup.sh list         # what we hold, and how old the newest is
./backup.sh install      # install the nightly systemd timer (02:30)
```

Nightly, encrypted with GPG AES-256, uploaded off the server with `rclone`.
**1.8 MB** per backup out of a 183 MB database, which is the whole point of the
next two sections.

**What runs is the file a release ships, `/opt/ny/local-stack/backup.sh`.**
Since phase 4 (2026-10-06) the units are in git too — `stack/systemd/
movin-backup.service` and `.timer` — and a release installs them into
`/etc/systemd/system`, so a change to the backup is live after `ops/deploy.sh`
and nothing else. Before that, `movin-backup.service` ran a hand copy in
`/root/backup.sh` that no release touched: until 2026-10-04 it was the only
copy of the 488-line script (driver papers, the `movin` schema), with git and
`/opt` holding two older versions. The three were made identical on
2026-10-04, and the unit switched to the shipped one in phase 4.

**The off-site copy has an expiry date.** On 2026-10-04 rclone warned:
*« This remote uses rclone's shared Google Drive client_id, which is being
retired and will stop working during 2026. »* When it stops, every backup stays
on the VPS — the script says `upload failed` in the journal, which nobody
reads. The fix is a Google Cloud client id of the company's own, set once in
`rclone config` (https://rclone.org/drive/#making-your-own-client-id); it needs
the Google account that owns the Drive.

**Done 2026-10-08: the remote uses our own client id.** Google Cloud project
*Movin backup*, OAuth app *Movin backup* — External, **In production** (in
*Testing* the refresh token expires after 7 days and the upload quietly stops),
one scope, `drive.file`, a *Desktop app* client. The owner set it on the server
with `rclone config` (edit `movin-drive`, new client id and secret, refresh the
token through `rclone authorize` on the laptop); the previous settings are kept
beside the config as `rclone.conf.before-own-client`. A manual run of the unit
then uploaded `movin-20261008T101330Z.tar.gz.gpg` (47 MB) with no warning.

`drive.file` means rclone sees **only the files its own client created**. So
the old folder, renamed **`movin-backups-old`** in Drive, is invisible to
rclone: the backups up to 2026-10-08 — and phase 6's
`subscription-final-20261007T184106Z.sql.gpg` — are downloaded from it in the
browser, not with `rclone`. New backups go to a fresh `movin-backups`. The
narrow scope is kept on purpose: this server's key cannot read anything else
in that Drive.

### It is not `pg_dump atlas_dev`, for two reasons

**The encryption keys are in a different container.** `atlas_app` stores rider
phone numbers encrypted and the keys live in `ny-passetto-db`. A dump of
`ny-postgres` alone restores perfectly and leaves every number permanently
unreadable — a backup that looks complete and is not. Both databases go into the
archive, and a restore is only meaningful with both.

**Most of the database is rebuildable, and skipping it is 183 MB → 1.8 MB:**

| Schema | Size | In the backup? |
|---|---|---|
| `geo` | 155 MB | no — `./geocoder-prepare.sh` rebuilds it in ~5 min |
| `public` | 7 MB | no — PostGIS `spatial_ref_sys`, ships with the extension |
| `tiger`, `tiger_data`, `topology` | 2 MB | no — PostGIS reference data |
| `atlas_app` | 3.9 MB | **yes** — riders, bookings, rides |
| `atlas_driver_offer_bpp` | 3.6 MB | **yes** — drivers, fares, the BPP side |
| `atlas_registry` | 40 kB | **yes** |
| `movin` | small | **yes** — wallets, top-up receipts, drivers' papers, deletion requests |

An include list has one failure mode: a schema added later is left out silently.
So the script **refuses to run if the schema set has changed**, and says which
one is new. That guard earned itself on its first run by catching `tiger_data`.

#### It earned itself a second time, and the second time is the useful lesson

`movin` was created on **26 August**. From the 27th the backup **refused to run
every night**, exited 1, and printed exactly which schema was new. That is the
guard doing its job perfectly — it chose no backup over a backup silently
missing the subscription payments.

Nobody read it. Found on **30 August** while checking something else: four
consecutive nights with no backup, the newest archive four days old, locally
and on Drive. Fixed by adding `movin` to `DATA_SCHEMAS`; a manual run then
produced `movin-20260830T150531Z.tar.gz.gpg`, 3.5 MB, copied offsite, and the
gap is closed.

Two habits come out of it, and they are worth more than the one-line fix:

- **A schema added to this database is not finished until it is named in
  `DATA_SCHEMAS` or `REBUILDABLE_SCHEMAS`.** Adding it is part of the migration,
  not a follow-up.
- **A failure nobody is told about is a failure that continues.** After any
  schema change, run `systemctl status movin-backup.service`. Better still,
  give the unit an `OnFailure=` that actually speaks — the timer will otherwise
  keep failing politely for as long as you let it.

### Setting it up

```bash
openssl rand -base64 32 > /root/.movin-backup-pass
chmod 600 /root/.movin-backup-pass

rclone config                                        # once, interactively
./backup.sh install    # copies systemd/movin-backup.* in (the remote is named there)
```

On the live server the release does that `install` itself; it is only for a box
that has never had one.

```bash
```

**The passphrase must not live only on this server.** It is the one thing
standing between the uploaded archive and every rider's phone number, and the
backups exist for the case where this machine is gone. Keep it wherever the
Android signing key is kept. Changing it later does **not** re-encrypt existing
backups, so the old value has to be kept too.

With `RCLONE_REMOTE` unset the backup still runs and says loudly that it stayed
on this server. That is deliberate — a local-only backup is worth something, and
a script that implied it went offsite when it did not would be worth less than
nothing.

### Verifying, rather than assuming

`./backup.sh restore` decrypts into a **scratch database**, never the live one,
and checks the row counts against the manifest inside the archive. Do it against
a copy pulled back down from the remote, not the local file — that is the copy
that will actually be used.

Two traps met while building this, both silent:

- `docker exec -i` inside `ssh host "bash -s" <<EOF` **consumes the rest of the
  script** from stdin; the first query runs and nothing after it does.
- A unit that works when you run it can still fail under systemd. Fire
  `systemctl start movin-backup.service` and read the journal, the same way the
  certbot timer had to be checked.
