# Key rotation: the keys and salt that protect stored numbers

Plan point 5, before the first real passenger. **Done on the live server on
2026-10-13** (owner's OK at each step; he ran the steps that read the secrets
folder himself).

Phone numbers (and a few document numbers) are stored twice: encrypted by
passetto, and hashed — `sha256(salt + number)` — so the backend can look a
number up without decrypting anything. Rotating means new passetto keys AND a
new salt, and every stored value converted in one step: a number encrypted
under the old keys, or hashed with the old salt, would no longer match anyone.

## What is here

| File | What it is |
|---|---|
| `rotate.js` | The conversion. `check` reads every value and proves the current salt reproduces every stored hash, writing nothing. `apply` re-encrypts with the new passetto, re-hashes with the new salt, all rows in one transaction, then reads every value back through the new keys. Renames the passengers' photographs, whose file name is the hash. Tested: `tests/key-rotation.test.js`. |
| `window.sh` | The switch on the live server, run by the owner as root: `sudo bash key-rotation/window.sh` (and `... rollback`). Checks everything with nothing stopped (secrets, key store 1|4, `rotate.js check`, the test passenger's identity), dumps the two schemas, converts with the services stopped, switches (overlay into `COMPOSE_FILE`, new salt into `number-change.env`, `--no-deps` recreate of passetto, rider, driver, maps-shim), then proves it: `rotate.js check` through the live passetto with the new salt, and the test passenger signs in as the same person. Rollback converts back to today's keys and salt, which are never touched. |
| `ci-proof.sh` | The whole switch on the ride regression's throwaway stack (a step of `algeria-ride-regression.yml`): a passenger and a driver sign in, the keys and salt are rotated as in the window, and the same two numbers must sign in again as the same people (the person their new token names). |
| `compose.keys.yml` | The switch, as a compose overlay: passetto keeps its name and port (8021) but runs `server` on a new key database with its own volume (`passetto-keys`), its master password from `/opt/ny/secrets/passetto-master.env`; the rider and driver apps get the two dhall copies mounted over upstream's and `MOVIN_HASH_SALT` from `/opt/ny/secrets/hash-salt.env`. Inert until named in `.env`'s `COMPOSE_FILE`; compose refuses it while either secrets file is missing. Merge checked with `docker compose config` on the server, 2026-10-13. Today's `ny-passetto-db` was left as it was, the rollback, until 2026-10-15: since then the overlay puts it in the `old-keys` profile and passetto no longer depends on it. |
| `cleanup.sh` | After the rotation (2026-10-15): checks the live passetto reads the new keys, `.env` names the overlay, compose would recreate nothing and no longer makes `passetto-db`, nothing is connected to it and a stored number decrypts; then (`--apply`) removes `ny-passetto-db` with its volume and `/root/key-rotation.*`. Ends `window.sh rollback` and `configs`. |
| `rehearse.sh` | The rehearsal: copies the seven tables, passetto's database and the photographs into throwaway containers on an internal network, starts a passetto on NEW keys, runs `check` then `apply` there, proves today's keys no longer open a converted value, and removes everything however it ends. Reads the live stack, writes nothing to it. |
| `rider-app.secrets.dhall`, `dynamic-offer-driver-app.secrets.dhall` | Upstream's secrets files at `03a7531` (what the images run), byte for byte, except that `encHashSalt` is read from `MOVIN_HASH_SALT`. Mounted over the upstream files on the day; nothing reads them before. |

## What uses the salt, and must change on the same day

- the rider and driver apps: `encHashSalt` (the two files above);
- `maps-shim/number-change.js`: `NUMBER_HASH_SALT` in `/opt/ny/secrets/number-change.env`;
- the passengers' photographs: `maps-shim/avatars.js` names each `h_<hash>` — renamed by `rotate.js apply --avatars DIR`.

Not affected: the trusted-phone keys, the SMS inbox token and the drivers'
personal codes each hash with their own secret; `registration_token.auth_value_hash`
holds the code itself.

## The rehearsal, 2026-10-13

`bash rehearse.sh` on the server (owner's OK): **passed**. `check` read 184
values -- 14 passengers, 20 drivers, 143 rides, 7 rider records -- and the
current salt reproduced every stored hash; 17 erased accounts' markers were set
aside. `apply` converted all 184 in **4.5 s** and read each back through the
new keys; the old passetto refused a converted value (422). One passenger
photograph to rename. What it taught, now in the scripts:

- the image's `create_schema.sql` also inserts upstream's demo master key and
  keys: the new key store takes its CREATE statements only;
- an erased account's value is `deleted:<id>`, not ciphertext
  (db/deletion/anonymise.sql in the website): left as it is.

## The order, as planned (to be confirmed by the rehearsal)

1. **Rehearse** on a copy restored by `../restore.sh rehearse` (its own
   network, no ports): a second passetto with new keys (`launcher.sh init
   <password> <keys>`, then `server` with `MASTER_PASSWORD`), `rotate.js check`,
   `rotate.js apply`, then prove a passenger and a driver sign in as the same
   person they were. Measure how long `apply` takes.
2. **Prepare**, nothing switched: the new salt and master password generated on
   the server into `/opt/ny/secrets` (never printed, never in git); the new
   passetto's database on its own volume, initialised.
3. **The window** (~10 minutes; the owner's OK and a time): a fresh backup;
   stop what reads or writes numbers; `rotate.js apply`; switch passetto to the
   new keys, mount the two dhall files with `MOVIN_HASH_SALT` (`up -d
   --no-deps passetto rider-app driver-app`: without `--no-deps` compose also
   recreates Postgres, seen in the first CI proof), update
   `NUMBER_HASH_SALT`; drop the cached people in Redis (a cached row holds the
   old ciphertext); start again; prove sign-in on both sides and both countries.
4. **Rollback**, if any proof fails: restore the window's backup with the old
   passetto and the old salt (`docs/runbooks/rollback.md`).

Answered 2026-10-13: these tables are read from Postgres, not served from
Redis -- the deployed configuration (03a7531) caches configs only
(`configsExpTime`), and the live Redis holds no person or rider row (only the
fleet's positions). Dropping person keys after the switch stays in the window
as a precaution. The configuration change is released as
`compose.keys.yml`, shipped like any stack file and switched on by naming it
in `.env`. The proof that a passenger and a driver sign in as the same people after
the switch is `ci-proof.sh`, run by the ride regression on every push.

## The live switch, 2026-10-13

- **20:57 UTC, `window.sh`**: ended « done: new keys and salt are live ». Every
  number converted and read back; the test passenger signed in as the same
  person; no crypto error in any log.
- **Then a ride search failed** (500: passetto's `/decrypt/obj` refused a
  value). `merchant_service_config` keeps the services' credentials -- maps,
  SMS, WhatsApp -- encrypted inside its JSON: 37 values in 20 rows, which the
  inventory had missed (it looked for `*_encrypted` columns), and which
  `ci-proof.sh` did not catch (it re-tested sign-in, not search). Fixed the same
  hour: `rotate.js configs`, `window.sh configs` (run by the owner ~21:05),
  the cached configs dropped. The switch, its rollback and `ci-proof.sh` now
  include it, and the proof prices a ride in each country after the switch.
- **After**: a complete test ride in each country (`probe-two-country-rides.py`):
  Mauritania 102 MRU, Algeria 917 DA, both COMPLETED.

**The next morning (2026-10-14): the backups.** The nightly backup of 00:32
UTC still dumped `ny-passetto-db` by name -- the old keys -- beside numbers
already under the new ones, and the new keys' master password and salt were
in no backup at all. `backup.sh` and `restore.sh` now take the key database
from the live passetto's connection string and carry `key-secrets.env`; a
backup taken after the fix was rehearsed: 73 s, 20 of 20 numbers decrypted.
The 00:32 backup is not a usable restore point for the numbers.

Left on the server, on purpose: `ny-passetto-db` (today's old keys, the
rollback -- `window.sh rollback`), and `/root/key-rotation.*` (the window's
safety dump). Both can go once the new keys have run without trouble for a
while.


**2026-10-15: the overlay had fallen out of `COMPOSE_FILE`.** The website's
release of 14 Oct ran its `install.sh`, which wrote `.env`'s `COMPOSE_FILE`
as exactly the stack plus its own overlay, and dropped `compose.keys.yml`.
Nothing broke -- running containers keep the settings they were created with
-- but the next `docker compose up` of passetto, rider or driver would have
brought them back on the old keys and salt, and no number could have been
read. Found by the morning check (`docker compose --dry-run up -d --no-deps
passetto rider-app driver-app` said *Recreate* for all three); the file was
put back on the line the same morning, and the same dry run then said
*Running* for all three: nothing restarted. Since then the website's
installer keeps every file the line lists, and `ops/release-remote.py`
refuses a release (and `plan`, the checks and `status` say so) while
passetto runs on the new keys and the line does not name this overlay.
