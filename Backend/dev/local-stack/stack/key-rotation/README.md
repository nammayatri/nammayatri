# Key rotation: the keys and salt that protect stored numbers

Plan point 5, before the first real passenger. Prepared 2026-10-13; **not run
on the live server yet**. Every step on the server needs the owner's OK, the
rehearsal included.

Phone numbers (and a few document numbers) are stored twice: encrypted by
passetto, and hashed — `sha256(salt + number)` — so the backend can look a
number up without decrypting anything. Rotating means new passetto keys AND a
new salt, and every stored value converted in one step: a number encrypted
under the old keys, or hashed with the old salt, would no longer match anyone.

## What is here

| File | What it is |
|---|---|
| `rotate.js` | The conversion. `check` reads every value and proves the current salt reproduces every stored hash, writing nothing. `apply` re-encrypts with the new passetto, re-hashes with the new salt, all rows in one transaction, then reads every value back through the new keys. Renames the passengers' photographs, whose file name is the hash. Tested: `tests/key-rotation.test.js`. |
| `rider-app.secrets.dhall`, `dynamic-offer-driver-app.secrets.dhall` | Upstream's secrets files at `03a7531` (what the images run), byte for byte, except that `encHashSalt` is read from `MOVIN_HASH_SALT`. Mounted over the upstream files on the day; nothing reads them before. |

## What uses the salt, and must change on the same day

- the rider and driver apps: `encHashSalt` (the two files above);
- `maps-shim/number-change.js`: `NUMBER_HASH_SALT` in `/opt/ny/secrets/number-change.env`;
- the passengers' photographs: `maps-shim/avatars.js` names each `h_<hash>` — renamed by `rotate.js apply --avatars DIR`.

Not affected: the trusted-phone keys, the SMS inbox token and the drivers'
personal codes each hash with their own secret; `registration_token.auth_value_hash`
holds the code itself.

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
   new keys, mount the two dhall files with `MOVIN_HASH_SALT`, update
   `NUMBER_HASH_SALT`; drop the cached people in Redis (a cached row holds the
   old ciphertext); start again; prove sign-in on both sides and both countries.
4. **Rollback**, if any proof fails: restore the window's backup with the old
   passetto and the old salt (`docs/runbooks/rollback.md`).

Open questions for the rehearsal: whether any of these tables is served from
Redis (KV) rather than read from Postgres, and how the configuration change is
released (the dhall files under `2023/` are not shipped by `ops/deploy.sh`).
