# Runbook — launch

The launch plan as a procedure: from the day the boss names the date, to the end of the first week. It is the plan the owner and the boss agreed, written down with the commands that carry each step and what to read in their output. Written 2026-10-18, before the date was set.

Two things govern everything below.

- **A production step is the owner's to run.** Anything that changes the live server (a release, an erase, a restore) goes through the owner, with one explicit OK for each, after a dry run that says what restarts and who notices ([release runbook](release.md)). The tools here print a dry run first and change nothing until `--apply`.
- **One batch, one proof.** Every measurement in this project was taken against the binaries that run today, and a backend rebuild is 44 minutes cold ([ours and upstream's](../ours-and-upstream.md)). Before launch, fix what is wrong with config, SQL or a shim; do not start a backend rebuild in the last days unless something forces it.

The notice the stores need is set by the slowest of: Google Play's account type, Apple's review and the reviewers' sign-in (the app repository's `docs/store-readiness.md`). Ask for the date with that in hand.

## 1. When the date is known — the final versions

1. **Everything green.** On the day: `(cd tests && npm ci) && bash tests/run-all.sh` (23+ files), CI green on the commit the server runs ([testing](../testing.md)), and the console's **Tests** page: all 16 scenarios, English and Arabic.
2. **Server = git.** `Backend/dev/local-stack/ops/deploy.sh status` says `no: all … shipped files are exactly as released`. If someone edited the server by hand, the next release stops: find out who and why before launch, not on it.
3. **The apps** (the app repository, `docs/build-and-release.md` and `docs/store-readiness.md`): `before-ios-build.sh`, the iOS build and its TestFlight upload, `VARIANT=Release FORMAT=aab ./scripts/build-android.sh`, `verify-apk.py` on the result, then the store submissions, the store pages and the privacy labels.
4. **The privacy page matches the app.** The website release and the in-app text go together (`docs/store-readiness.md`, section 4).
5. **No release in the last 48 hours** except for a fault. A change that is not needed is a change that is not proven.

## 2. While the stores review

**First drivers.** They sign up in the app (an unknown number is texted a code, or signs in by WhatsApp), send their papers, and the agency decides on the console's **Validation** page. A driver cannot work until he is enabled **and** has credit for a day: no top-up, no work ([wallet](../wallet.md), *No top-up, no work*). So a first driver's first act is a top-up, which is the next check.

**The top-up, with a small real payment.** One in each country, by a person who will not mind the amount: the smallest top-up is two days, **60 MRU** (Moosyl) and **200 DA** (Chargily). Run `sudo ./launch-gate.sh` first: it fails if Chargily is still on its test address or a test key. Then, after paying:

- the console's **Porte-monnaie**: the top-up is there, and the driver has his day;
- `select count(*) from movin.wallet_check where drift <> 0` is `0` (the gate asks it);
- the owner's bot says « rechargement reçu »;
- the driver can go online, and a search from another phone finds his car.

**One real ride in each country**, a real driver and one of us as the passenger, on real phones with the store builds or the release APK. After it: the ride is in the console, the receipt is right, and the driver was charged his day once (30 MRU / 100 DA), not twice. Watch for the two things that failed before: no estimate (his position is stale, so he is ignored) and an offer nobody answers.

**Reviewers' sign-in** (the app repository's `docs/store-readiness.md`, section 2): if a review number is used, it is added to `SMS_BYPASS` the day before the submission and **removed the day the review ends**. The gate fails while any bypass is open.

## 3. The day before — clean the server

The plan's three jobs, in this order. Each of the first two has a rehearsal.

### 3a. Rehearse on a copy (any time before; nothing live is touched)

    cd /opt/ny/local-stack
    REHEARSE_HOOK='./launch-cleanup.sh --rehearsal "$R_PG" --apply' \
      ./restore.sh rehearse offsite:latest

This restores last night's off-site backup into throwaway containers (about 70 seconds), proves the restore, runs the clean-up **on that copy**, and removes the copy. Read the clean-up's output for: *the accounts to remove* (12 drivers and 2 passengers, or fewer if some are already gone), *what stays* and any warning, and *none of the listed numbers can be found any more*. It also proves the backup itself can be put back, which is the last item of this day.

**Rehearsed 2026-10-18, passed (105 s).** On the backup of 20261018T003335Z (the 106 tables exact, 20 numbers decrypted, 25 documents): the script found exactly **12 drivers and 2 passengers**, with the number formats the simulator had written (Algerian drivers `+213` with the trunk zero, Mauritanian `+222` without; the two passengers `+213 0555000199` and `+222 22778899`); no uploaded papers; **39 wallet-ledger entries** belonging to those cars, which stay by design (see below). Each erase touched 6 rows per car and 4 per passenger, left every person row in place (56 drivers and 84 passengers before and after), wrote 14 audit rows and found none of the numbers afterwards. 6 drivers still have a number on record: the real ones.

### 3b. Remove the 12 simulated cars and the 2 test passengers

    sudo ./launch-cleanup.sh              # who would go, and what stays. Writes nothing.
    sudo ./launch-cleanup.sh --apply      # does it

`--apply` takes a fresh backup first, dumps the three account schemas into `/root/pre-launch-clean/` (root-only: it is the way back if a number was wrong, and it holds **real** people's data), stops and removes the simulated fleet **before** erasing (otherwise the daemon signs the cars back in), erases each account with the console's own `anonymise.sql` (one transaction each, an audit row each), drops their Redis sessions and checks again. It asks you to type `erase 14 accounts`.

What it does **not** do, and you do:

- **Papers on the documents volume.** If a target has uploaded papers it stops and names the files; delete them, then re-run with `--papers-handled`.
- **Profile photographs** on the maps-shim avatars volume, keyed by person id: the simulated cars have none unless someone gave them one.
- **`auth-guard/driver-codes.json`**: the script warns if a target's number is in it.
- **The rides and the wallet ledger stay**, on purpose ([deleting an account](../account-deletion.md)). The console keeps counting the test rides and the test day-charges (« test accounts are counted, not excluded », the owner, 2026-09-15). **A decision for the owner:** keep them as the history of the pilot, or have the console count from the launch date. It is the console's business, not the script's.

### 3c. Remove the temporary safety copies

What a person can find lying around from the phone-number protection work and the earlier clean-ups. Look first; remove each only after deciding it is no longer the way back:

    ls -la /root /opt/ny/secrets /var/backups/movin /opt/ny
    ls -la /root/pre-removal /root/pre-launch-clean 2>/dev/null

| What | Why it exists | When it can go |
|---|---|---|
| `/root/pre-removal/` | dump of the account schemas before the 2026-10-01 erase, numbers under the **old** keys | once the launch has settled: it can no longer be read after the key rotation without the old store, and it holds real data |
| `/root/pre-launch-clean/` | dump taken by 3b | after the first week without a problem |
| `/opt/ny/secrets/*.before-*` | the pre-launch `SMS_BYPASS` and driver-codes files | now: they list numbers that no longer exist |
| `/opt/ny/local-stack.prev` | the one-level rollback of the last release | never by hand: the next release replaces it |
| `ny-passetto-db`, `/root/key-rotation.*` | the old key store and the rotation's work folders | **already removed**, 2026-10-15 (`key-rotation/cleanup.sh`) |
| Drive `movin-backups-old` | backups from before the Drive client changed, browser-download only | the owner's decision, in the browser |

### 3d. A fresh backup, and proof it can be restored

    sudo ./backup.sh
    ./restore.sh rehearse offsite:latest

The second command is 3a without the hook. It must end *rehearsal passed*, with every table's rows exact and the sample of phone numbers decrypted. A backup nobody has restored is a hope.

### 3e. The gate

    sudo ./launch-gate.sh

It is read-only and ends **NO FAIL** or the list of what blocks the launch: a container down, a health check, Chargily on test, a bypass open, the fleet still there, a wallet that disagrees with its ledger, a backup older than 26 hours, a key that no longer opens the numbers, a certificate near its end, the disk. A **WARN** is read and decided on; a **FAIL** is not launched over. Run it again in the morning.

## 4. Launch day

- The apps go live; the owner and the boss do that in App Store Connect and the Play Console.
- **Nothing is released to the server** unless a user-facing fault needs it, and then one change, with the owner's OK, a dry run, and the [release runbook](release.md).
- **Watch**, in this order: the owner's bot (`/jour`, `/serveur`, `/flotte`); the console's **Courses**, **Validation**, **Signalements**; the first rides end to end.
- What a problem looks like, and what it usually is:

| You see | Usually |
|---|---|
| a passenger finds no car, no error | no driver is online, or his position is stale: dispatch ignores old positions. Nobody to fix; ask a driver to go online |
| an offer is made and never answered | the driver's app is in the background without the location permission, or he has no credit for the day |
| « compte bloqué » on a driver's phone | the profile is cached: drop `driver-offer:CachedQueries:DriverInformation:DriverId-…` in Redis (the backend clears it before it writes the row, so a read in between can re-cache the old state) |
| sign-in codes not arriving in Mauritania | the SMS budget or the gateway: the bot says which; Algeria has no SMS by design (WhatsApp, or an SMS the person sends) |
| a top-up paid and not credited | the gateway's webhook: read `docker logs ny-maps-shim` and the top-up row's `status`; never credit by hand without the ledger entry |

- If a release made it worse: [rollback](rollback.md). A rollback does not undo SQL.

## 5. The first week

**Every morning, ten minutes:**

1. The bot's overnight messages, then `/jour`.
2. `sudo ./launch-gate.sh` on the server, and `Backend/dev/local-stack/ops/deploy.sh status` from the laptop.
3. The console, in this order: **Validation** (drivers waiting: the answer a driver is owed is a decision, not a queue), **Suppressions** (an account-deletion request is a 30-day promise: do it, then the two backend steps on that page), **Signalements** (a passenger's report about a driver, possibly one still on the road), **Notes** (low ratings with a comment), **Annulations**, **Porte-monnaie** (negative balances, a top-up that did not arrive).
4. Write down every complaint with the time, the country and the phone model: the fixes are chosen from this list, not from the loudest message.

**Once, on the third day:** `./restore.sh rehearse offsite:latest` again (the backups now contain real people).

**Fixes:** batch them. Config, SQL and shims first; each release alone, with its dry run and its proof from outside. An app fix is a store build and a review, so it waits for a batch worth a build, unless it blocks a ride.

**On the seventh day:** remove `/root/pre-launch-clean/` if nothing sent anyone back to it, and write what the week taught into [gotchas](../gotchas.md).

## What is not in this runbook

- Store accounts, listings and screenshots: the app repository's `docs/store-readiness.md` and `docs/store-listing.md`.
- The Moorsyl sender and template for « Mova » (the sign-in keyword is `MOVA` now, `MOVIN` still read): [the rename note](../gotchas.md#the-product-is-called-mova-2026-10-18--what-moved-here-and-what-did-not).
- Anything about the Beckn gateway or the registry: they have not changed since the pilot.
