# Drivers — the BPP, the test drivers and the simulator

The driver side of the backend, keeping driver positions fresh, the test drivers and `simulate-driver.py`.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## The driver side (BPP)

Runs from the **same image** — it already contains every executable in the
repo — with a different entrypoint (`dynamic-offer-driver-app-exe`), its own
schema and its own migrations. No second build.

Seeded with 2 merchants, 12 drivers, 12 vehicles and 2 fare policies, and
`verify` registers and logs in a new driver on every run.

**Seeding order is not interchangeable**, and getting it wrong fails in a way
that is hard to read later:

1. `sql-seed/dynamic-offer-driver-app-seed.sql` — schema + 13 base tables
   including `organization`. Contains **no data**.
2. `local-testing-data/dynamic-offer-driver-app.sql` — organizations, drivers,
   vehicles, fare policies, inserted into `organization`.
3. Migrations, applied by driver-app at startup. Migration **0050**
   (`rename-org-to-merchant`) renames `organization` → `merchant`, carrying
   those rows across.

So the data must be loaded **before driver-app starts**. Load it afterwards and
every insert fails, because `organization` no longer exists. This is the same
trap as the rider side, which is why `local-testing-data/rider-app.sql` is
deliberately never applied.

> Driver auth takes the merchant **UUID**, not the `shortId` the rider side
> uses. An unknown number is fine — `auth` calls `createDriverWithDetails`, so
> registration and login are the same call.

## Driver freshness — `./drivers-keepalive.sh`

**Uninstalled on the live server 2026-10-01**, with the simulated fleet and
every test account (see *[The test fleet](countries.md#the-test-fleet)*). It re-stamped every row of
`driver_location`, real drivers' included, so an offline real driver's last
position looked fresh to the dispatcher. The trap below is real again on a dev
stack; on the live one, positions now come only from driver apps.

**The single most misleading failure in this stack.** The dispatch pool only
considers drivers whose recorded position is recent. Real drivers send one
constantly; the seeded ones are rows nobody updates. So a stack that worked
yesterday returns **zero estimates today, with no error anywhere** — empty
arrays, HTTP 200, nothing in any log — and it looks exactly like broken
dispatch.

Measured on 12 Aug: six drivers within 600 m of the pickup, every one invisible,
positions **1 day 21 hours** old. It has cost time twice.

```bash
./setup.sh drivers              # place them, once
./drivers-keepalive.sh install  # keep them visible, every 2 minutes
./drivers-keepalive.sh status   # is the timer up, how fresh are they
```

The timer only re-stamps rows `setup.sh drivers` already created — it does not
move anyone, so a driver placed by hand for a test stays where they were put.

**It is a demo prop, not a fix.** The real fix is a driver app sending real
positions, and on the day that exists this should be *deleted* rather than left
quietly keeping fictional cars alive next to real ones:

```bash
./drivers-keepalive.sh uninstall
```

Also worth knowing for anyone testing by hand: a rider only reaches drivers
whose **vehicle matches the variant they picked**, and the seeded fleet is 9
auto-rickshaws, 2 sedans, 1 hatchback and 1 SUV. "SUV" therefore reaches exactly
one driver, and dispatch will look unreliable for reasons that are nothing to do
with dispatch.

## Two test drivers, at the two ends of the journey

Kept as a pair because one account cannot show both paths: the duty screen sends
a driver with no vehicle back to his file, correctly, so the working loop is
unreachable from an unapproved account.

| Number | State | Vehicle |
|---|---|---|
| `0555000001` | not approved — lands on the file screen, must file papers | none |
| `0555000002` | approved, `Yacine` — lands on the duty screen, can go online | SEDAN · Hyundai Accent Blanc · `06182 118 16` |

Their personal codes are **not written here**: the guard keeps a salted hash and
prints a code once, and this file is in git. `./enrol-driver.sh --list` shows
who is enrolled; `--set <number> <code>` sets a new one.

### The server does not join a driver's name, and it was blamed for doing so

`POST /ui/driver/profile` takes `firstName`, `middleName` and `lastName` and
writes the three columns separately — `updateDriver` assigns
`firstName = fromMaybe person.firstName req.firstName` and nothing composes
them. Worth writing down because the opposite was the obvious explanation for
this, measured 2026-09-02 across every driver ever enrolled from a handset:

```
first_name    last_name
Moha Gefl     Gefl        <- filled both ways
Test Test     Test        <- filled both ways
Mohamed       Gn          <- filled as intended
Moha          Gb          <- filled as intended
```

Each row is exactly what was typed. The app asks for the name in two boxes
under a heading reading *Votre nom*, with the second marked *Facultatif*, and
half the people who met it wrote the whole name in the first box and the
surname in the second. Everything downstream then joined the two columns — the
app's dossier and profile, and the agency console, which joins the same two —
and printed **Moha Gefl Gefl**.

Fixed on both sides by making the join idempotent (a surname the first name
already carries is not added again) and by stripping the repetition on the way
in, so no new row is written in that shape. The two rows above are still wrong
in the database; they are test profiles and both sides display them correctly
without being touched.

The lesson is the cheap one: **four rows of real data answered this, and a
probe would have taken longer and told us less.** The temptation was to POST a
known pair and read it back, which measures the server — and the server was
never the thing in doubt once the rows were read as *typed input* rather than
as output.

To approve a driver the way the agency does — both switches, plus the vehicle
that dispatch actually matches on:

```sql
UPDATE atlas_driver_offer_bpp.driver_information
   SET enabled = true, verified = true, blocked = false WHERE driver_id = '…';

INSERT INTO atlas_driver_offer_bpp.vehicle
  (driver_id, capacity, make, model, variant, color, registration_no,
   merchant_id, vehicle_class, created_at, updated_at)
VALUES ('…', 4, 'Hyundai', 'Accent', 'SEDAN', 'Blanc', '06182 118 16',
        (SELECT merchant_id FROM atlas_driver_offer_bpp.vehicle LIMIT 1),
        '3WT', now(), now());
```

~~`enabled` and `verified` are separate switches and the pool skips a driver
missing either.~~ **Not true, and worth knowing exactly which of the three
matters where.** Read from `Storage/Queries/Person.hs` and
`Domain/Action/UI/Driver.hs` at `03a7531`, the ref these binaries were built
from:

| Column | Read by | Effect |
|---|---|---|
| `blocked` | `setActivity` **and** `getNearestDrivers` | cannot go online, and skipped by the pool |
| `enabled` | `setActivity` only | cannot go online — `DRIVER_ACCOUNT_DISABLED` |
| `verified` | **nothing at all** | none |

`getNearestDrivers` filters on role, merchant, `active`, not-`blocked`, position
freshness and vehicle variant. It never looks at `enabled` or `verified`.

Three consequences.

**A driver created by `POST /ui/auth` starts `enabled = false`**, which is why
enrolling is not enabling. Watch out for the near-miss here: there are **two
functions called `createDriverDetails`**, and they disagree. `Registration.hs`
— the self-signup path `/ui/auth` uses — writes `enabled = False`. `Driver.hs`
— the office path — writes `enabled = True`. Reading the wrong one produces the
confident and wrong conclusion that a fresh driver can work immediately.

**`verified` is not what makes an account work.** Nothing in the backend reads
it. The app uses it as *"the agency has checked the papers"*, which is a product
convention this stack invented; the approve SQL above must therefore keep
setting it, or D7 holds a driver who could actually work.

**Disabling by SQL does not put a driver offline.** The dashboard route does
(`changeDriverEnableState` calls `updateActivity … False` when disabling), but
`/dashboard/` is not published here, so accounts are switched off with raw SQL —
and that leaves `active = true`. `setActivity` is a gate, not a leash: nothing
revokes a flag already set, so he keeps receiving work until he next toggles.
**Set `active = false` in the same statement.**

`registration_no` is unique — reusing a plate fails the insert.
`vehicle_class = '3WT'` is copied from the fleet rows known to dispatch
correctly; it reads wrong for a sedan and is an upstream artefact.

## Playing a driver — `./simulate-driver.py`

```bash
./simulate-driver.py seed      # one Algerian driver per row the app sells
./simulate-driver.py status    # who exists, who is online, how fresh
./simulate-driver.py once      # take the next request, drive it, finish
./simulate-driver.py daemon    # all three online, keep accepting
```

Runs **on the server** — `/ui/` is loopback-only, for the reason above.

There is no driver app, and screens 10–13 of the passenger app cannot be built
or demonstrated without something on the other side. This drives the real
endpoints against the real backend, so what the app sees is what it will see in
production. It also **drives the actual OSRM route**, which is the part that
makes a moving car on the passenger's map testable rather than imagined.

```
11:51:28 HATCHBACK driver taking a request
11:51:28   accepting 8003ac71 -- 14.0 km, base 641 DZD
11:51:32     ride h36FJtMGuf assigned
11:51:32     to the pickup: 35 points, 4.1 min of real driving
11:51:36     started with the passenger's code 2240
11:51:36     to the destination: 329 points, 19.3 min of real driving
11:51:56     finished -- 641 DZD
```

`--speed` is a multiplier on real driving time: `1` is real time (19 minutes for
the standard 14 km test trip), `0` teleports, and the default `8` is roughly
demo pace. `--decline N` turns down the first N requests so that path can be
built too. `--variant` restricts `once` to one row.

### The one shortcut, kept visible

It reads the ride OTP out of Postgres. A real driver is told the code by the
passenger, and `/ui/driver/ride/list` deliberately does not carry it. That is
the entire difference between this and a real driver, and it is better stated
than hidden behind something that looks complete.

### Why `seed` exists — dispatch matches on vehicle variant

**A search only ever reaches drivers whose vehicle variant matches the estimate
the rider picked.** Before this, the only Algerian driver was a `SEDAN`, so of
the three rows the app sells:

| Row | Variant | Before | After `seed` |
|---|---|---|---|
| Economy | `HATCHBACK` | nobody | `0551234568` |
| Comfort | `SEDAN` | `0551234567` | unchanged |
| Premium | `SUV` | nobody | `0551234569` |

Two of the three rows spun for the full 300 s and returned nothing, **with no
error on either side** — it presents exactly like broken dispatch. The remaining
seeded drivers are upstream's, with `+91`/`+94` numbers that driver auth rejects
outright (`mobileCountryCode matches regex /^\+213$/`), so nothing can log in as
them.

`0551234567` is left as a `SEDAN` on purpose: he is the driver every earlier
probe was proven against, and `setup.sh`'s smoke test recreates him on login.

### Keep it running — `./fleet-service.sh`

**Uninstalled on the live server 2026-10-01**: the simulated cars answered real
ride requests in Nouakchott. Dev stacks only.

```bash
./fleet-service.sh install     # run the fleet, and keep it running
./fleet-service.sh status      # up? and what has it done lately
./fleet-service.sh uninstall   # stop and remove
```

**Cars on the map and no offers is this, every time.** The two are produced by
completely different things and only one of them was ever automated:

| The rider sees | Needs |
|---|---|
| Estimates, and cars drawn on screen 10 | fresh rows in `driver_location` — the `movin-drivers` timer does this every 2 min |
| An actual **offer** | a *process* polling the driver API and answering — `simulate-driver.py daemon` |

So with the timer running and the simulator not, a search succeeds, prices come
back, screen 10 draws three cars near the rider — and then nobody ever bids. It
looks exactly like broken dispatch, and it is not: there is simply no driver.

That state persisted for hours at a time because the simulator had only ever
been started by hand, usually wrapped in `timeout`, so it always died later.
`fleet-service.sh install` makes it a systemd unit with `Restart=always`, so it
survives a reboot, a crash, and a stack restart.

Stopping it is clean: the script turns `SIGTERM` into the interrupt its own
cleanup handles, so the drivers go **offline** rather than being left online
with positions that then go stale.

### Two behaviours worth knowing before changing this

**A declined request keeps appearing.** After `respond` with `Reject`, the same
search stays in `nearbyRideRequest`. Poll, decline, poll again and you will be
handed the one you just refused; accepting it then fails with
`QUOTE_ALREADY_REJECTED`. The simulator remembers what it declined.

**Killing it leaves the drivers online.** `finally` does not run on `SIGTERM`,
so `timeout`, `docker stop` or a systemd restart used to leave the fleet
marked online whose positions then went stale — which is the silent
zero-estimates failure in [Driver freshness](#driver-freshness--drivers-keepalivesh)
all over again. `SIGTERM` and `SIGHUP` are now turned into the interrupt the
cleanup already handles.

While it is running it also heartbeats its own drivers' positions every 30 s,
so for those six it does `drivers-keepalive.sh`'s job.

### An unfinished ride locks that rider out — `./simulate-driver.py finish`

This is the most expensive trap in the whole stack, because the symptom points
squarely at the wrong thing.

**One ride left open ends every future booking for that account.** Confirming
any new quote while a booking is still open answers

```
E400 INVALID_REQUEST: ACTIVE_BOOKING_PRESENT
```

and nothing else about the flow changes. The search runs. Estimates come back.
Drivers offer. Cars appear on the map. Every single tap is refused.

Measured 2026-08-18: a test booking from **10 August** sat in `TRIP_ASSIGNED`
for eight days. On the 18th a tester tapped five different drivers, watched
nothing happen five times, and reported the app's button as dead. The server had
said exactly what was wrong on all five, in the log, at the time.

```bash
./simulate-driver.py finish --speed 0     # close out everything hanging
```

It completes rather than cancels: the real end-of-ride path runs, the rider gets
a finished trip in their history, and screen 14 has something to rate. `--speed 0`
teleports, so a fossil costs about two seconds.

The daemon will never do this for you. `my_active_ride` ignores anything older
than 30 minutes, deliberately, so a fossil cannot hijack a live session — which
is correct there and the reason `finish` is separate.

**Two things this trap taught, both now fixed in the script:**

*Logging in as a driver revokes that driver's other session.* One session per
user applies to drivers exactly as it does to riders. So running `finish` while
`movin-fleet` is up used to pull the daemon's tokens out from under it — and the
daemon could not tell, because a 401 made `poll()` return `None`, which is what
it also returns when there is simply no work. It looped silently for sixteen
minutes, `systemctl status` said `active` the whole time, and searches came back
with cars on the map and no offers. It now recognises a revoked session and
signs back in.

*`run_ride` could not resume an `INPROGRESS` ride.* It called `arrived/pickup`
and `start` unconditionally, and `start` on an already-started ride is not a 200
— so it gave up and returned `False`. That is exactly what a daemon restart
mid-trip produces, which means the fix for ghost rides was also quietly creating
them.

**The daemon is single-threaded.** `run_ride` blocks the entire loop, so while
one driver is driving, *no* driver answers anything. At `--speed 3` a 16-minute
trip is five and a half real minutes of a fleet that offers nothing. On one
phone that is invisible; it is worth knowing before blaming dispatch again.
