# Tests and CI

Every automated test, the three workflows, and what a red tick does and does not mean.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Tests, and what CI actually runs

> **Since early November 2026, none of the three runs** (measured 2026-11-09).
> GitHub disabled the workflows on this repository because it is a FORK of
> upstream Namma Yatri (« Workflows on this fork have been disabled to reduce
> compute », on the Actions tab). The last push that started a run was
> 2026-10-25; the last scheduled ride regression, 2026-11-02 (green); pushes on
> 3 and 5 November and every push since started nothing, and said nothing.
> **The owner chose not to re-enable them (2026-11-09).** Until that changes:
>
> - **the gate is `bash tests/run-all.sh` on the laptop** — the same 29 files
>   `algeria: node tests` ran, nothing missing (37 since 2026-11-16);
> - **nothing replaces the ride regression before a release.** After one, the
>   whole ride on the server ([release runbook](runbooks/release.md), step 4.3)
>   is the ride check — and it needs the simulated fleet, so after the launch
>   clean-up there is none;
> - **a Haskell rebuild is impossible** until the workflows are re-enabled:
>   `algeria: build backend` is one of them. Re-enable on the Actions tab of
>   `nammayatri-algeria/nammayatri` (the green button) before planning one.
>
> The table below is what each workflow does when it runs.

Three workflows, none of which deploys anything:

| Workflow | What it proves | When |
|---|---|---|
| `algeria: node tests` | **Every** test in `tests/` (`run-all.sh` globs), on Node 20 and 22: sign-in rules, the push relay, the release rehearsal — and the money path (`wallet`, `restricted`, `deletion`, the guard's `WALLET_EMPTY`) with its SQL run in a real Postgres (PGlite, in-process) | Every push to `algeria/**`, and pull requests |
| `algeria: ride regression` | A whole backend, brought up from nothing on a throwaway runner from **the image the server runs**, with real routing on an Algiers + Nouakchott map. Then the **live two-country layout** from the server's own SQL files, and a priced ride in each country — `+213` in Algiers, `+222` in Nouakchott — each priced **only by its own country's merchant** | Every push to `algeria/**`, Mondays 06:00 UTC, and on demand |
| `algeria: build backend` | The Haskell binaries. 44 minutes; nothing else triggers it | Push to `algeria/build-backend` |

```bash
(cd tests && npm ci) && bash tests/run-all.sh   # every test -- THE gate while CI is off
node tests/wallet.test.js              # the money path: canWork, top-up, credit, the day
./setup.sh price                       # sign in, ask for a priced ride
```

**The ride regression was red from its first run (2026-09-20) to phase 4
(2026-10-06), and never about the change that triggered it.** It ran with
`SKIP_OSRM=1`, on the theory that mock-google would stand in for routing. It
cannot: the mock in the image we run (upstream `03a7531`) has **no
`/directions/json`**, answers 404, and the rider turns that into
`E500 GOOGLE_MAPS_API_ERROR` — so every search failed with *"ride search
returned no searchId"* before a price was ever computed. Read in the rider's
log, not the job's: the job only said FAILED.

Now the job routes as the server does — rider → `maps-shim` → OSRM. It cuts
Algiers out of the Geofabrik extract with `osmium` (cached a week; a 93 MB
graph built in seconds), so a red tick means **a ride in Algiers could not be
priced** by a stack built from this repository. `SKIP_OSRM=1` still brings a
stack up without the graph, but it now skips the price check and says so.

Its trigger was a list of paths; four days of commits went by without it
running. It now runs on **every push to `algeria/**`**, and every Monday at
06:00 UTC since 2026-10-08, when `algeria/osrm-routing` became the default
branch of both repositories (GitHub runs schedules only from the default
branch; until then it was upstream's `main`, which lacks this file). The same
day the "Run workflow" button appeared for all three workflows, and upstream's
`stale.yaml` — which would otherwise have started labelling pull requests every
night — was disabled on both.

**Since 2026-10-08 it runs the live layout and tests the live image.**
`setup.sh two-countries` applies `db/mauritania-geofences.sql`,
`db/two-countries-merchants.sql` and both tariffs (through `apply-tariff.sh`)
exactly as the server got them; `setup.sh price-both` then asks for a ride in
each country and checks `atlas_app.estimate.provider_url` — every estimate in
Algiers from `algeria0…`, every one in Nouakchott from `favorit0…`. The first
green run: Algiers 629 / 899 DA, Nouakchott 74 / 74 / 148 MRU. Before that day it
priced one Algiers ride with upstream's seed merchant (fare 258) — and **pulled
the wrong image**: `ghcr.io/<repo owner>/ny-backend`, which on MohaGNPro is the
2026-08-26 build, six weeks older than the server's and unable to sign in a
`+222` number. It now pulls `ghcr.io/nammayatri-algeria/ny-backend:latest`, as
`stack/deploy-backend.sh` does, and prints the digest it tested.

`preflight` accepts a **pulled** image in place of the loose binaries in `bin/`,
which is what the regression job and `deploy-backend.sh` both do.

## The virtual passenger — `maps-shim/virtual-rider.js`, since 2026-11-16

**A passenger who does not exist books real rides for a real driver**, on his
own phone, so the driver side can be tried as often as wanted (the boss,
2026-11-16: « this way we can test the driver side extensively »). It is the
ride test turned around: in `ops/checks/probe-two-country-rides.py` the
simulated cars drive and a script is the passenger; here a person drives and
the script is the passenger. Day 7 of the « Lieux proposés » plan; since
day 8 (2026-11-18) **the office runs it from the console**: Tests → « Passager
virtuel » (admins), a driver online in the country viewed — real ones first,
the simulated cars marked — a scenario, « Lancer » / « Arrêter », the
passenger's lines every 2 s and the verdict. The command line still works:

    docker exec ny-maps-shim node virtual-rider.js "+213 0555 12 34 56" 1 | tee ~/vr-1.log
    docker exec ny-maps-shim node virtual-rider.js stop     # from another terminal
    docker exec ny-maps-shim node virtual-rider.js list

Run by the owner on the server (`ssh ny '…'`): it books on the live stack.
Ctrl-C does not reach the process through ssh; `stop` does (SIGTERM), and the
run then closes what it opened.

**The console's route** is `POST /internal/virtual-rider` in maps-shim
(`virtual-rider-route.js`): `start {driverId, scenario}`, `stop {country}`,
`status`. Loopback and the docker bridge only, like `/internal/driver-push`
(admin-api reaches 8030 through the ufw bridge rule; the edge routes nothing
under `/internal/`: 404 on every public name). **The engine runs as a child
process** (plan decision V8): maps-shim serves the live maps, search and
wallet, and a fault in a test tool must not stop them. One run per country,
whoever started it — a command-line run's pid file also answers 409. Runs are
kept in maps-shim's memory: a restart empties the status (and kills a running
child; the next run cancels what it left). The console's side, its audit rows
(`virtual_rider_start` / `_stop`) and its list of drivers are the website's
`apps/api/src/modules/tests/virtual-rider.ts`. Tests:
`tests/virtual-rider-route.test.js` (a fake child, the real engine as the
child, server.js).

**A stop says the truth** (2026-11-18). It cancels the booking and reads the
answer: before the driver starts, « rien n'est resté ouvert »; after, the
passenger can no longer cancel and the stop says « la course a déjà démarré :
… le chauffeur doit la terminer ». It used to print « rien n'est resté
ouvert » unchecked — true the first time from the console (the server showed
the ride CANCELLED, never arrived), but not by construction.

| # | Scenario | RÉUSSI when |
|---|---|---|
| 1 | a whole ride | COMPLETED, a price (the driver's side, `ride.fare`, since 2026-11-18: the passenger's copy has none yet), the passenger's 5 stars **and** the driver's rating of her (`movin.driver_rider_rating`). **A simulated car never rates**, so with one as the tester the run ends RÉUSSI after the passenger's stars (owner, 2026-11-18); a real driver must still rate within 5 min |
| 2 | the passenger cancels before the arrival | CANCELLED on the driver side; « pas joué » if he arrives or starts first |
| 3 | she cancels after the arrival | the same, 20 s after « Je suis arrivé » |
| 4 | she does not come | the driver cancels (`booking_cancellation_reason.source = ByDriver`); starting the ride is PAS RÉUSSI |
| 5 | she chooses a simulated car, or nobody | his offer was made and not taken; never a real driver's |
| 6 | three requests in a row | three offers of his, three bookings, three cancels |
| 7 | to an approved proposed place | as 1, the place's name as the destination |
| 8 | two requests at once, from two passengers at two points near him (since 2026-11-18, decision V3) | his first offer, on either, gets ONE booking; the other request is closed with no booking (his offer on it, if any, never taken); then cancelled as in 2 |

**What it never does**, each one a test in `tests/virtual-rider.test.js`:

- **run without `VIRTUAL_RIDER=on`** in the server's `.env` (compose passes it
  to maps-shim, `off` by default). On since 2026-11-16. **Off before launch**
  ([launch runbook](runbooks/launch.md), 3b): `launch-gate.sh` says no-go while
  it is on (since 2026-11-18, `tests/launch-gate.test.sh` runs that check on
  and off);
- **call the driver API.** Signing in as a driver revokes the session on his
  phone; everything about him is read from the database;
- **accept another real driver's offer.** Each offer is traced
  `atlas_app.quote.driver_offer_id → driver_offer.bpp_quote_id →
  atlas_driver_offer_bpp.driver_quote.driver_id`; only the tester's is
  confirmed. Other drivers near him see the request; their offers expire;
- **book for a driver dispatch would skip:** validated, not blocked, a vehicle,
  « En service », not on a ride, a position under 10 min old, and
  `wallet.workState().canWork` — the wallet's own rule, one function the
  wallet screen reads too;
- **leave a booking open** (an open one locks the passenger out:
  `ACTIVE_BOOKING_PRESENT`): leftovers are cancelled first, everything on exit
  and on `stop`; one run at a time per country (a pid file in the container);
- **claim a cancel the server refused.** Every answer is read.

Its passengers are the ride test's two test accounts, plus since 2026-11-18 a
second one per country for scenario 8 (`+213 0555000198`, `+222 22778898`;
one passenger cannot hold two bookings) (`+213 0555000199`,
`+222 22778899`), signed in on loopback 8013 with the backend's fixed code;
they are already on `launch-cleanup-targets.sql`. The pickup is ~300 m from
the driver and the arrival ~2 km on, both snapped to a road by OSRM's
`/nearest` (the raw offset can land in the sea at Algiers).

**First live runs, 2026-11-16, with a simulated car as the tester** (no phone
was free). Every column it reads exists on the live database (27/27, checked
in `information_schema`). Scenario 1 went through on the real server: offer
traced, arrival, a start **with no code** (the guard's day-6 rule, proven
live), COMPLETED at 211 DA, the passenger's rating — then PAS RÉUSSI, as
expected, because the simulator never rates a passenger. Scenario 2 found a
bug: **a simulated car reaches a pickup 282 m away in 26 s and starts the
same second**, so the passenger's cancel at 30 s was refused (the ride had
started), and the engine had not read the refusal. Fixed the same evening
(`bfb4f7a854`): every cancel's answer is read, the driver is watched during
the wait, a closed terminal no longer skips the clean-up. **So the simulated
cars can prove scenario 1's chain and nothing else: 2, 3 and 4 need a person.**
