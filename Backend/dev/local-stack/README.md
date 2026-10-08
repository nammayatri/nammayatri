# local-stack — self-hosted Namma Yatri rider backend

Brings up a **working** rider-app backend (API + Postgres + Redis + Kafka +
encryption service) in Docker, with a seeded merchant and a test rider, so the
full login flow works end to end.

```bash
cd Backend/dev/local-stack/stack
./setup.sh
```

**Where things are (since 2026-10-06).** Everything the server runs is in
[`stack/`](stack/) — it mirrors `/opt/ny/local-stack` on the VPS, and it is the
only folder a release copies. The SQL is in `stack/db/`. Nothing outside
`stack/` is deployed: `ops/` is run from the laptop, `investigations/` holds the
probes that explain how the system was understood, `retired/` the scripts that
made test accounts, `tests/` the automated tests. See *Layout* at the end.
Commands written as `./x.sh` are run from `stack/`.

**The documentation is in [`docs/`](docs/), one page per subject** — see
*Where everything is* below. This README was 4 127 lines until 2026-10-08
(phase 7); every section moved word for word, and the index below says where.

First run takes ~10 minutes (it compiles librdkafka). After that, `docker compose up -d`
starts everything in seconds.

When it finishes you get:

```
POST /v2/auth                  200  authId=…
POST /v2/auth/{id}/verify      200  token=…
POST /v2/serviceability/origin 200  Algiers      serviceable=true
POST /v2/serviceability/origin 200  Bangalore    serviceable=false
*** Backend is fully operational ***
```

| URL | What |
|-----|------|
| `http://localhost:8025` | **Service-area map** — click anywhere, the backend answers |
| `http://localhost:8014/swagger` | Rider (BAP) Swagger UI — 60 endpoints (**no trailing slash**) |
| `http://localhost:8014/openapi` | Rider OpenAPI spec (JSON) |
| `http://localhost:8017/swagger` | **Driver (BPP) Swagger UI — 99 endpoints** |
| `http://localhost:8017/openapi` | Driver OpenAPI spec (JSON) |
| `localhost:5434` | Postgres (`postgres` / `root`, db `atlas_dev`) |

Two schemas in one database: `atlas_app` (rider) and `atlas_driver_offer_bpp`
(driver).

Demo script for showing it works: `./demo.sh` (or `demo.ps1` on Windows).

---

## Where everything is

| Page | What it covers |
|---|---|
| [Countries — two of them, on one stack](docs/countries.md) | The service areas, the move to Mauritania, and running Algeria beside it: merchants, geofences, phone rules, tariffs per country. |
| [Maps — routing, tiles and place search](docs/maps.md) | Our three replacements for Google: OSRM for routes, tileserver-gl for the map picture, and the place index behind `maps-shim`. |
| [On the internet — TLS, the domain, and what a person may send](docs/edge.md) | The nginx edge: certificates and the lock that had to come first, the domain switch, and the size limits on everything a caller can send. |
| [Fares and dispatch](docs/fares-and-dispatch.md) | What a ride costs in each country, how "a car is near" is decided, and how a driver is chosen. |
| [Drivers — the BPP, the test drivers and the simulator](docs/drivers.md) | The driver side of the backend, keeping driver positions fresh, the test drivers and `simulate-driver.py`. |
| [The driver API — what the deployed binary really does](docs/driver-api.md) | The routes the driver app calls, measured against the running binary rather than read from the source tree; driver documents; a ride from the driver's side. |
| [Riders — the rider API and ratings](docs/riders.md) | What the rider app uses and what sits unused; rider → driver; ratings in both directions. |
| [Push notifications](docs/push.md) | Firebase for Android, the push relay for iPhones, and what each notification says. |
| [The driver wallet](docs/wallet.md) | No top-up, no work: the wallet, the top-up gateways, the daily charge and the dispatch list built from it. |
| [Account deletion](docs/account-deletion.md) | How a person asks for their account to be deleted, and why nothing here deletes anything. |
| [Sign-in — SMS and WhatsApp](docs/sign-in.md) | The auth guard's codes: the SMS gateway, sign-in by an SMS the person sends, and WhatsApp. |
| [Backups](docs/backups.md) | The nightly encrypted backup: what it takes, what it skips, where it goes, and how to restore. |
| [Tests and CI](docs/testing.md) | Every automated test, the three workflows, and what a red tick does and does not mean. |
| [Releasing — `ops/deploy.sh`](docs/releasing.md) | How the server is changed: one command, what it checks, what it restarts. The step-by-step versions are the runbooks in `runbooks/`. |
| [Gotchas and known limitations](docs/gotchas.md) | Traps that each cost an afternoon, and what this stack does not do. |

Beside them:

- **[Decisions](docs/adr/README.md)** — why it is built this way: shims not Haskell, config not code, patches at build time, one merchant per country, the wallet.
- **Runbooks** — [release a change](docs/runbooks/release.md) and [undo one](docs/runbooks/rollback.md), step by step.
- **[What is ours and what is upstream's](docs/ours-and-upstream.md)** — in a fork of 15 000 files, the ~200 that are this deployment.
- **[The restructuring report](../../../docs/Movin-backend-restructuring-report.pdf)** (PDF, 2026-10-08) — the 24 September plan checked step by step, and the structure it produced.
- **Records** — the server as found and as changed by the restructuring: [box snapshot](docs/box-snapshot-2026-10-04.md) (phases 0–6, every release) and [box inventory](docs/box-inventory-2026-10-06.md) (every file, by hash).
- `investigations/README.md` — the probes; `stack/README.md` — the note left on the server itself.

### Sections that moved, by their old name

Comments in the code and in `CLAUDE.md` cite README sections by name ("README → *The test fleet*"). They are all here:

| Section | Now in |
|---|---|
| Algeria service areas | [docs/countries.md](docs/countries.md#algeria-service-areas) |
| Mauritania — the switch, and the two things that hid in it | [docs/countries.md](docs/countries.md#mauritania--the-switch-and-the-two-things-that-hid-in-it) |
| Two countries — Algeria beside Mauritania, since 2026-09-13 | [docs/countries.md](docs/countries.md#two-countries--algeria-beside-mauritania-since-2026-09-13) |
| OSRM — real Algerian routing | [docs/maps.md](docs/maps.md#osrm--real-algerian-routing) |
| Map tiles — the picture under the route | [docs/maps.md](docs/maps.md#map-tiles--the-picture-under-the-route) |
| Place search — finding somewhere to go | [docs/maps.md](docs/maps.md#place-search--finding-somewhere-to-go) |
| Published on the internet — TLS, and the lock that had to come first | [docs/edge.md](docs/edge.md#published-on-the-internet--tls-and-the-lock-that-had-to-come-first) |
| The domain — `./switch-domain.sh` | [docs/edge.md](docs/edge.md#the-domain--switch-domainsh) |
| What a person may send — the bounds, audited 2026-09-27 | [docs/edge.md](docs/edge.md#what-a-person-may-send--the-bounds-audited-2026-09-27) |
| The tariff — `./apply-tariff.sh` | [docs/fares-and-dispatch.md](docs/fares-and-dispatch.md#the-tariff--apply-tariffsh) |
| The search radius — how "a car is near" is decided | [docs/fares-and-dispatch.md](docs/fares-and-dispatch.md#the-search-radius--how-a-car-is-near-is-decided) |
| Choosing a driver — the fleet, the car, and the shortlist | [docs/fares-and-dispatch.md](docs/fares-and-dispatch.md#choosing-a-driver--the-fleet-the-car-and-the-shortlist) |
| The driver side (BPP) | [docs/drivers.md](docs/drivers.md#the-driver-side-bpp) |
| Driver freshness — `./drivers-keepalive.sh` | [docs/drivers.md](docs/drivers.md#driver-freshness--drivers-keepalivesh) |
| Two test drivers, at the two ends of the journey | [docs/drivers.md](docs/drivers.md#two-test-drivers-at-the-two-ends-of-the-journey) |
| Playing a driver — `./simulate-driver.py` | [docs/drivers.md](docs/drivers.md#playing-a-driver--simulate-driverpy) |
| The driver API — and why the source tree lies about it | [docs/driver-api.md](docs/driver-api.md#the-driver-api--and-why-the-source-tree-lies-about-it) |
| Driver documents — why `register/*` is deliberately never called | [docs/driver-api.md](docs/driver-api.md#driver-documents--why-register-is-deliberately-never-called) |
| A ride from the driver's side — measured, and where `/openapi` is wrong | [docs/driver-api.md](docs/driver-api.md#a-ride-from-the-drivers-side--measured-and-where-openapi-is-wrong) |
| Not connected yet: rider → driver | [docs/riders.md](docs/riders.md#not-connected-yet-rider--driver) |
| The rider API — what the app uses, and what is sitting there unused | [docs/riders.md](docs/riders.md#the-rider-api--what-the-app-uses-and-what-is-sitting-there-unused) |
| Ratings — `./apply-ratings.sh` | [docs/riders.md](docs/riders.md#ratings--apply-ratingssh) |
| Push notifications — `./apply-fcm.sh` | [docs/push.md](docs/push.md#push-notifications--apply-fcmsh) |
| The driver wallet — `driver-wallet.sql`, `maps-shim/wallet.js` | [docs/wallet.md](docs/wallet.md#the-driver-wallet--driver-walletsql-maps-shimwalletjs) |
| Account deletion — `account-deletion.sql`, `maps-shim/deletion.js` | [docs/account-deletion.md](docs/account-deletion.md#account-deletion--account-deletionsql-maps-shimdeletionjs) |
| The SMS gateway — Moorsyl, since 2026-09-06 | [docs/sign-in.md](docs/sign-in.md#the-sms-gateway--moorsyl-since-2026-09-06) |
| WhatsApp — the webhook, since 2026-09-27 | [docs/sign-in.md](docs/sign-in.md#whatsapp--the-webhook-since-2026-09-27) |
| Backups — `./backup.sh` | [docs/backups.md](docs/backups.md#backups--backupsh) |
| Tests, and what CI actually runs | [docs/testing.md](docs/testing.md#tests-and-what-ci-actually-runs) |
| Releasing — `ops/deploy.sh` | [docs/releasing.md](docs/releasing.md#releasing--opsdeploysh) |
| Gotchas | [docs/gotchas.md](docs/gotchas.md#gotchas) |
| Known limitations | [docs/gotchas.md](docs/gotchas.md#known-limitations) |
| The test fleet | [docs/countries.md](docs/countries.md#the-test-fleet) |
| The design: one merchant per country — on the driver side only | [docs/countries.md](docs/countries.md#the-design-one-merchant-per-country--on-the-driver-side-only) |
| Sign-in: accepted by the backend, gated by the guard | [docs/countries.md](docs/countries.md#sign-in-accepted-by-the-backend-gated-by-the-guard) |
| No top-up, no work — 2026-09-14, and hard at every layer | [docs/wallet.md](docs/wallet.md#no-top-up-no-work--2026-09-14-and-hard-at-every-layer) |
| Dispatch — `maps-shim/restricted.js` and two lines of Haskell | [docs/wallet.md](docs/wallet.md#dispatch--maps-shimrestrictedjs-and-two-lines-of-haskell) |
| iPhones — the push relay, `maps-shim/push-relay.js`, since 2026-09-16 | [docs/push.md](docs/push.md#iphones--the-push-relay-maps-shimpush-relayjs-since-2026-09-16) |
| The SMS inbox and sign-in by an SMS he sends (2026-09-29) | [docs/sign-in.md](docs/sign-in.md#the-sms-inbox-and-sign-in-by-an-sms-he-sends-2026-09-29) |
| Switching off a driver who has not paid | [docs/riders.md](docs/riders.md#switching-off-a-driver-who-has-not-paid) |

---

## Your first change

From nothing to a change running on the server, using only these docs.

1. **Bring a stack up** on your machine: `cd stack && ./setup.sh` (first run ~10 minutes). `./setup.sh price` signs a `+213` number in and asks for a priced ride — the same check CI runs. A dev stack has no drivers until `./setup.sh drivers`.
2. **Find where the change goes.** Almost never the Haskell ([decision 0001](docs/adr/0001-shims-not-haskell.md)). Config and SQL are under `stack/db/` and `docker-compose.yml`; behaviour beside the backend is `stack/maps-shim/` (one module per subject; `server.js` is only the router) or `stack/auth-guard/`.
3. **Change it and test it:** `(cd tests && npm ci) && bash tests/run-all.sh`. The golden files `tests/*-routes.test.js` compare every answer the shims give; a change that is *meant* to alter one re-records with `--record` in the same commit ([testing.md](docs/testing.md)).
4. **Commit and push**; both workflows go green on the commit.
5. **Release it** with the [release runbook](docs/runbooks/release.md): dry run, the owner's OK, `ops/deploy.sh`, then prove it from outside. If it went wrong: the [rollback runbook](docs/runbooks/rollback.md).

---

## Why this exists — three problems it solves

### 1. The current upstream backend cannot be run by outsiders

The database builds fine (421/422 migrations, 252 tables) but comes up **empty**.
Nothing in the repo inserts a row into `atlas_app.merchant`, and
`dev/local-testing-data/rider-app.sql` only creates a test rider *per merchant
that already exists*. Merchants come from `dev/config-sync`, which pulls from
Namma Yatri's own databases, or from an S3 bundle that returns `AccessDenied`.

So this stack pins upstream commit **`03a7531` (2023-03-02)** — the last
baseline that is self-contained and seeds a real merchant (`YATRI`).

### 2. The published Docker images are broken

`ghcr.io/nammayatri/nammayatri:*` is Ubuntu 18.04 and ships **librdkafka 0.11
(Feb 2018)**, but `rider-app-exe` needs >= 1.0 — it calls `rd_kafka_destroy_flags()`,
which doesn't exist in 0.11, and no newer copy is present anywhere in the image:

```
rider-app-exe: symbol lookup error: undefined symbol: rd_kafka_destroy_flags
```

`Dockerfile.rider` fixes this by building librdkafka 1.9.2 from source **on the
same 18.04 base**. Taking a prebuilt one from a modern distro does not work — it
pulls in OpenSSL 3 and a newer glibc, and the loader then fails with
`libpthread.so.0: symbol __libc_vfork ... not defined`.

### 3. Encryption is mandatory, not optional

rider-app encrypts PII (phone numbers) via **passetto**. Without it, every auth
request returns `500 INTERNAL_ERROR`. `passetto-db` is seeded with the
pre-generated keys that match the encrypted values in the seed data — that's why
the test rider's number decrypts correctly (`999...001`).

---

## Layout

Sorted on 2026-10-06 (phase 2 of the backend restructuring plan). **The rule:
everything a release copies is under `stack/`, and nothing else is.**

```
local-stack/
├── stack/                 WHAT THE SERVER RUNS — mirrors /opt/ny/local-stack
│   ├── docker-compose.yml   the stack (the website's admin-api is its overlay)
│   ├── Dockerfile.rider     the backend image: librdkafka + the binaries in bin/
│   ├── Dockerfile.maps-shim
│   ├── auth-guard/          sign-in, SMS/WhatsApp, attempt limits, wallet gate --
│   │                        server.js is the sign-in flow and the router; one
│   │                        module per subject beside it: limits, gateway,
│   │                        personal-codes, number-change, driver-rules,
│   │                        whatsapp, sms-inbox, trusted-phones (phase 5)
│   ├── maps-shim/           routing, places, wallet, payments, push, avatars --
│   │                        server.js is the router and start-up; directions,
│   │                        places, wallet, restricted, ... one module each
│   ├── edge/                nginx: names, TLS, rate limits
│   ├── demo-map/            the service-area map
│   ├── geocoder/            the place index's SQL and lists
│   ├── db/                  every .sql: tariffs, geofences, the movin schema,
│   │                        seeds — applied by the apply-*.sh scripts
│   ├── simulate-driver.py   the test fleet (systemd: movin-fleet)
│   ├── movin-bot.py         the owner's bot (systemd: movin-bot)
│   ├── backup.sh            the nightly backup (systemd: movin-backup, runs this file)
│   ├── restore.sh           put a backup back, or rehearse it in a throwaway copy
│   ├── systemd/             units a release installs: movin-backup.service/.timer
│   ├── setup.sh             bring a stack up from nothing (what CI runs)
│   └── apply-*.sh, *-prepare.sh, enrol-driver.sh, install-moosyl-key.sh,
│       fleet-service.sh, switch-domain.sh, deploy-*.sh, tiles-arabic.sh,
│       maps-two-countries.sh   — run ON the server, from this folder
├── ops/                   run from the laptop
│   ├── deploy.sh            THE way to release stack/ (see *Releasing*)
│   ├── release-remote.py    its server half, run by deploy.sh
│   ├── release-verify.py    the outside check: server's hashes vs. the commit, from git
│   ├── demo.sh, demo.ps1
│   └── checks/              the ride test (probe-two-country-rides.py)
├── investigations/        35 probes: how a booking behaves, what a route
│                          costs, why a driver was not offered a ride. Kept —
│                          they answer questions that will be asked again — but
│                          not part of anything that runs
├── retired/               the four scripts that made test accounts (see *The
│                          test fleet*). They expect to sit beside
│                          docker-compose.yml; copy one into stack/ to use it
│                          on a dev stack, never on the live server
├── tests/                 the node and python tests CI runs
├── docs/                  one page per subject, decisions (adr/), runbooks/,
│                          and the records of the server (box-*.md)
└── README.md
```

**The server has had this layout since the first release, 2026-10-06 09:21
UTC** (commit `0d463c3016`): `db/` arrived, the old top-level `.sql` copies,
probes, demos and retired scripts left the server (kept in `.prev` and in git),
and nothing restarted. Paths that systemd uses —
`/opt/ny/local-stack/simulate-driver.py`, `movin-bot.py`, and since phase 4
`backup.sh` — stay at the top of `stack/`.
