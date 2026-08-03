# local-stack — self-hosted Namma Yatri rider backend

Brings up a **working** rider-app backend (API + Postgres + Redis + Kafka +
encryption service) in Docker, with a seeded merchant and a test rider, so the
full login flow works end to end.

```bash
cd Backend/dev/local-stack
./setup.sh
```

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
| `http://localhost:8015` | **Service-area map** — click anywhere, the backend answers |
| `http://localhost:8014/swagger` | Swagger UI — 60 endpoints (**no trailing slash**, see Gotchas) |
| `http://localhost:8014/openapi` | OpenAPI spec (JSON) |
| `localhost:5434` | Postgres (`postgres` / `root`, db `atlas_dev`, schema `atlas_app`) |

Demo script for showing it works: `./demo.sh` (or `demo.ps1` on Windows).

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

## Algeria service areas

`algeria-geofences.sql` repoints the backend from the upstream Indian service
areas to **Algiers, Oran and Annaba**. `setup.sh` applies it automatically;
`./setup.sh algeria` re-applies it on its own.

Serviceability is one query — `Main/src/Storage/Queries/Geometry.hs`:

```sql
SELECT * FROM atlas_app.geometry
 WHERE region IN (<merchant.origin_restriction>)
   AND ST_Contains(geom, ST_Point(lon, lat));
```

So a city is **two pieces of data and zero lines of code**: a `geometry` row
(name + boundary), and that name listed in the merchant's origin/destination
restriction. A fourth city is one more row and one more array element.

Boundaries are the OpenStreetMap wilaya relations (`admin_level=4`), simplified
to ~55 m to keep the file reviewable — 33k points down to 2.6k. Verified:

| Point | Serviceable |
|-------|-------------|
| Algiers centre / Algiers airport / Bab Ezzouar | ✅ |
| Oran centre / Es Senia airport | ✅ |
| Annaba centre | ✅ |
| Constantine | ❌ |
| Bangalore | ❌ |

The two negatives matter — they prove the Indian areas were *replaced*, not
merely added to.

> **SRID 0, not 4326.** Matches the existing rows and the `ST_Point()` the
> application builds. PostGIS refuses `ST_Contains` across mismatched SRIDs.

> **Redis caches the merchant.** The service-area restriction lives on the
> merchant row, which `Storage/CachedQueries/Merchant.hs` caches. Change it in
> Postgres without dropping the cache and the API keeps serving the old areas.
> `setup.sh` flushes Redis for you.

### The map — `http://localhost:8015`

A one-page visual of the same thing the terminal checks: the three boundaries
drawn on a map, click anywhere to fire a real
`POST /v2/serviceability/origin` and get green (served) or red (not served).

Two things keep it honest:

- The polygons are **exported from `atlas_app.geometry` on every run**, not
  hand-drawn, so the map cannot drift from what the API enforces.
- The answers are **live API calls**, not a lookup in the page.

It also exists because of CORS: rider-app sends no CORS headers and 404s on
`OPTIONS`, so a page opened from `file://` or any other port cannot call it.
The `map` container (nginx) serves the page *and* reverse-proxies `/v2` to
rider-app, putting both on one origin — no upstream patch needed.

### What is *not* just config: the country code

Cities are data, but the **country is hard-coded**. `POST /v2/auth` rejects an
Algerian number outright:

```
{"errorPayload":[
  {"expectation":"(length(mobileNumber) == 10 and mobileNumber matches regex /^[0-9]*$/)"},
  {"expectation":"mobileCountryCode matches regex /^\\+91$/"}],
 "errorCode":"REQUEST_VALIDATION_FAILURE"}
```

From `Main/src/Domain/Action/UI/Registration.hs`:

```haskell
validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
```

Making this configurable is a small source change, but it is a *source* change —
it needs a Haskell rebuild, which this stack (running a prebuilt image)
deliberately avoids. The demo therefore still logs in with the `+91` test rider;
the geofence result is independent of the phone number.

---

## Gotchas

**Use `/swagger`, not `/swagger/`.** With a trailing slash the page's relative
asset paths resolve to `/swagger/swagger-ui.css` and 404, leaving a blank page.
The static files are served from the root, and `swagger-initializer.js` derives
the spec URL via `window.location.href.replace("/swagger", "/openapi")`.

**Port 8014, not 8013.** rider-app runs with `network_mode: host` (the dhall
configs hardcode `localhost` for Postgres/Redis/Kafka/passetto). Host-network
ports aren't forwarded out of the Docker Desktop VM, so a small `socat` proxy
re-exposes the API on `localhost:8014`.

**Migration ordering.** `setup.sh` applies only the base schema; rider-app runs
`dev/migrations/rider-app` itself on every startup. Applying them beforehand
causes `column ... already exists` failures. Test data is loaded *after*
rider-app has migrated.

---

## Known limitations

- **This is the 2023 baseline, not current `main`.** Running today's backend
  would need a full Haskell build *and* merchant config we don't have.
- `GET /v2/profile` returns 500 — it needs a WhatsApp provider and another
  service that aren't configured. Optional integrations; core flows are fine.
- Kafka connection warnings in the logs are harmless.
- No BPP (driver side) — this is the rider platform only, so a search returns no
  quotes. Enough to validate auth and serviceability.

## Layout

```
setup.sh              one-shot bring-up / verify / algeria / down / clean
docker-compose.yml    the stack
Dockerfile.rider      librdkafka fix
algeria-geofences.sql Algiers / Oran / Annaba service areas
demo.sh, demo.ps1     scripted end-to-end demo
demo-map/             the map on :8015 (nginx conf + page)
  site/areas.geojson  exported from the DB by setup.sh (gitignored)
2023/                 pinned upstream tree (fetched by setup.sh, gitignored)
```
