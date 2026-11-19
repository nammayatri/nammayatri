# 0002 — Config, not code

**Status:** in force since August 2026. **Where it shows:**
`atlas_app.merchant_service_config`, `transporter_config.fcm_url`,
`stack/db/*.sql`, `docker-compose.yml`.

## Context

Upstream calls Google for directions, places and geocoding, and Firebase for
push. We wanted none of the bills and none of the accounts. The backend turns
out to read its service **endpoints from the database**, not from compiled
constants.

## Decision

Integrate by repointing the backend's own configuration at our services:

- `Maps_Google` in `atlas_app.merchant_service_config` carries `googleMapsUrl`;
  pointing it at `maps-shim` *is* the maps integration
  ([maps.md](../maps.md)).
- `fcm_url` points at the shim's push relay, which forwards Android tokens to
  Google and sends iPhone tokens to APNs ([push.md](../push.md)).
- Tariffs, geofences, merchants, the registry are SQL files under `stack/db/`,
  applied once, keyed by merchant ([0004](0004-one-merchant-per-country.md)).
- Shim and guard behaviour is environment in `docker-compose.yml`; secrets come
  from files on the server, never from git.

## Consequences

- Swapping a provider is a row and a cache flush, not a build.
- **Cached config survives a restart.** `merchant` and `transporter_config` are
  cached in Redis; an `UPDATE` and a restart change nothing until the keys are
  dropped ([gotchas.md](../gotchas.md)).
- A `db/*.sql` is applied by a release only when its content is new to the
  server ([releasing.md](../releasing.md)) — re-running a tariff would undo
  every fare changed since.
- Not every knob is real: `useFakeSms` is compiled in, so the SMS gateway went
  in front of the backend instead ([0001](0001-shims-not-haskell.md)).
