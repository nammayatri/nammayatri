# Shared-cab scenario scripts (validators layer 5)

**Untested until a backend is up.** These files parse (`hurlfmt --check`) and fail only on the HTTP connection. None has run against a rider-app yet.

Board rows 3.6 (driver flow), 6.8 (rider flow) and 8.7 (boarding flow). **`boarding-flow.hurl` is untested until 8.1 merges** (boarding is on `backend/feat/shared-cab-prime-81`) and allocation (7.1–7.4) is on. The driver side hits rider-app's internal `/internal/sharedCab/*` APIs directly: these are what driver-app proxies, plus the `driverId` / `vehicleNumber` that driver-app resolves from its token (`spec/API/SharedCabInternal.yaml`). The rider side uses the multimodal UI APIs with a rider token.

## Run

Prerequisites:
- rider-app, Redis, Postgres, and the OTPRest feed for the city.
- The shared-cab tier and fare rows seeded (task 0.3).
- `hurl` and `jq` installed.

Fill the `REPLACE_ME` values in `local.env` (or copy it and point `ENV` at the copy), then:

```sh
./run.sh            # driver, flush, rider
./run.sh driver     # 3.6 driver flow
./run.sh flush      # 3.6 flush recovery (needs redis-cli on the rider-app's Redis; override with REDIS_CLI="redis-cli -p 6380")
./run.sh rider      # 6.8 rider flow (logs in via login.hurl, or reuse TOKEN=...)
./run.sh boarding   # 8.7 boarding flow (needs 8.1 and allocation on)
```

`run.sh` passes `date` (today in IST) for the trips history. Each file ends by closing the driver's session, so every file re-runs. The rider login is rate limited, so reuse `TOKEN` across runs.

## What each file asserts

| File | Steps | Asserts |
|---|---|---|
| `driver-flow.hurl` | select route_a → walk-up on → stale seats write → walk-up off → change to route_b → back to route_a → Start return → End at last stop | session `status` ACTIVE, `route.code`, `queuedRoute` null, `walkupCount` / `available` (4 → 3 → 4), `version` rises on every write; stale `version` → 409 `SHARED_CAB_SESSION_VERSION_MISMATCH`; trips: old run COMPLETED/`ROUTE_CHANGED`, then `RETURN` onto `route_a_return`, last run COMPLETED/`END_ROUTE` with `endedAt`; after End the session is 404 `SHARED_CAB_SESSION_NOT_FOUND` and the end response is `null` |
| `flush-before.hurl` | select route_a with one walk-up | ACTIVE session, trip row ACTIVE |
| (run.sh) | `DEL sharedcab:session:{plate} sharedcab:route:{route_a}` | |
| `flush-after.hurl` | GET session → seats write → end | GET session rebuilds from the live trip row (3.5), no re-select: same route and run, status ACTIVE, `walkupCount` 1 / `available` 3, `version` from the clock; the trip row is still the ACTIVE one; the restored `version` takes a seats write; after End the session is 404 and the run closes as `END_FOR_NOW` |
| `rider-flow.hurl` | search with no cab → driver selects → search → initiate → confirm → cancel → search → initiate → confirm → "I got down" (`setStatus/Completed`) → driver ends → search | no cab ⇒ no itinerary on `route_a`, with a cab ⇒ one; initiate's Bus leg is on `route_a`; confirm returns no `orderSdkPayload`, `paymentStatus.paymentOrder` null and `journeyPaymentStatus` null (pay on board); leg `sharedCab.state` FINDING with `vehicleNumber` null, CANCELLED after cancel, DROPPED after "I got down"; itinerary hidden again once the cab ends |
| `boarding-flow.hurl` | (1) cab A on route_a → book → rider location at the stop → allocated → wrong code → sticker code → cancel → driver end → "I got down"; (2) book → allocated to A → cab B comes on → board B with B's code; (3) walk-up: book and type A's code at once | (1) leg ALLOCATED on `plate`, route view `freeSeats` 3 while allocated; code `0000` → 400 `SHARED_CAB_BOARDING_FAILED`; last-4 code → no `boardingConfirmationRequired`, leg BOARDED on `plate`; cancel → 400 (R7); driver END without force → 409 `SHARED_CAB_RIDERS_ON_BOARD`; DROPPED, `freeSeats` back to 4. (2) leg BOARDED on `plate_b`, A's `freeSeats` 4 and B's 3. (3) BOARDED on `plate` straight from FINDING or ALLOCATED, `freeSeats` 3, then DROPPED |
| `login.hurl` | OTP auth → verify | prints the verify body; `.token` is the rider token |

## Not covered yet
- **DEGRADED, and a route change with riders on board.** `boarding-flow.hurl` covers ALLOCATED, BOARDED, the refused cancel (R7) and the refused End. Still missing: a DEGRADED leg (boarded, then the cab's session disappears) and a route select blocked by riders on board (`affectedRiders`).
- **Flush recovery with a booking** (04 §9: the booking found again by plate): needs an allocated booking, same dependency.
- **Restore details the APIs can't show**: re-listing on `sharedcab:route:{code}` and the LTS re-attach. Check them in Redis and the rider-app log. Needs 3.5 (`backend/feat/shared-cab-lts2`) merged.
- **Restored walk-ups**: `walkupCount` is seeded from the trip's `offlineBoardings`, which counts every walk-up of the run. After toggles it can exceed the real count, so `available` comes out low. That is conservative, and the flush test avoids it by never toggling off before the flush.
- **Itinerary choice.** The rider flow takes `journeys[0]`: pick `from_*` / `to_*` so the shared cab is the only transit option (true for a Shillong feed with only SC routes). The `initiate` assert catches a wrong pick.
- **Invariants.** The checker (layer 4) logs `invariant_violation` to rider-app's log and does not surface it over HTTP; grep the log after a run.
