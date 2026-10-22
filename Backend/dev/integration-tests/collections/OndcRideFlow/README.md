# OndcRideFlow — local prerequisites

The collection plays an ONDC buyer app (`workbench.ondc.tech`) against the local BPP:
search → on_search → select → on_select → init → on_init → confirm → on_confirm, then the
driver accepts, starts and ends the scheduled ride. The BAP side is `dev/mock-servers/services/workbench_bap.py`,
which stores every `on_*` callback the BPP sends so the wait steps can poll it.

Everything below is dev-only test setup. None of it belongs in a real environment.

## Driver app config (`dhall-configs/dev/dynamic-offer-driver-app.dhall`)

- `noSignatureSubscribers` must contain `"workbench.ondc.tech"`. The collection signs every Beckn
  request with a placeholder signature (86 × `A`), so the BPP must skip verification for this
  subscriber. The registry lookup for the key still runs, so the registry rows below are required too.
- `disableSignatureAuth` has no effect (mobility-core no longer reads it); leave it `False`.
- Restart the driver app after changing the dhall: it is read once at startup.

## Seeded data (applied by the test-tool startup runner from `dev/local-testing-data/*.sql`)

| file | what it seeds | why |
|---|---|---|
| `mock-registry.sql` | registry rows for `workbench.ondc.tech` / key `c11d8b66-…`, type **BAP** and type **BG** | `Authorization` resolves as BAP, `X-Gateway-Authorization` as BG; both lookups must succeed |
| `dynamic-offer-driver-app.sql` §10 | `white_list_org` row for the workbench in every city that keeps a whitelist | cities with a whitelist reject every other subscriber |
| `dynamic-offer-driver-app.sql` §11 | `transporter_config.enable_ondc_scheduled_ride_support = true` for MSIL cities | a scheduled search answers with Quotes; select/init only resolve a Quote item when this is on |
| `dynamic-offer-driver-app.sql` §8 / §8b2 | seed fleet owner mobile number, `fleet_owner_information` row | the seed driver belongs to a fleet, so go-online checks the fleet owner's prepaid wallet and fleet status |

If `transporter_config` was changed while the app was running, restart it: ConfigPilot keeps the
resolved config in memory for an hour, and the Redis copies (`driver-offer:CachedQueries:TransporterConfig:…`,
`dynamic-offer-driver-app:ConfigPilot:TransporterConfig`) must be dropped as well.

## Environment (`Local/Local_<MERCHANT>_<City>.postman_environment.json`)

- `driver_token` / `fleet_owner_token`: the seeded `seed-driver-token-<shortId>` / `seed-fleet-owner-token-<shortId>`.
- `prepaid_min_plan_amount`: the Setup group buys the cheapest prepaid plan at or above this for the
  fleet owner through the Juspay mock, so the driver clears the go-online balance threshold.
- `drop_locations`: JSON `[{name, lat, lon, address}]` of drops inside the city's service geometry.
  The test-tool runner shows them in a **Drop** dropdown; "Random" picks a point `drop_min_km`–`drop_max_km`
  from pickup. A drop outside the geometry makes search ACK and then fail silently with
  `RIDE_NOT_SERVICEABLE`, so verify new points with `ST_Contains` against `atlas_driver_offer_bpp.geometry`.

## Runner dropdowns (env `ui_selects`)

- **BAP**: `workbench.ondc.tech` or `local.collector`; each option also sets `ondc_bap_key_id`. Both are in
  `noSignatureSubscribers`, `mock-registry.sql` and the city whitelist. A ride booked through one BAP can only be
  reconciled by that BAP (RSF E4 checks the booking's `bap_id`).
- **Collected by**: BPP (cash, the default) or BAP (online; init sends a `PAYMENT_INSTRUMENT` tag from
  `bap_payment_instrument`, which must match a BAP-collected payment method of the city).
- **Drop**: a named drop from `drop_locations`, or Random.

## Setup group, in order

1. Switch dashboard merchant/city; point route search at the Google Maps mock.
2. List prepaid plans, buy one as the fleet owner, mark the order CHARGED via `/mock/override`,
   confirm with `GET /payment/{orderId}/status`, clear the override.
3. Set the driver online.
