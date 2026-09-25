# SpecialZoneEdcCommissionFlow

Integration collection for the **special-zone booth / Paytm EDC commission** flow
(BHARAT_TAXI, Delhi airport, `std:011`). Drives a booth booking paid via Paytm EDC,
simulates the Paytm EDC S2S success callback, and asserts the booking is confirmed
and the EDC order collected **only the commission** (`fareSettlementType=CommissionOnly`).

> **Status: SCAFFOLD — not yet run end-to-end.** It was authored from the codebase
> (mirrors `AirportTaxiFlow`, which is the same booth sequence paid with Cash) but has
> not been executed against a live stack. Expect to fix the items marked **TODO** below.
> It is intentionally checked in unverified so it can be finished in a full dev env.

## What it does (step order)

1. `BAP - Login` / `Switch Merchant And City` — dashboard auth (sets `bap_dashboard_token`).
2. `Seed: Paytm EDC service config (update / insert)` — ensures a `Payment_PaytmEDC`
   row in `merchant_service_config` whose `config_json.baseUrl` points at the mock
   (`{{mockServerUrl}}/paytm`). Without it `createRideBookingPaymentOrder` throws
   `MerchantServiceConfigNotFound`.
3. `Read Special Location Id (IGI Airport)` — resolves the exact `special_location.id`
   (`atlas_driver_offer_bpp`, `location_name LIKE '%IGI%'`) so the following steps target
   it precisely instead of guessing with a bare `LIKE`.
4. `Seed: special_location fareSettlementType=CommissionOnly` — sets the zone to
   `CommissionOnly` by id. This is what makes the EDC collect only the commission
   (`Domain/Action/UI/Payment.hs:188`).
5. `Read Fare Policy Ids for zone` — selects `fare_product` rows whose
   `area LIKE 'Pickup_<specialLocationId>%'` and captures their `fare_policy_id`s.
   **This assertion is the "fare policy is configured for this location" check** — it
   fails loudly (non-empty required) if the zone has no fare_product/fare_policy mapping,
   which is also exactly why `Get Quotes` would otherwise time out with no useful signal.
6. `Seed: Fare Policy commission (fixed amount, not %)` — sets
   `commission_charge_config = {"value":"{{edc_commission_amount}}","appliesOn":["RideFare"]}`
   on every fare_policy found in step 5. **No `%` suffix** → `parseCodeValue` parses it as
   `ParsedFixed`, and `applyParsedValue` returns that value untouched regardless of fare
   (`FareCalculator.hs`: `ParsedFixed amount -> amount`) — i.e. the commission is a flat
   `{{edc_commission_amount}}` (default ₹50), not a percentage of the ride fare.
   ⚠️ Overwrites `commission_charge_config` wholesale for every vehicle-variant fare policy
   the zone routes to — fine for a dedicated test env, not for a shared one.
7. `Ride Search` → `Get Quotes` → `Rider Registration` → `Update Rider Profile` — same as AirportTaxiFlow.
8. `Confirm Booking (Paytm EDC)` — confirm with `paymentInstrument=BoothOnline&paymentMethodId=PAYTM_EDC`.
   This pair sets `requiresPaymentBeforeConfirm`, so the BPP confirm is held until payment.
9. `Get Booking Details` — captures `commissionCharge`.
10. `Read Payment Order` — SQL-selects the `payment_order` (by `domain_entity_id = bookingId`),
    captures `orderId` / `orderShortId`, and asserts **EDC order amount == commissionCharge**
    AND **== the constant `edc_commission_amount`** seeded in step 6 (cross-check).
11. `Simulate Paytm EDC Callback (success)` — `POST {{bap_app_url}}/s2s/payment/paytm/edc/callback`
    with `merchantTransactionId=orderShortId`, `resultStatus="S"`. (No checksum/auth is validated
    on this endpoint — see `paytmEdcCallbackHandler`.)
12. `Verify Payment Order CHARGED` — asserts the order reached `CHARGED` (→ confirm forwarded to BPP).
13. `Cleanup` — clears mock overrides.

## Prerequisites (must be true in the target env)

- **Mock server running** on `{{mockServerUrl}}` with the extended `services/paytm.py`
  (serves `/paytm/ecr/generateChecksum`, `/ecr/payment/request`, `/ecr/payment/status`).
- **Merchant `online_payment = false`** for BHARAT_TAXI (or `validatePaymentInstrument`
  rejects `BoothOnline`).
- **A `special_location` row exists** for the IGI airport pickup (`location_name LIKE '%IGI%'`)
  **with at least one `fare_product` mapped to it** — the collection verifies this (step 5) and
  seeds `fareSettlementType` + a fixed commission on top of whatever fare policy is already
  there (steps 3–6). It does **not** create a fare policy from scratch; if step 5's assertion
  fails, the zone isn't wired for quoting at all in this env and needs fixing before the EDC
  parts are worth debugging.
- Confirmed against `dev/migrations-read-only`: `atlas_driver_offer_bpp.special_location.fare_settlement_type`,
  `atlas_driver_offer_bpp.fare_policy.commission_charge_config`, and
  `atlas_app.payment_order.{domain_entity_id,short_id,payment_fulfillment_status}` all exist as used here.

## TODOs to make it green

- **Agent + EDC machine:** this scaffold does not pass `dashboardAgentId`, so an
  `edc_machine_mapping` is not required (`createRideBookingPaymentOrder` only demands one when
  `booking.dashboardAgentId` is set). To exercise the agent-terminal path, capture the login
  user's `personId`, seed an active `edc_machine_mapping`, and pass `dashboardAgentId` on confirm.
- **Cache:** after seeding `merchant_service_config` / `fare_policy`, the cached copy may be
  stale — add a `POST {{mockServerUrl}}/mock/redis/del` (pattern match on the relevant cache
  keys) before Ride Search if the seeded config doesn't take effect.
- **Timing:** `createRideBookingPaymentOrder` runs off a fork inside `on_init`, so `Read Payment
  Order` may race the row being written. Add a short retry/poll (same pattern as `Get Quotes`)
  if it comes back empty.
- **`payment_order` other column values:** the collection selects `amount`/`status` too but
  doesn't hard-assert their raw shape beyond what's described above — sanity check they read as
  expected on first run.

## Run

Via the test dashboard (Collections tab) or Newman:

```bash
newman run SpecialZoneEdcCommissionFlow.json \
  -e Local/Local_BT_Delhi_EDC.postman_environment.json
```
