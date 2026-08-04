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
3. `Seed: special zone fareSettlementType=CommissionOnly` — **TODO** (see below).
4. `Ride Search` → `Get Quotes` → `Rider Registration` → `Update Rider Profile` — same as AirportTaxiFlow.
5. `Confirm Booking (Paytm EDC)` — confirm with `paymentInstrument=BoothOnline&paymentMethodId=PAYTM_EDC`.
   This pair sets `requiresPaymentBeforeConfirm`, so the BPP confirm is held until payment.
6. `Get Booking Details` — captures `commissionCharge`.
7. `Read Payment Order` — SQL-selects the `payment_order` (by `domain_entity_id = bookingId`),
   captures `orderId` / `orderShortId`, and asserts **EDC order amount == commission**.
8. `Simulate Paytm EDC Callback (success)` — `POST {{bap_app_url}}/s2s/payment/paytm/edc/callback`
   with `merchantTransactionId=orderShortId`, `resultStatus="S"`. (No checksum/auth is validated
   on this endpoint — see `paytmEdcCallbackHandler`.)
9. `Verify Payment Order CHARGED` — asserts the order reached `CHARGED` (→ confirm forwarded to BPP).
10. `Cleanup` — clears mock overrides.

## Prerequisites (must be true in the target env)

- **Mock server running** on `{{mockServerUrl}}` with the extended `services/paytm.py`
  (serves `/paytm/ecr/generateChecksum`, `/ecr/payment/request`, `/ecr/payment/status`).
- **Merchant `online_payment = false`** for BHARAT_TAXI (or `validatePaymentInstrument`
  rejects `BoothOnline`).
- **Special zone exists** for the IGI airport pickup with a **fare policy that produces a
  non-zero commission** (the commission arrives on the booking via `on_init`). The collection
  cannot create fare policy — that must already be seeded.

## TODOs to make it green

- **Step 3 schema/zone id:** confirm the `special_location` table's schema and the WHERE
  that selects the airport zone. `fareSettlementType` lives on the **BPP** special location and
  is what makes the EDC collect only the commission (`Domain/Action/UI/Payment.hs:188`). The
  `LIKE '%IGI%'` filter is a placeholder.
- **`payment_order` column names:** verify `domain_entity_id`, `short_id`, `amount`, `status`,
  `payment_fulfillment_status` match the live schema.
- **Agent + EDC machine:** this scaffold does not pass `dashboardAgentId`, so an
  `edc_machine_mapping` is not required (`createRideBookingPaymentOrder` only demands one when
  `booking.dashboardAgentId` is set). To exercise the agent-terminal path, capture the login
  user's `personId`, seed an active `edc_machine_mapping`, and pass `dashboardAgentId` on confirm.
- **Cache:** after seeding `merchant_service_config`, the cached copy may be stale — add a
  `POST {{mockServerUrl}}/mock/redis/del` for the merchant-service-config key if the config
  doesn't take effect.

## Run

Via the test dashboard (Collections tab) or Newman:

```bash
newman run SpecialZoneEdcCommissionFlow.json \
  -e Local/Local_BT_Delhi_EDC.postman_environment.json
```
