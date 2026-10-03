# Helsinki Invoices Collection

Verifies invoice + ledger generation after a ride completes in Helsinki.
Driven by `0003-enable-invoice-generation-for-helsinki-delhi.sql` (the merchant
config flag that turns on per-ride invoice generation for `BRIDGE_FINLAND_PARTNER`).

## When to run

Run this collection **after each ride combo** in `InternationalRideBookingFlow/`.
The orchestrator `run-helsinki-e2e.sh` does this automatically by chaining
`B-<combo>` → `C-<combo>-Invoices` with the env file flowing through (so
`driver_ride_id`, `driver_id`, `fleet_owner_id` are all populated).

## What it asserts (v1)

- **Existence** of one invoice per `issuedTo` enum for the just-finished ride:
  Customer, Driver, FleetOwner.
- **Existence** of one `AggregatedCommission` invoice (the platform's commission
  rollup).
- **Existence** of wallet ledger entries — exercises the new
  `from_sub_counterparty_id`/`to_sub_counterparty_id` indexes added by
  `dev/ddl-migrations/.../0822` and `1533`.
- Customer invoice has a **non-zero total**.

## What it does NOT assert (v2 follow-ups)

- Commission percentage math (e.g. driver share = fare × (1 − commission_pct)).
- Refund/cancellation-fee invoice paths — for the cancellation combos, the
  Customer/Driver invoice may be absent and a cancellation-fee invoice present;
  current assertions are tolerant (existence only) but should be tightened.
- VAT line items per `0007-helsinki-vat-config.sql`.
- Cross-check with `finance_ledger_entry` SQL rows (the orchestrator can do this
  via `psql -c`).

## Env vars consumed

- `dashboard_token`, `dashboard_merchant_id`, `dashboard_city` — auth + scope
- `driver_ride_id` — set by the ride combo collection
- `driver_id`, `fleet_owner_id` — set by `HelsinkiOnboarding/`
- `invoice_from`, `invoice_to` — defaulted by the collection's prerequest
  (last 1 hour window). Override via env to tighten the window.

## Open questions to validate at first runtime

- Exact path: `financeInvoiceList` may be under a different segment than
  `/{merchantId}/{city}/financeManagement/` — verify by 404 if it returns one;
  candidates include `/management/financeManagement/...` per the
  `FinanceManagement.hs` mount.
- Query-param names (`issuedTo`, `invoiceType`, `rideId`, `fleetOwnerId`,
  `driverId`) — these are best-guess from the route `GET` handler; if the API
  rejects them, the script logs the response body for inspection.
- Response shape: tests handle three wrappers (`list`, `invoices`, raw array).
