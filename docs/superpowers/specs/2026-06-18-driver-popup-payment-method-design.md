# Show Customer Payment Method (Cash / Online) on Driver Ride-Request Popup

**Date:** 2026-06-18
**Service:** dynamic-offer-driver-app (BPP)
**Scope:** Backend only. Frontend (driver app UI) lives in a separate repo and is out of scope.

## Problem

When a driver receives a new ride-request popup, they cannot see how the customer
will pay. Drivers want to know up front whether the ride is **cash** (collect from
rider) or **online/prepaid** (already paid or paid digitally), since it affects
whether they accept and how they handle pickup/drop.

The customer's payment method is already known to the BPP before the popup is shown —
it just isn't surfaced on the popup payload today. The backend job is narrow:

1. **Persist** the customer's payment instrument on the BPP `SearchRequest` at the
   moment the driver-search batch is initiated.
2. **Expose** a derived `isPaymentOnline :: Maybe Bool` on the ride-request popup
   payload (`SearchRequestForDriverAPIEntity`) so the frontend can render a
   cash/online badge.

No new endpoint, no new BECKN field, no BAP (rider-app) change.

## Background: where payment is known vs. where the popup is built

The driver popup payload is `SearchRequestForDriverAPIEntity`, built by
`makeSearchRequestForDriverAPIEntity`
(`src/Domain/Action/UI/SearchRequestForDriver.hs`). The per-driver rows
(`SearchRequestForDriver`) are constructed in `buildSearchRequestForDriver`
(`src/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/SendSearchRequestToDrivers.hs`).

Both the row builder and the API-entity builder only have the **`SearchRequest`** in
scope — and `sendSearchRequestToDrivers` even **re-fetches the `SearchRequest` from
DB** for the first batch
(`SendSearchRequestToDrivers.hs:124`: `QSR.findById oldSearchReq.id`). The customer's
`paymentMethodInfo` (which carries the instrument) flows only through the in-memory
`DriverSearchBatchInput` and is consumed earlier for driver-pool filtering — it never
reaches the popup builders.

Both driver-offer flows funnel through a single function,
`initiateDriverSearchBatch` (`src/SharedLogic/SearchTry.hs:132`), which destructures
`DriverSearchBatchInput {..}` and therefore has **both** the `searchReq` **and** the
`paymentMethodInfo` in scope at one point.

### When each flow knows the payment method

| Flow | Trip categories | Popup fires in | Payment source |
|------|-----------------|----------------|----------------|
| Estimate (dynamic offer) | `*OnDemandDynamicOffer` | **Select** → `initiateDriverSearchBatch` (`Domain/Action/Beckn/Select.hs`) | `sReq.paymentMethodInfo` parsed from BECKN `select` |
| Direct-quote static offer | `*OnDemandStaticOffer` | **Confirm** → `handleStaticOfferFlow` → `initiateDriverSearchBatch` (`Domain/Action/Beckn/Confirm.hs:185`) | `booking.paymentMethodId` → `mkPaymentMethodInfo` |
| Direct-quote RideOtp / special-zone OTP | `*RideOtp` | **No popup** — driver self-selects by entering OTP at the gate | n/a |

In both popup-bearing flows the caller already populates
`DriverSearchBatchInput.paymentMethodInfo` before calling `initiateDriverSearchBatch`.

## Design

### Chosen approach: persist on `SearchRequest`, write once centrally

Add `paymentInstrument :: Maybe PaymentInstrument` to the BPP `SearchRequest`, and
persist it **once** inside `initiateDriverSearchBatch` from the batch input's
`paymentMethodInfo`. The existing DB re-fetch in the builder then makes it available to
`makeSearchRequestForDriverAPIEntity`, which derives `isPaymentOnline`.

```
Select.hs (estimate) ─┐
                      ├─► DriverSearchBatchInput{searchReq, paymentMethodInfo}
Confirm.hs (static) ──┘            │
                                   ▼
                    initiateDriverSearchBatch  ──►  QSR.updatePaymentInstrument
                                   │                (persist on SearchRequest row)
                                   ▼
                    sendSearchRequestToDrivers ──► QSR.findById (re-fetch)
                                   │
                                   ▼
              makeSearchRequestForDriverAPIEntity ──► isPaymentOnline (derived)
```

### Why this approach (and why not the alternatives)

**Store on `SearchRequest`, not on the per-driver `SearchRequestForDriver` rows.**
- Payment method is a property of the **ride**, identical for every driver in the
  batch. Putting it on each `SearchRequestForDriver` row duplicates one value across N
  rows with no per-driver meaning.
- The read side (`buildSearchRequestForDriver`, `makeSearchRequestForDriverAPIEntity`)
  only has the `SearchRequest` in scope and already re-fetches it from DB, so reading
  from `SearchRequest` needs **zero** new plumbing.
- The per-row alternative would force threading `paymentMethodInfo` as a new parameter
  through `initiateDriverSearchBatch → sendSearchRequestToDrivers →
  buildSearchRequestForDriver` (three signature changes) for no semantic gain.

**Write once in `initiateDriverSearchBatch`, not in each handler.**
- It is the single funnel both flows pass through, and it already has both `searchReq`
  and `paymentMethodInfo` in scope. One write site covers the estimate flow and the
  direct-quote static-offer flow with no per-handler edits.

**Store the full `PaymentInstrument`, derive the cash/online flag at the edge.**
- Mirrors the existing `Booking.paymentInstrument` column
  (`spec/Storage/Booking.yaml:109`), which already persists `Maybe PaymentInstrument`
  with no custom serialization — proven pattern.
- Keeps the stored data future-proof (e.g. distinguishing UPI vs Card later) while the
  driver-facing contract stays a simple `isPaymentOnline :: Maybe Bool`.
- Derivation rule: `Cash → Online = False`; every other instrument (UPI, Card, Wallet,
  NetBanking, BoothOnline) → `Online = True`; absent → `Nothing`.

### Cases handled

- **Estimate / dynamic-offer flow** — payment from `select`, persisted at
  `initiateDriverSearchBatch`, shown on the post-select popup. ✅
- **Direct-quote static-offer flow** (`*OnDemandStaticOffer`) — payment from the
  booking, persisted at `initiateDriverSearchBatch` (called inside
  `handleStaticOfferFlow`), shown on the post-confirm popup. ✅
- **Scheduled rides** — the batch fires later via the allocator
  (`isAllocatorBatch = True`, so the builder does not re-fetch), but the scheduled job
  reloads the `SearchRequest` from DB when it runs, so the persisted field survives.
  ✅ (verify during testing — see Verification).

### Cases NOT handled (by design)

- **Direct-quote RideOtp / special-zone OTP flow** (`*RideOtp`) — there is **no driver
  popup**; the driver self-selects by entering an OTP at the pickup gate
  (`Domain/Action/Beckn/Confirm.hs`, RideOtp branch). There is nothing to attach a
  badge to. `isPaymentOnline` is simply never produced for this flow.
- **Any popup-bearing flow where the instrument is unknown** at batch time
  (`paymentMethodInfo = Nothing`) — the field persists as `Nothing` and the popup
  carries `isPaymentOnline = Nothing`. The frontend must treat absent as "unknown /
  show nothing", not as a specific method.

## Backward Compatibility (hard requirement)

- **DB column is nullable** (`Maybe PaymentInstrument`); existing rows become `NULL`.
  The generated migration adds the column with no `NOT NULL` and no default. No
  backfill.
- **Response field is additive `Maybe`**: the popup JSON gains one optional
  `isPaymentOnline` field (`omitNothingFields = True`, so it is absent when `Nothing`).
  Old frontend versions ignore the unknown field; new frontend treats `Nothing`/absent
  as "unknown".
- **No wire-format change to BECKN** and **no BAP change** — payment already arrives at
  the BPP via `select`/`init` as it does today.
- **Internal signature change** is limited to a new persistence query and one call
  site; no public API or BECKN contract changes.
- **Default behavior unchanged**: with no instrument known, the popup looks exactly as
  it does today.

## Out of Scope

- Frontend changes (separate repo): reading `isPaymentOnline` and rendering the badge.
- Surfacing the specific instrument (UPI vs Card vs Wallet) to the driver — only
  cash/online is required.
- The RideOtp / special-zone OTP flow (no popup exists).
- `collectedBy`-based nuance (a BPP-collected online payment is still "online" for this
  feature; we discriminate purely on `paymentInstrument == Cash`).

## Verification

- `cd Backend && cabal build dynamic-offer-driver-app` — clean under `-Werror`.
- Confirm the generator produced a migration in
  `dev/migrations/dynamic-offer-driver-app/` adding `payment_instrument` to the
  `search_request` table.
- Estimate flow: trigger a search → select with a Cash method, assert the
  `nearbyRideRequest` popup payload has `isPaymentOnline = false`; repeat with an
  online method and assert `true`.
- Static-offer flow: same assertion via the OnDemandStaticOffer confirm path.
- Scheduled ride: confirm a scheduled booking and assert the field survives into the
  later allocator-fired batch's popup.
- Absent case: a flow with no payment instrument omits `isPaymentOnline` from the JSON.

## Affected Files (summary)

| File | Change |
|------|--------|
| `spec/Storage/SearchRequest.yaml` | Add `PaymentInstrument` import + `paymentInstrument: Maybe PaymentInstrument` field |
| `src-read-only/Domain/Types/SearchRequest.hs` | Generated: field |
| `src-read-only/Storage/Beam/SearchRequest.hs` | Generated: Beam column |
| `dev/migrations/dynamic-offer-driver-app/*.sql` | Generated: nullable column |
| `src/Storage/Queries/SearchRequestExtra.hs` | Add `updatePaymentInstrument` query |
| `src/SharedLogic/SearchTry.hs` | Persist instrument from `paymentMethodInfo` in `initiateDriverSearchBatch` |
| `src/Domain/Action/UI/SearchRequestForDriver.hs` | Add `isPaymentOnline` to `SearchRequestForDriverAPIEntity`; derive it in `makeSearchRequestForDriverAPIEntity` |
