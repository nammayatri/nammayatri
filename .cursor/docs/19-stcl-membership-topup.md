# STCL Membership — Share Top-Up

How a driver who already bought N shares can buy more shares later, without breaking the global "shares are continuous" invariant or any existing API.

## The constraints we have to respect

1. **One driver, max 5 shares total.**
2. **Share numbers are globally continuous.** Driver A gets 1–2, Driver B gets 3–4, Driver C gets 5–9, etc. Once a range is allocated, it is permanent.
3. **Payment is required for every share purchased.** No free top-ups.
4. **`get` and `update` APIs keep working** for older clients that already expect a single application per driver.

Because share numbers are globally continuous, **a driver who bought shares 1–2 cannot later "stretch" that range to 1–5** — shares 3–4 belong to someone else. The additional 3 shares must be a *new* range (e.g., 12–14).

## The core idea — one row per purchase

We do **not** mutate the existing application to add shares. Instead, **each purchase is a new `stcl_membership` row** for the same driver.

```
Driver bought 2 shares last month:
┌────────────────────────────────────────────────────────────────┐
│ driverId=D1, applicationId=A1, numberOfShares=2,               │
│ shareStartCount=1, shareEndCount=2, status=SUBMITTED           │
└────────────────────────────────────────────────────────────────┘

Driver wants to top up by 3 more shares today:
┌────────────────────────────────────────────────────────────────┐
│ driverId=D1, applicationId=A2, numberOfShares=3,               │  ← NEW ROW
│ shareStartCount=12, shareEndCount=14, status=SUBMITTED         │
└────────────────────────────────────────────────────────────────┘

The driver now "owns" 5 shares total, across 2 allotments.
```

Both rows live forever in the table. The driver effectively holds shares `[1, 2, 12, 13, 14]`.

## How does the driver buy additional shares?

A **dedicated endpoint** `POST /buyAdditionalShares` handles top-ups. The driver does *not* re-call `/submitApplication` (that endpoint remains first-purchase-only and rejects any driver who already has a SUBMITTED row, matching its original contract).

```
POST /buyAdditionalShares
{
  "numberOfShares": 3,                       ← only the extra shares, NOT the new total
  "amount": null,                            ← optional; server fills in pricePerShare × numberOfShares when omitted
  "paymentServiceType": null
}
```

The endpoint:

| Step | Behavior |
|------|----------|
| Rejects if no SUBMITTED row | The driver must already have completed a first purchase via `/submitApplication`. KYC isn't invented on this endpoint. |
| Loads the driver's latest SUBMITTED row | Source of truth for KYC, address, bank, vehicle, nominee, declaration. Frontend doesn't resend these. |
| Validates | No PENDING row already in flight; `sum(existing SUBMITTED shares) + numberOfShares ≤ 5`; `numberOfShares > 0`. |
| Resolves amount | If `amount` is in the request, use it. Otherwise compute `pricePerShare (₹100) × numberOfShares`. |
| Creates a new row with `status = PENDING` | Copies KYC/address/bank from the latest SUBMITTED row; sets `numberOfShares` to the requested delta. |
| Creates a Juspay payment order | Identical to the first-purchase flow. |
| Returns `CreateOrderResp` | Frontend opens the payment screen. |
| On `CHARGED` webhook | `stclMemberShipOrderStatusHandler` calls `updateApplicationAndShareCounts`, which atomically claims the next contiguous range from Redis (e.g., 12–14) and flips the row to `SUBMITTED`. |

The webhook code is **completely unchanged** — it doesn't know or care whether the row was created via first purchase or top-up. The share-range allocation is identical for both paths.

### Why a separate endpoint instead of reusing /submitApplication?

- Frontend doesn't have to resend (or recompute) KYC/address/bank — the request body is tiny.
- A stale cached form in the frontend can't accidentally overwrite the driver's address or bank details.
- Clear separation of concerns: first-purchase (full KYC) vs top-up (numberOfShares only). Old clients that rely on `/submitApplication` rejecting duplicates still work exactly as before.

## What stops a driver from going over the cap?

Inside `postBuyAdditionalShares` (the top-up endpoint):

1. **At most one fresh PENDING in flight.** If the driver already has a `PENDING` row younger than `pendingStaleMinutes` (15 min), we look at whether the new request *matches* the in-flight order:
   - **Same `numberOfShares` AND same resolved `amount`** → re-issue the existing payment order (same Juspay `orderId`). The frontend gets back a `CreateOrderResp` with the same payment links the driver got the first time, so they resume the in-flight payment instead of starting a second one.
   - **Different `numberOfShares` or `amount`** → the driver's intent has changed (e.g., they originally requested 1 share by mistake and now want 2). We retire the in-flight `PENDING` as `REJECTED` and create a fresh order with the new quantity / amount. Silently replaying the old payment link would have charged them for the wrong number of shares.
   - If the existing `PENDING` is older than the stale window, we treat it as abandoned (driver closed the payment app and never came back) and retire it as `REJECTED` so the new request can proceed.

   Either way a stuck payment never permanently blocks future top-ups, and the cap check still ignores PENDING shares so over-allocation is impossible.
2. **Total stays ≤ 5.** Sum of `numberOfShares` across all the driver's `SUBMITTED` rows + the new requested shares must be ≤ 5. Otherwise the request is rejected before any payment order is created. (PENDING shares don't count — they only become real when the webhook flips the row to SUBMITTED.)

The resume path is provided "for free" by `Lib.Payment.Domain.Action.createOrderService`: when called with an `orderId` that already exists in `payment_order`, it returns the stored `CreateOrderResp` (`paymentLinks`, `sdkPayload`, etc.) instead of hitting Juspay again.

`postSubmitApplication` keeps its original guard: it rejects any driver who already has a SUBMITTED application (so a driver wanting to top up gets steered to the new endpoint).

In our example: a driver with 2 shares can request +3 (total 5, OK) but not +4 (total 6, blocked).

## What does the `get` API show after a top-up?

`GET /membership` now aggregates across all the driver's `SUBMITTED` rows:

- `numberOfShares` at the top level = **total across all allotments** (5 in our example).
- `shareAllotments` = **a list, one entry per purchase**, each with its own `applicationId`, `numberOfShares`, `shareStartCount`, `shareEndCount`, `applicationCount`, `submittedAt`. This is how the frontend renders "you own shares 1–2 and 12–14".
- KYC, address, bank, vehicle, nominee, declaration = read from the **latest** allotment (the most recently submitted row), because those values are the driver's current details.

```jsonc
{
  "numberOfShares": 5,
  "shareAllotments": [
    { "applicationId": "A2", "numberOfShares": 3, "shareStartCount": 12, "shareEndCount": 14, "applicationCount": 5, "submittedAt": "2026-05-12T..." },
    { "applicationId": "A1", "numberOfShares": 2, "shareStartCount": 1,  "shareEndCount": 2,  "applicationCount": 1, "submittedAt": "2026-04-03T..." }
  ],
  // Legacy fields — deprecated. Populated from the *latest* allotment (A2 here) for backward compatibility.
  // New clients should read shareAllotments instead.
  "applicationCount": 5,
  "shareStartCount": 12,
  "shareEndCount":   14,
  "address":     { ...latest... },
  "bankDetails": { ...latest... }
}
```

## What does the `update` API change?

`PUT /updateApplication` edits **every `SUBMITTED` and `PENDING` row for the driver** (KYC, address, bank, vehicle, nominee, address-proof). The latest SUBMITTED allotment is used as the source for "keep existing" defaults when a field is omitted from the request; the call requires at least one SUBMITTED row (a driver who only has a PENDING first-purchase hasn't finished onboarding yet).

This keeps the dashboard consistent: any direct DB query on `stcl_membership` rows will see the same address / bank / nominee values for every allotment a driver owns. The per-allotment fields that *should* differ — `applicationId`, `numberOfShares`, `applicationCount`, `shareStartCount`, `shareEndCount`, `createdAt`, `updatedAt` (per-row) — are not touched.

PENDING rows are included so that an in-flight top-up payment carries the up-to-date driver details when the webhook flips it to SUBMITTED. Otherwise the sequence (1) old details → (2) top-up started (PENDING with old details) → (3) update API → (4) payment completes → SUBMITTED with stale details would briefly resurface old data on the dashboard.

Tradeoff: we lose the per-allotment KYC snapshot ("what was the driver's address when shares 1–2 were issued?"). If you ever need that history, it must be reconstructed from audit/event logs, not from this table.

## Why this design

| Question | Answer |
|----------|--------|
| Why not edit the existing row's `numberOfShares` and `shareEndCount`? | The new range (e.g., 12–14) isn't contiguous with the old range (1–2). A single `(shareStartCount, shareEndCount)` pair on one row can't represent two disjoint ranges. |
| Why not introduce a separate `share_allotment` table? | We could, but it requires a schema migration and refactors of every caller. Using one row per purchase keeps the existing table intact and gets us the same correctness. |
| Why not reuse `/submitApplication` for top-ups? | That endpoint requires the full KYC/address/bank payload. A frontend cache could silently overwrite the driver's details on a top-up. A dedicated endpoint that only takes `numberOfShares` keeps the request body tight and eliminates the accidental-overwrite class of bug entirely. |

## Tunables — TransporterConfig.stclConfig

The three knobs that govern the top-up flow live on `TransporterConfig.stclConfig` (per-MOC, stored as one JSON column). Any field left as `null` falls back to a module-level default in `Domain.Action.UI.StclMembership`:

```jsonc
{
  "maxSharesPerDriver":   5,    // cap on total shares per driver across all SUBMITTED rows
  "pricePerShare":        100,  // rupees per share, used when client omits `amount` on /buyAdditionalShares
  "pendingStaleMinutes":  15    // PENDING top-up older than this is retired as REJECTED
}
```

Update via SQL — the whole object is JSON, so set the whole document or merge:

```sql
-- Replace the full object:
UPDATE atlas_driver_offer_bpp.transporter_config
SET stcl_config = '{"pricePerShare":150,"maxSharesPerDriver":5,"pendingStaleMinutes":15}'::json
WHERE merchant_operating_city_id = '<MOC-id>';

-- Or merge a single field:
UPDATE atlas_driver_offer_bpp.transporter_config
SET stcl_config = COALESCE(stcl_config::jsonb, '{}'::jsonb) || '{"pricePerShare":150}'::jsonb
WHERE merchant_operating_city_id = '<MOC-id>';
```

No redeploy needed. The TransporterConfig cache picks up the new value on its next refresh (or call `Storage.CachedQueries.Merchant.TransporterConfig.clearCache` to force it).

## One subtle invariant — Redis counter bootstrap

`atomicIncrCounts` (in `StclMembershipExtra.hs`) uses a Redis counter to hand out the next share range. If Redis loses the counter (restart, flush), `initFromDbAndIncr` re-bootstraps it by reading the **largest existing `shareEndCount`** from the DB and using that as the base.

Because `updateApplication` now touches `updatedAt` on older rows, we changed the bootstrap query (`findLatestSubmittedMembership`) to order by `shareEndCount DESC` instead of `updatedAt DESC`. Otherwise editing an old row would make it look "most recent" and the bootstrap would seed Redis from a stale `shareEndCount` — and the next purchase would collide with already-allocated share numbers.

## TL;DR for a frontend developer

- **First purchase:** call `POST /submitApplication` with the full KYC/address/bank payload (unchanged).
- **Top-up:** call `POST /buyAdditionalShares` with only `{ numberOfShares, amount, paymentServiceType }`. The server reads KYC/address/bank from the driver's most recent SUBMITTED row.
- Both endpoints return a `CreateOrderResp` with the Juspay payment screen URL. Same payment UX in both cases.
- After successful payment, `GET /membership` returns `numberOfShares = total`, with per-purchase allotments in `shareAllotments`.
- Server enforces: cap of 5 total, only one PENDING at a time.
- Legacy fields `applicationCount` / `shareStartCount` / `shareEndCount` are still in the `GET /membership` response (populated from the latest allotment) and will be removed in a later release. New code should read `shareAllotments`.
