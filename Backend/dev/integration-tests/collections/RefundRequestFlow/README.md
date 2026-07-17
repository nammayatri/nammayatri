# RefundRequestFlow collections

Two independent families of integration tests, run via the local test dashboard.

## 00–07 — refund regression suite (SEQUENTIAL)

`00` creates one completed Stripe-charged ride; `01`–`07` then refund it from every angle.
**Run strictly in order, 00 → 07, in one sitting** — they share the fixture ride.

| # | covers |
|---|---|
| 00 | fixture: real card-charged ride |
| 01 | driver-deducted refund, clawback accounting |
| 02 | platform-absorbed refund |
| 03 | async resolution |
| 04 | failure + retry |
| 05 | admin entry points |
| 06 | caps + multi-component refunds |
| 07 | customer-surface validation |

## CF1–CF3 — cancellation-fee flows (each STANDALONE)

Each collection onboards its own driver/rider, flips its own feature flags
(`enable_payment_refunds`, `enable_cancellation_commission`), seeds its own
cancellation-dues JsonLogic (fee 5.00 + 1.20 VAT; CF2 adds overdue 4.00 + 0.96) and cleans
up after itself. **Run individually — any order, any subset.** Each runs TWO cancelled/
completed rides because every collection proves a *contrast* a single ride can't show.

### CF1-CancellationSettleNow — the kill-switch pair
Fee is charged to the card immediately at cancel (driver cancels CUSTOMER_NO_SHOW after
the no-show window) in both rides; only the commission flag differs.
- **Ride 1 — flag OFF:** fee 6.20 charged, and the test proves **no** commission books
  anywhere (no legs, no Commission invoice) — the kill-switch works.
- **Ride 2 — flag ON:** same scenario; commission **0.93** books at cancel; then two
  half-cap refunds (3.10 + 3.10, driver-deducted) each claw back 0.465; a third refund is
  cap-rejected; the AggregatedCommission job nets booked − clawed to **exactly 0**.

### CF2-CancellationOverdue — the two overdue endings
In both rides the card charge FAILS at cancel, so the fee goes overdue: the driver is owed
only the reduced 4.00 + 0.96 and the platform would keep the 1.00 + 0.24 difference
("benefit"). The rides differ in what happens *before* the customer finally pays.
- **Ride A — settled AFTER overdue (reversal road):** customer clears dues *before* any
  driver payout → the overdue + benefit ledger legs are **reversed**, the full fee re-books,
  and the FULL 0.93 commission books. Refunds then show plain full-fee math with **zero**
  benefit-refund legs — proving the reversal netting is honored.
- **Ride B — overdue STANDS (payout road):** the driver payout is simulated first (overdue
  legs marked paid out) → clearing dues keeps the overdue split; only **0.744** commission
  (15% of the reduced 4.96) books. Refunds return **three buckets** pro-rata: driver share +
  commission clawback + the platform giving back its kept benefit
  (`CancellationOverdueBenefitRefund(+Tax)` 1.00 + 0.24). Conservation: all buckets sum to
  the 6.20 the customer gets back.

### CF3-CancellationDueOnNextRide — the deferred fee
One scenario that *needs* two rides: the due born on ride 1 is collected on ride 2
(`settle_cancellation_fee_before_next_ride = false`).
- **Ride 1 — cancelled, fee defers:** nothing is charged, **no** ledger legs book; the dues
  record sits PENDING with the frozen 0.93 commission candidate.
- **Ride 2 — completes and carries the fee:** ONE card charge = ride fare + the carried
  6.20 (fareBreakup shows RIDE_FARE + CANCELLATION_FEE). At EndRide the commission books
  against **ride 2's** booking; the dues record flips PAID. Refunds target ride 2's
  CANCELLATION_FEE component and the clawback reads **ride 2's** ledger — the hard case
  where the commission lives on a different booking than the cancelled ride. Aggregation
  carries ride 2's ride commission while netting the cancellation slice to zero.

## How to run

1. Full local stack up with a synced/seeded dev DB.
2. `, run-local-test-dashboard` → UI on **:7070**.
3. Pick the collection + the `Local_BF_Helsinki` environment → Run.

## Do NOT interleave the families

Never mix runs like `00 → CF2 → 03`. The 00–07 chain depends on shared fixture/env state
that a CF run's setup and cleanup will overwrite. Finish one family before starting the
other. (Re-running any single CF is always safe — they reset their own state.)
