# Premium Bus — Impact Analysis & Validation Queries

**Date:** 2026-03-17
**PR:** https://github.com/nammayatri/nammayatri/pull/14007 (backend), https://github.com/nammayatri/ny-react-native/pull/6146 (frontend)
**Launch date:** 2026-03-11 (premium bus app-only booking)
**Data source:** ClickHouse `atlas_app` database

---

## Executive Impact Summary

| Issue | Finding ID | Est. % Users Affected | Revenue Impact | Confidence |
|---|---|---|---|---|
| Payment deducted, no ticket | F14, F16 | ~30% of bus booking attempts | Rs 50-200/incident | High — from review analysis + fix-plan-booking.md |
| Stuck cancellations (refund deadlock) | F7 | ~5-10% of cancellation attempts | Rs 50-200/incident (locked funds) | Medium — from RCA, no live data |
| Bookings for departed buses | PB-3 | ~3-5% of premium bus bookings | Rs 50-200/incident | Medium — from user reviews |
| Duplicate tickets on BPP retry | F14 | ~1-3% of confirmed bookings | Double-charge risk | Medium — from code analysis |
| Pass verification rejects valid passes | BUG-14 | Unknown (PreBooked → Active gap) | Pass holders blocked at boarding | Low — needs live data |
| OnInit partial-update crash | F16 | ~2-5% of bookings during peak | Booking stuck in inconsistent state | Medium — from code analysis |
| Wrong direction QR (frontend) | Bug 2 | Reported in multiple 1-star reviews | Trust erosion | High — from review data |
| Test OTPs in production (frontend) | Bug 1 | Low (5 specific OTPs affected) | Data integrity risk | High — confirmed in code |

**Estimated overall premium bus booking success rate: ~30-40%** (based on review complaint rate of 19.1% in Mar 2026 + payment failure analysis in reports/fix-plan-booking.md showing `postFrfsQuotePaymentRetry = error "Logic yet to be decided"`)

---

## Validation Queries

> **Instructions:** Run each query against ClickHouse (`atlas_app` database) before and after deployment. Record results to measure fix impact.

### Q1. Overall Bus Booking Funnel (since premium launch)

```sql
SELECT
    status,
    count(*) AS booking_count,
    count(DISTINCT rider_id) AS unique_riders,
    round(count(*) * 100.0 / sum(count(*)) OVER (), 2) AS pct_of_total
FROM atlas_app.frfs_ticket_booking FINAL
WHERE vehicle_type = 'BUS'
  AND created_at >= '2026-03-11'
GROUP BY status
ORDER BY booking_count DESC
```

**Expected baseline:** CONFIRMED < 50%, FAILED + PAYMENT_PENDING > 30%
**Post-fix target:** CONFIRMED > 80%, FAILED < 10%

---

### Q2. Payment Failure Rate (money deducted, no ticket)

```sql
SELECT
    count(DISTINCT CASE
        WHEN pt.status = 'CHARGED' AND tb.status NOT IN ('CONFIRMED', 'CANCELLED')
        THEN tb.rider_id
    END) AS affected_riders,
    count(DISTINCT tb.rider_id) AS total_riders,
    round(
        count(DISTINCT CASE
            WHEN pt.status = 'CHARGED' AND tb.status NOT IN ('CONFIRMED', 'CANCELLED')
            THEN tb.rider_id
        END) * 100.0 / nullIf(count(DISTINCT tb.rider_id), 0),
    2) AS pct_affected
FROM atlas_app.frfs_ticket_booking FINAL AS tb
INNER JOIN atlas_app.frfs_ticket_booking_payment FINAL AS tbp
    ON tb.id = tbp.frfs_ticket_booking_id
INNER JOIN atlas_app.payment_order FINAL AS po
    ON tbp.payment_order_id = po.id
INNER JOIN atlas_app.payment_transaction FINAL AS pt
    ON po.id = pt.payment_order_id
WHERE tb.vehicle_type = 'BUS'
  AND tb.created_at >= '2026-03-11'
```

**Expected baseline:** ~30% affected (from reports/fix-plan-booking.md analysis)
**Post-fix target:** < 5%

---

### Q3. Duplicate Tickets (OnConfirm idempotency gap — F14)

```sql
SELECT
    tb.id AS booking_id,
    tb.rider_id,
    count(t.id) AS actual_ticket_count,
    tb.quantity AS expected_ticket_count,
    tb.created_at
FROM atlas_app.frfs_ticket_booking FINAL AS tb
INNER JOIN atlas_app.frfs_ticket FINAL AS t
    ON t.frfs_ticket_booking_id = tb.id
WHERE tb.vehicle_type = 'BUS'
  AND tb.created_at >= '2026-03-11'
GROUP BY tb.id, tb.rider_id, tb.quantity, tb.created_at
HAVING count(t.id) > tb.quantity
ORDER BY tb.created_at DESC
```

**Expected:** Non-zero rows = duplicate ticket incidents
**Post-fix target:** Zero new duplicates after deployment

---

### Q4. Stuck Cancellations (refund mismatch deadlock — F7)

```sql
-- Bookings where BPP sent cancel data but booking is still CONFIRMED
SELECT
    count(*) AS stuck_cancellations,
    count(DISTINCT rider_id) AS affected_riders,
    sum(price) AS total_locked_amount
FROM atlas_app.frfs_ticket_booking FINAL
WHERE vehicle_type = 'BUS'
  AND status = 'CONFIRMED'
  AND cancellation_charges IS NOT NULL
  AND refund_amount IS NOT NULL
  AND created_at >= '2026-03-11'
```

**Expected:** Non-zero = users stuck with no refund despite BPP accepting cancel
**Post-fix target:** Zero (tolerance-based validation prevents deadlock)

---

### Q5. Bookings Created After Departure (no departure validation — PB-3)

```sql
-- Proxy: bookings where ticket validity expired very quickly (suggests bus already departed)
SELECT
    count(*) AS likely_post_departure_bookings,
    count(DISTINCT rider_id) AS affected_riders,
    sum(price) AS total_wasted_amount
FROM atlas_app.frfs_ticket_booking FINAL
WHERE vehicle_type = 'BUS'
  AND status IN ('CONFIRMED', 'FAILED', 'PAYMENT_PENDING')
  AND valid_till < created_at + INTERVAL 10 MINUTE
  AND created_at >= '2026-03-11'
```

**Expected:** Non-zero = users who booked buses that departed <10min after booking
**Post-fix target:** Zero (booking rejected if departure < 10min away)

---

### Q6. Refund Processing SLA (should be <24h per RBI/NPCI mandate)

```sql
SELECT
    count(*) AS total_cancelled,
    count(CASE WHEN tbp.status = 'REFUNDED' THEN 1 END) AS refunded,
    count(CASE WHEN tbp.status = 'REFUND_PENDING' THEN 1 END) AS pending,
    count(CASE WHEN tbp.status = 'REFUND_FAILED' THEN 1 END) AS failed,
    round(avg(CASE
        WHEN tbp.status = 'REFUNDED'
        THEN dateDiff('hour', tb.updated_at, tbp.updated_at)
    END), 1) AS avg_refund_hours,
    max(CASE
        WHEN tbp.status = 'REFUNDED'
        THEN dateDiff('hour', tb.updated_at, tbp.updated_at)
    END) AS max_refund_hours
FROM atlas_app.frfs_ticket_booking FINAL AS tb
INNER JOIN atlas_app.frfs_ticket_booking_payment FINAL AS tbp
    ON tb.id = tbp.frfs_ticket_booking_id
WHERE tb.vehicle_type = 'BUS'
  AND tb.status = 'CANCELLED'
  AND tb.created_at >= '2026-03-11'
```

**Expected baseline:** avg > 48h (from reports: "6+ day processing time")
**Post-fix target:** avg < 24h

---

### Q7. Daily Premium Bus Booking Trend (monitor post-deploy)

```sql
SELECT
    toDate(created_at) AS dt,
    count(*) AS total_bookings,
    count(CASE WHEN status = 'CONFIRMED' THEN 1 END) AS confirmed,
    count(CASE WHEN status = 'FAILED' THEN 1 END) AS failed,
    count(CASE WHEN status = 'CANCELLED' THEN 1 END) AS cancelled,
    round(count(CASE WHEN status = 'CONFIRMED' THEN 1 END) * 100.0
          / nullIf(count(*), 0), 1) AS success_rate_pct,
    count(DISTINCT rider_id) AS unique_riders
FROM atlas_app.frfs_ticket_booking FINAL
WHERE vehicle_type = 'BUS'
  AND created_at >= '2026-03-11'
GROUP BY dt
ORDER BY dt
```

**Expected:** success_rate_pct should increase from ~30-40% to >80% after deploy

---

### Q8. Orphaned Payments (double-charge risk)

```sql
SELECT
    count(*) AS orphaned_payments,
    count(DISTINCT tb.rider_id) AS affected_riders,
    sum(po.amount) AS total_orphaned_amount
FROM atlas_app.frfs_ticket_booking FINAL AS tb
INNER JOIN atlas_app.frfs_ticket_booking_payment FINAL AS tbp
    ON tb.id = tbp.frfs_ticket_booking_id
INNER JOIN atlas_app.payment_order FINAL AS po
    ON tbp.payment_order_id = po.id
INNER JOIN atlas_app.payment_transaction FINAL AS pt
    ON po.id = pt.payment_order_id
WHERE tb.vehicle_type = 'BUS'
  AND pt.status = 'CHARGED'
  AND tb.status IN ('FAILED', 'NEW', 'PAYMENT_PENDING')
  AND tb.created_at >= '2026-03-11'
```

**Expected:** Non-zero = money taken but no ticket issued
**Post-fix target:** Zero new orphans after deployment

---

## How to Run

```bash
# Via ClickHouse HTTP interface:
curl "http://<CH_HOST>:8123/?database=atlas_app" \
  --user "<user>:<password>" \
  --data-binary @query.sql

# Via clickhouse-client:
clickhouse-client --host <CH_HOST> --user <user> --database atlas_app < query.sql
```

---

## Post-Deployment Monitoring

1. Run Q1 + Q7 daily for 7 days after deployment
2. Compare Q2, Q3, Q4, Q5, Q8 before vs after (should trend to zero)
3. Set up alerts (see reports/premium-bus-fix-and-test-strategy.md section PB-8) for:
   - `premium_bus_booking_total{status="failed"}` > 30% → P0
   - `premium_bus_expired_slot_booking` > 0 → P0
   - `premium_bus_noshow_total` > 3/day → P1

---

*Generated: 2026-03-17 | Sources: Backend RCA (16 findings), Frontend RCA (6 bugs), 32 analysis reports, production review data*
