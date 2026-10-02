-- One free month for the fleet that is already on the road.
--
--   docker cp driver-subscription-free-month.sql ny-postgres:/tmp/ && \
--   docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/driver-subscription-free-month.sql
--
-- ── The decision this settles ──────────────────────────────────────────────
-- `driver-subscription.sql` deliberately leaves every driver at `paid_until
-- NULL` -- state `never` -- and the README says why: nobody had decided what
-- happens to drivers already working. Decided 2026-08-26: **a free month**, on
-- the grounds that all 33 are test drivers and not one is a real person.
--
-- That reasoning is worth writing down because it expires. This is the right
-- answer for a fleet of simulated drivers; it is not automatically the right
-- answer on the day real drivers are enrolled, and re-running this file then
-- would quietly hand a free month to people who should have been billed.
--
-- ── Why it cannot double-give ──────────────────────────────────────────────
-- `WHERE NOT EXISTS` rather than an upsert. Running this twice must not extend
-- anybody twice, and more importantly it must never overwrite a date somebody
-- actually paid for -- a driver who pays tomorrow and then has this file run
-- again would silently lose whatever he had bought beyond the free month.
--
-- ── Not recorded as a payment, on purpose ──────────────────────────────────
-- Nothing is written to `movin.subscription_payment`. No money moved, so no
-- receipt number is drawn: a receipt is a record of a payment, and issuing one
-- for 0 DA would put a hole in a numbered series that has to stand up to being
-- checked.
--
-- The consequence is visible in the app and is the intended behaviour:
-- `/subscription/status` answers `state: active` with `lastPayment: null`, and
-- that combination means exactly one thing -- the free month. The driver's
-- screen says *Offert* rather than inventing a payment he never made.

BEGIN;

INSERT INTO movin.subscription (driver_id, paid_until)
SELECT p.id, now() + interval '30 days'
  FROM atlas_driver_offer_bpp.person p
 WHERE p.role = 'DRIVER'
   AND NOT EXISTS (SELECT 1 FROM movin.subscription s WHERE s.driver_id = p.id);

COMMIT;

\echo == the fleet now
SELECT state, count(*), min(days_left) AS min_days, max(days_left) AS max_days
  FROM movin.driver_subscription_state
 GROUP BY state ORDER BY state;
