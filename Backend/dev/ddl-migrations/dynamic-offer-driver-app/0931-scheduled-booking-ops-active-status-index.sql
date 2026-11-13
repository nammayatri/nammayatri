-- Narrows the scheduled-ops partial index to the live ops status set -- NEW / TRIP_ASSIGNED /
-- REALLOCATED. The old idx_booking_scheduled_ops was partial on is_scheduled alone, so it also
-- carried CONFIRMED / AWAITING_REASSIGNMENT / COMPLETED / CANCELLED scheduled rows that the live
-- view never reads. Folding the status set into the predicate shrinks the index to the live slice
-- while keeping the (merchant_operating_city_id, start_time) ordering the LIMIT relies on.
--
-- A partial index is only used when the query's status set is a SUBSET of this predicate.
-- REQUIRED COMPANION CHANGE: getScheduledBookingList's live branch currently sends
--   status IN (NEW, TRIP_ASSIGNED, CANCELLED, REALLOCATED)
-- (ScheduledBooking.hs: `windowStatuses <> [SRB.CANCELLED, SRB.REALLOCATED]`). CANCELLED is NOT in
-- this predicate, so the live query will NOT use this index until that append drops CANCELLED
-- (-> `<> [SRB.REALLOCATED]`), making the live set exactly {NEW, TRIP_ASSIGNED, REALLOCATED}.
--
-- NOTE: queries that read terminal statuses still can NOT use this index -- the history list
-- (NEW/TRIP_ASSIGNED/COMPLETED/CANCELLED/REALLOCATED) and findScheduledStatusCountsByCitiesLite
-- (cityWiseCount, which counts every status). Update that function's index-usage comment accordingly.
--
-- Create the new index first, then drop the old one, so there is never a window without index
-- coverage. Both run CONCURRENTLY -- booking is a hot table and neither should take an
-- ACCESS EXCLUSIVE lock. CONCURRENTLY cannot run inside a transaction block.

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_scheduled_ops_active
  ON atlas_driver_offer_bpp.booking (merchant_operating_city_id, start_time)
  WHERE is_scheduled = true
    AND status IN ('NEW', 'TRIP_ASSIGNED', 'REALLOCATED');

DROP INDEX CONCURRENTLY IF EXISTS atlas_driver_offer_bpp.idx_booking_scheduled_ops;
