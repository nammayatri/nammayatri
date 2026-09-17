-- Ops "scheduled bookings" list (getScheduledBookingList) filters on
-- merchant_operating_city_id + is_scheduled + status and ranges over start_time, ordered by
-- start_time with a small LIMIT. The only usable index was booking_start_time, which matched on
-- start_time alone: EXPLAIN showed 455k rows bitmap-scanned (bitmap going lossy, ~367MB of buffers)
-- to return 4 rows, 529ms. Leading with the city and keeping start_time second lets the scan return
-- rows already ordered, so the LIMIT stops early with no sort. Partial on is_scheduled because
-- scheduled bookings are a small slice of the table -- the predicate must stay identical to the
-- query's `is_scheduled = true` or the index will not be used (the column is nullable, so this
-- excludes both NULL and false, exactly as the query does).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_scheduled_ops
  ON atlas_driver_offer_bpp.booking (merchant_operating_city_id, start_time)
  WHERE is_scheduled = true;
