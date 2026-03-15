-- Drop indices that are now subsumed by composite upgrades created in 1492.
-- Must run AFTER 1492 indices are confirmed live and serving queries.
-- Uses CONCURRENTLY to avoid holding AccessExclusive lock during drop.

-- idx_ride_booking_id (booking_id)
-- subsumed by idx_ride_booking_id_created (booking_id, created_at DESC)
DROP INDEX CONCURRENTLY IF EXISTS atlas_app.idx_ride_booking_id;

-- idx_estimate_request_id (request_id)
-- subsumed by idx_estimate_request_id_status (request_id, status)
DROP INDEX CONCURRENTLY IF EXISTS atlas_app.idx_estimate_request_id;
