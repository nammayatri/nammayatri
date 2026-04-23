-- Drop indices that are now subsumed by composite upgrades created in 0776.
-- Must run AFTER 0776 indices are confirmed live and serving queries.
-- Uses CONCURRENTLY to avoid holding AccessExclusive lock during drop.

-- idx_driver_quote_s_try_id (search_try_id)
-- subsumed by idx_driver_quote_search_try_status (search_try_id, status)
DROP INDEX CONCURRENTLY IF EXISTS atlas_driver_offer_bpp.idx_driver_quote_s_try_id;

-- idx_ride_driver_id_and_status (driver_id, status)
-- subsumed by idx_ride_driver_id_status_created (driver_id, status, created_at DESC)
DROP INDEX CONCURRENTLY IF EXISTS atlas_driver_offer_bpp.idx_ride_driver_id_and_status;
