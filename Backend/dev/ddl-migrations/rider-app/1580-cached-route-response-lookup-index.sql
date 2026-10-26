-- Route-cache lookup (findByRiderIdAndGeohashAndHourRangeAndToll) runs on every search with
-- enableRouteCacheRead, and findAllWithKV always queries Postgres too; without this it seq-scans the table.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_cached_route_response_rider_geohash
    ON atlas_app.cached_route_response USING btree (rider_id, pickup_geohash, drop_geohash);
