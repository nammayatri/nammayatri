-- Indexes to eliminate expensive sorts for rider my-rides / journey list APIs
-- that caused rider-db-cluster read-pool saturation and primary CPU alert.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_booking_rider_id_status_start_time
  ON atlas_app.booking (rider_id, status, start_time DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_journey_rider_id_status_created_at
  ON atlas_app.journey (rider_id, status, created_at DESC);
