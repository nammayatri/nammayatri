CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_cancellation_dues_details_ride_id_unique
  ON atlas_driver_offer_bpp.cancellation_dues_details (ride_id);
