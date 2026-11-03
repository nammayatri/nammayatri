-- TDS disbursement: the recent-uploads list and the quarter cards read a city's batches newest first
-- (findAllActiveByCity), optionally for one financial year.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_tds_distribution_batch_city_created
  ON atlas_driver_offer_bpp.tds_distribution_batch (merchant_operating_city_id, created_at DESC);
