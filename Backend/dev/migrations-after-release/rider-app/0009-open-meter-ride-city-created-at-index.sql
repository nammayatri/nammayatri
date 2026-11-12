-- The dashboard's open meter ride list (GET /openMeter/ride/list) filters on merchant_operating_city_id
-- and orders by created_at DESC; without a customerId it reads Postgres, so it needs this index.
CREATE INDEX CONCURRENTLY IF NOT EXISTS open_meter_ride_idx_merchant_operating_city_id_created_at_desc ON atlas_app.open_meter_ride USING btree (merchant_operating_city_id, created_at DESC);
