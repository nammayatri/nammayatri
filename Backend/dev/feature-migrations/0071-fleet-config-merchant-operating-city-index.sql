-- Index for Config Pilot's per-city fleet_config load (findAllByMerchantOperatingCityId),
-- which runs on a cache miss for every city that reads fleet features. Kept here rather
-- than in ddl-migrations; IF NOT EXISTS keeps it safe to re-run.
CREATE INDEX IF NOT EXISTS idx_fleet_config_merchant_operating_city_id ON atlas_driver_offer_bpp.fleet_config USING btree (merchant_operating_city_id);
