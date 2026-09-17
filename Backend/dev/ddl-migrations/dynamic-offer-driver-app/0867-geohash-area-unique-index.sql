-- One label per (city, geohash) -- lets the dashboard bulk-upsert endpoint update instead of duplicate.
-- Guarded: 0862-geohash-area-unique-index.sql adds the same constraint and always runs first,
-- so this must be a no-op rather than a hard failure. ADD CONSTRAINT has no IF NOT EXISTS.
DO $$
BEGIN
  ALTER TABLE atlas_driver_offer_bpp.geohash_area
    ADD CONSTRAINT geohash_area_unique_idx_merchant_operating_city_id_geohash
    UNIQUE (merchant_operating_city_id, geohash);
EXCEPTION
  WHEN duplicate_table THEN NULL;
  WHEN duplicate_object THEN NULL;
END $$;
