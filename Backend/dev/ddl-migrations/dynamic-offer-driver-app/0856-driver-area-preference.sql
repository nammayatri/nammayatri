-- Driver preferred-area preference (stored as tags on person.driver_tag -- see
-- SharedLogic.DriverPool.AreaPreference -- so no driver_information columns are
-- needed here), and the min-cells config used to validate a cells-method selection.

-- Backfills a column that commit d4d69daf01 ("added preference score for driver
-- pooling") shipped without a DDL migration. That commit is already pushed, so an
-- environment running that branch may have had this column added by hand --
-- IF NOT EXISTS keeps this migration replayable there.
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN IF NOT EXISTS is_pet_ride boolean;

-- Fewest cells a driver must select for a cells-method area preference to be
-- accepted. Nullable: unset falls back to the in-code default (1).
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN IF NOT EXISTS area_preference_min_cells integer;


CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.geohash_area (
  id character varying(36) NOT NULL PRIMARY KEY,
  geohash text NOT NULL,
  area_name text NOT NULL,
  merchant_id character varying(36),
  merchant_operating_city_id character varying(36),
  created_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_geohash_area_moc_id ON atlas_driver_offer_bpp.geohash_area (merchant_operating_city_id);
