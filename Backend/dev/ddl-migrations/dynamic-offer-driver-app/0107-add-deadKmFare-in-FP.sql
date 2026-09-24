-- NOTE: dont need to run these queries
alter table atlas_driver_offer_bpp.fare_parameters add column IF NOT EXISTS dead_km_fare double precision;
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN dead_km_fare SET DATA TYPE integer
  USING round(dead_km_fare);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
