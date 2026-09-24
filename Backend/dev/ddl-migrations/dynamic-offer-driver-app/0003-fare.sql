-- NOTE: dont need to run these queries
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN base_fare TO fare_for_pickup;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD CHECK (fare_for_pickup > 0);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS fare_per_km double precision NOT NULL CHECK (fare_per_km > 0) DEFAULT 12;
DROP TABLE IF EXISTS atlas_driver_offer_bpp.fare_policy_per_extra_km_rate;
-- NOTE: dont need to run these queries
