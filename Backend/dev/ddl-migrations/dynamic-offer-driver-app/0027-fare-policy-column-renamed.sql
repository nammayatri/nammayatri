-- NOTE: dont need to run these queries
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy
    RENAME COLUMN base_distance_per_km_fare TO base_distance_fare;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
