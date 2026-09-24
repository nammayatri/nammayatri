-- NOTE: dont need to run these queries
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab
  ALTER COLUMN free_wating_time DROP NOT NULL;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
-- NOTE: dont need to run these queries
