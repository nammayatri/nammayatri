-- NOTE: dont need to run these queries
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details ALTER COLUMN platform_fee TYPE double precision;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
-- NOTE: dont need to run these queries
