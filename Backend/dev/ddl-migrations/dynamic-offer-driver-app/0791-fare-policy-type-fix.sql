-- NOTE: dont need to run these queries
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN platform_fee_charge TYPE json USING platform_fee_charge::text::json;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN platform_fee_cgst TYPE double precision USING platform_fee_cgst::double precision;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN platform_fee_sgst TYPE double precision USING platform_fee_sgst::double precision;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
