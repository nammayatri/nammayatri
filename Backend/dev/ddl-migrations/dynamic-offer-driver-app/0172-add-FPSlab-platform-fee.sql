-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS platform_fee_charge integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS platform_fee_cgst integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS platform_fee_sgst integer;

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_parameters_slab_details (
  fare_parameters_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.fare_parameters(id),
  platform_fee integer
);
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details OWNER TO atlas_driver_offer_bpp_user;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
