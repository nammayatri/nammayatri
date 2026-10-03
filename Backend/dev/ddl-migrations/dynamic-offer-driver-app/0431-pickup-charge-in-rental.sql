-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details ADD COLUMN IF NOT EXISTS dead_km_fare INTEGER NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS dead_km_fare INTEGER;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds drop constraint fare_policy_driver_extra_fee_bounds_pkey;
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds alter column id drop not null;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
