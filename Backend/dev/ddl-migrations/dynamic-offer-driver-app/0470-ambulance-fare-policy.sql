-- NOTE: dont need to run these queries
-- fare_parameters_ambulance_details
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_parameters_ambulance_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS dist_based_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS platform_fee numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS cgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS sgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN IF NOT EXISTS currency character varying(255) NOT NULL;
DO $$ BEGIN

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD PRIMARY KEY (fare_parameters_id);
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

-- fare_policy_ambulance_details_slab
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_policy_ambulance_details_slab();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS id serial PRIMARY KEY;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS vehicle_age int NOT NULL; -- months(should we still take numeric?)
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS base_fare numeric(30, 2) NOT NULL; -- value?
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS per_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS night_shift_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS waiting_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS free_waiting_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS platform_fee_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS platform_fee_cgst double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN IF NOT EXISTS platform_fee_sgst double precision;

alter table atlas_driver_offer_bpp.quote_special_zone add column min_estimated_fare double precision;
alter table atlas_driver_offer_bpp.quote_special_zone add column max_estimated_fare double precision;
-- NOTE: dont need to run these queries
