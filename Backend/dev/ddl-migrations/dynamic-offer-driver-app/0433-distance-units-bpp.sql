-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN IF NOT EXISTS distance_unit character varying(255);
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD COLUMN distance_unit character varying(255);
-- NOTE: dont need to run these queries
