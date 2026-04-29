-- ADD COLUMN: fare_policy table
ALTER TABLE
    atlas_driver_offer_bpp.fare_policy
ADD COLUMN per_distance_unit_insurance_charge double precision,
ADD COLUMN card_charge_per_distance_unit_multiplier double precision,
ADD COLUMN fixed_card_charge double precision;

-- NOTE: Queries for MASTER
-- ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN per_distance_unit_insurance_charge DROP NOT NULL;
-- UPDATE atlas_driver_offer_bpp.fare_policy SET per_distance_unit_insurance_charge = NULL;
--
-- ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN card_charge_per_distance_unit_multiplier DROP NOT NULL;
-- UPDATE atlas_driver_offer_bpp.fare_policy SET card_charge_per_distance_unit_multiplier = NULL;
--
-- ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN fixed_card_charge DROP NOT NULL;
-- UPDATE atlas_driver_offer_bpp.fare_policy SET fixed_card_charge = NULL;

-- ADD COLUMN: fare_parameters table
ALTER TABLE
    atlas_driver_offer_bpp.fare_parameters
ADD COLUMN insurance_charge double precision,
ADD COLUMN card_charge_on_fare double precision,
ADD COLUMN fixed_card_charge double precision;

-- ADD COLUMN: ride_duration_fare column.
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details ADD COLUMN ride_duration_fare double precision;
------------------------------------------------------------------* END *-------------------------------------------------------------------------
