-- Add state_entry_permit_charges column to fare_policy and fare_parameters tables
-- This column stores state entry permit charges that apply to rides crossing state/city boundaries
-- The charges are calculated per segment for multi-stop journeys

-- Add state_entry_permit_charges to fare_policy table
ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN IF NOT EXISTS state_entry_permit_charges double precision;

-- Add state_entry_permit_charges to fare_parameters table
ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS state_entry_permit_charges double precision;

-- Remove state_entry_permit_charges from fare_policy_inter_city_details table
-- This field is now stored at the fare_policy level instead
ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details
DROP COLUMN IF EXISTS state_entry_permit_charges;

-- Remove state_entry_permit_charges from fare_parameters_inter_city_details table
-- This field is now stored at the fare_parameters level instead
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_inter_city_details
DROP COLUMN IF EXISTS state_entry_permit_charges;