-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS pickup_charges_min integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS pickup_charges_max integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS pickup_charges_min_amount double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN IF NOT EXISTS pickup_charges_max_amount double precision;
-- NOTE: dont need to run these queries
