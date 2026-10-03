-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN IF NOT EXISTS airport_convenience_fee double precision;


ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS airport_convenience_fee double precision;
-- NOTE: dont need to run these queries
