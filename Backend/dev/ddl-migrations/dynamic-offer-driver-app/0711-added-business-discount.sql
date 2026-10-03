-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS business_discount DOUBLE PRECISION;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS business_discount_percentage DOUBLE PRECISION;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS should_apply_business_discount BOOLEAN default false;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
