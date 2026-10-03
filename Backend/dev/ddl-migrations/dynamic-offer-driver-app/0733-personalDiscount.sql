-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS personal_discount DOUBLE PRECISION;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS personal_discount_percentage DOUBLE PRECISION;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS should_apply_personal_discount BOOLEAN default false;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
