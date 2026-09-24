-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS extra_duration int default 0;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_rental_details ADD COLUMN IF NOT EXISTS extra_distance int default 0;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
