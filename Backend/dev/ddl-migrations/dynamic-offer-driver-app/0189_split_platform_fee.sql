-- NOTE: dont need to run these queries
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details ADD COLUMN IF NOT EXISTS sgst numeric (30,2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_slab_details ADD COLUMN IF NOT EXISTS cgst numeric (30,2);
-- NOTE: dont need to run these queries
