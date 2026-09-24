-- NOTE: dont need to run these queries
-- Update fare policy table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS waiting_charge_per_min double precision;

-- Update fare params table to include waiting charges
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS waiting_charge_per_min double precision;
-- NOTE: dont need to run these queries
