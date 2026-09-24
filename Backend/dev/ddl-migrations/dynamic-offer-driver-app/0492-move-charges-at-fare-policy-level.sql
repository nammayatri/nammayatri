-- NOTE: dont need to run these queries

Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN IF NOT EXISTS platform_fee double precision ;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  IF NOT EXISTS sgst double precision;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  IF NOT EXISTS cgst double precision;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  IF NOT EXISTS platform_fee_charges_by text;

Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN IF NOT EXISTS platform_fee double precision ;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  IF NOT EXISTS sgst double precision;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  IF NOT EXISTS cgst double precision;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  IF NOT EXISTS platform_fee_charges_by text;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
