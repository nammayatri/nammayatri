
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN platform_fee double precision ;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  sgst double precision;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  cgst double precision;
Alter Table atlas_driver_offer_bpp.fare_policy ADD COLUMN  platform_fee_charges_by text;

Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN platform_fee double precision ;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  sgst double precision;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  cgst double precision;
Alter Table atlas_driver_offer_bpp.fare_parameters ADD COLUMN  platform_fee_charges_by text;