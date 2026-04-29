ALTER TABLE atlas_driver_offer_bpp.driver_location ADD COLUMN merchant_id character(36);

ALTER TABLE atlas_driver_offer_bpp.driver_location
ALTER COLUMN merchant_id set default '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';
ALTER TABLE atlas_driver_offer_bpp.driver_location
ALTER COLUMN merchant_id set NOT NULL;
