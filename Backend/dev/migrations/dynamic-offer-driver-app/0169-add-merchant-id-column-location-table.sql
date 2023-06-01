ALTER TABLE atlas_driver_offer_bpp.driver_location ADD COLUMN merchant_id character(36);

UPDATE atlas_driver_offer_bpp.driver_location SET merchant_id = (SELECT merchant_id FROM atlas_driver_offer_bpp.person WHERE atlas_driver_offer_bpp.driver_location.driver_id = atlas_driver_offer_bpp.person.id);

ALTER TABLE atlas_driver_offer_bpp.driver_location
ALTER COLUMN merchant_id set default '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';
ALTER TABLE atlas_driver_offer_bpp.driver_location
ALTER COLUMN merchant_id set NOT NULL;
