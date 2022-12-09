ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN blocked boolean;

UPDATE atlas_driver_offer_bpp.driver_information SET blocked = false;

ALTER TABLE atlas_driver_offer_bpp.driver_information ALTER COLUMN blocked SET NOT NULL;