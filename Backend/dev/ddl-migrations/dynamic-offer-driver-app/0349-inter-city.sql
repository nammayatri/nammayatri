

ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ALTER COLUMN state SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN state SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.geometry ADD COLUMN state text;

ALTER TABLE atlas_driver_offer_bpp.geometry ALTER COLUMN state SET NOT NULL;
