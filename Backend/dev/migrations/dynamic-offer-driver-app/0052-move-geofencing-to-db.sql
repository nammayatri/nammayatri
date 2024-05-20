-- test data
UPDATE atlas_driver_offer_bpp.merchant
SET origin_restriction = '{Karnataka}', destination_restriction = '{Karnataka}';
------------

ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN origin_restriction SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN destination_restriction SET NOT NULL;
