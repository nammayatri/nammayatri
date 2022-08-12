ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_fare double precision;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN estimated_fare double precision;
UPDATE atlas_driver_offer_bpp.booking SET estimated_fare = 0;
UPDATE atlas_driver_offer_bpp.driver_quote SET estimated_fare = 0;
ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN estimated_fare SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN estimated_fare SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
  ALTER COLUMN base_fare SET DATA TYPE integer
  USING round(base_fare);
