

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN base_fare DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN search_repeat_type SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN base_fare SET NOT NULL;