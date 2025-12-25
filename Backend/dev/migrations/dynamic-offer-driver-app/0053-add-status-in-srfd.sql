
UPDATE atlas_driver_offer_bpp.search_request_for_driver SET status = 'Inactive';

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN status SET NOT NULL;