ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN is_part_of_intelligent_pool BOOLEAN;
UPDATE atlas_driver_offer_bpp.search_request_for_driver SET is_part_of_intelligent_pool=FALSE;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN is_part_of_intelligent_pool SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN cancellation_ratio real NULL;
