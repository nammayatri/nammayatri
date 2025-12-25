ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN transaction_id character(36);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN transaction_id character(36);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id SET NOT NULL;
