ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request;
