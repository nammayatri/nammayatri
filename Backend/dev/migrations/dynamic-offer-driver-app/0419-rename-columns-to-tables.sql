-- this is only for local testing purposes, dont run in prod or master

ALTER TABLE atlas_driver_offer_bpp.driver_quote RENAME COLUMN request_id TO search_request_id;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver RENAME COLUMN request_id TO search_request_id;