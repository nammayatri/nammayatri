ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN driver_rating double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request DROP COLUMN gateway_uri;
