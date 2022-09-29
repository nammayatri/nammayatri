ALTER TABLE atlas_driver_offer_bpp.search_request_location
    ADD full_address CHARACTER VARYING(255);

ALTER TABLE
   atlas_driver_offer_bpp.booking_location DROP COLUMN door;

ALTER TABLE
   atlas_driver_offer_bpp.search_request_location DROP COLUMN door;