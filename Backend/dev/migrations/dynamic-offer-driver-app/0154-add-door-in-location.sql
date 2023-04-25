ALTER TABLE
    atlas_driver_offer_bpp.search_request_location
ADD
    COLUMN door character varying(255);

ALTER TABLE
    atlas_driver_offer_bpp.booking_location
ADD
    COLUMN door character varying(255);