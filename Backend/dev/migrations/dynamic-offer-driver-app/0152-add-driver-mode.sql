ALTER TABLE
    atlas_driver_offer_bpp.driver_information
ADD
    COLUMN mode text;

ALTER TABLE
    atlas_driver_offer_bpp.search_request_for_driver
ADD
    COLUMN mode text;