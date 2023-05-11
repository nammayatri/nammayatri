ALTER TABLE
    atlas_driver_offer_bpp.driver_pool_config
ADD
    COLUMN config_start_time integer;

ALTER TABLE
    atlas_driver_offer_bpp.driver_pool_config
ADD
    COLUMN config_end_time integer;