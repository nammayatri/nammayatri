ALTER TABLE
    atlas_driver_offer_bpp.driver_information
ADD
    COLUMN can_downgrade_to_hatchback boolean DEFAULT false NOT NULL;

ALTER TABLE
    atlas_driver_offer_bpp.driver_information
ADD
    COLUMN can_downgrade_to_sedan boolean DEFAULT false NOT NULL;

ALTER TABLE
    atlas_driver_offer_bpp.driver_information
ADD
    COLUMN can_downgrade_to_taxi boolean DEFAULT false NOT NULL;
