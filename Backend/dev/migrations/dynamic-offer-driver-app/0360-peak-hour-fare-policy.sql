ALTER TABLE
    atlas_driver_offer_bpp.fare_product
ADD
    COLUMN time_bounds text DEFAULT 'Unbounded' NOT NULL;