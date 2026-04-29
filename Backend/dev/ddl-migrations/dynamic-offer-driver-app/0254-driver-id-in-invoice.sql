

ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN platform_fee type numeric(30,2) USING platform_fee::numeric(30,2);