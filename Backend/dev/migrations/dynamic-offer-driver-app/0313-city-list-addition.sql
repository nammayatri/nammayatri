ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN lat double precision NOT NULL DEFAULT 12.971599;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN lon double precision NOT NULL DEFAULT 77.594566;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN support_number text;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN language text NOT NULL DEFAULT 'ENGLISH';