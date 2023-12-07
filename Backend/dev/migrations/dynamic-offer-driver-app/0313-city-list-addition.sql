ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN location json NOT NULL DEFAULT '{"lat":12.971599, "lon":77.594566}';
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN support_number text;
ALTER TABLE atlas_driver_offer_bpp.merchant_operating_city ADD COLUMN language text NOT NULL DEFAULT 'ENGLISH';