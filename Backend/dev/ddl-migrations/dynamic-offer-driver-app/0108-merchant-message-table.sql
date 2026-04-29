
--ONLY FOR LOCAL SYNC
ALTER TABLE atlas_driver_offer_bpp.merchant_message DROP CONSTRAINT merchant_message_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ALTER COLUMN merchant_operating_city_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD PRIMARY KEY (merchant_id, message_key);