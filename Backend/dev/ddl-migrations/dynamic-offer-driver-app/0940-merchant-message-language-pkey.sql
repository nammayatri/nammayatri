------------------------------------------------------------------------------------------------
-- NOTE: This is to fix local schema to make it similar as master schema for config-sync import.
-- Do not run in master or prod
------------------------------------------------------------------------------------------------

-- A fresh database runs migrations-read-only/merchant_message.sql before the ddl migrations, so 0108 and 0294
-- put back a primary key without language; this restores the (language, city, key) one that file sets.
ALTER TABLE atlas_driver_offer_bpp.merchant_message DROP CONSTRAINT IF EXISTS merchant_message_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_message ADD PRIMARY KEY (language, merchant_operating_city_id, message_key);
