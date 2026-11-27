-- ddl-migrations/0524's unique constraint has no language column, so it blocks a second language row
-- for a key with a vehicle category; the primary key (language, city, key) already keeps rows unique.
ALTER TABLE atlas_driver_offer_bpp.merchant_message DROP CONSTRAINT IF EXISTS unique_on_merchant_id_message_key;
