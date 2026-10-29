-- The primary key leads with language and senders read by city and message key, so the read gets its own index.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_merchant_message_city_message_key ON atlas_driver_offer_bpp.merchant_message USING btree (merchant_operating_city_id, message_key);
