-- Composite index on (merchant_operating_city_id, key, trip_category)
-- Covers all three query patterns via leftmost prefix:
--   findAllByMerchantOpCityId                             -> uses (merchant_operating_city_id)
--   findAllByMerchantOpCityIdAndMessageKey                 -> uses (merchant_operating_city_id, key)
--   findAllByMerchantOpCityAndMessageKeyAndTripCategory    -> uses (merchant_operating_city_id, key, trip_category)

CREATE INDEX IF NOT EXISTS merchant_push_notification_mocid_key_trip_idx
  ON atlas_app.merchant_push_notification USING btree (merchant_operating_city_id, key, trip_category);
