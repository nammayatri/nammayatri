-- Seed an operation_hub row for NAMMA_YATRI_PARTNER / Bangalore so the
-- OperationHubFlow integration tests ("Get All Operation Hubs" etc.) have
-- at least one hub to return.
--
-- Moved here from Backend/dev/local-testing-data/dynamic-offer-driver-app.sql:
-- that file only runs on a *fresh* DB init (docker-entrypoint-initdb.d), so
-- on any already-existing local DB the seed row never got inserted and the
-- test kept failing with "At least one operation hub exists — expected 0 to
-- be above 0". Feature-migrations run on every config-sync import, so this
-- now applies to existing DBs too.

INSERT INTO atlas_driver_offer_bpp.operation_hub
  (id, name, address, lat, lon, mobile_number, merchant_id, merchant_operating_city_id, created_at, updated_at)
SELECT
  'test-hub-001',
  'Test Inspection Hub Bangalore',
  'HSR Layout, Bangalore',
  12.9141,
  77.6446,
  '9999900000',
  moc.merchant_id,
  moc.id,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Bangalore'
ON CONFLICT (id) DO NOTHING;
