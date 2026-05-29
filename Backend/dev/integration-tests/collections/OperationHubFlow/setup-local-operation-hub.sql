-- Seed an operation hub for the OperationHubFlow integration tests
-- (NAMMA_YATRI_PARTNER / Bangalore)
--
-- Usage:
--   psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user -d atlas_dev -f setup-local-operation-hub.sql

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

-- Verify
SELECT 'operation_hub seed' as check, id, name, merchant_id, merchant_operating_city_id
FROM atlas_driver_offer_bpp.operation_hub
WHERE id = 'test-hub-001';
