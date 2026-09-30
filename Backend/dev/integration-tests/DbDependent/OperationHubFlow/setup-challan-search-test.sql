-- Seed ChallanSearch_Signzy merchant service config for integration tests.
-- Points to the local mock server at localhost:8080/signzy.
--
-- Usage:
--   psql -h localhost -p 5434 -U atlas_driver_offer_bpp_user -d atlas_dev -f setup-challan-search-test.sql

-- Ensure operation hub exists (idempotent, same as setup-local-operation-hub.sql)
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

-- Insert ChallanSearch_Signzy merchant service config pointing to mock server.
-- The config_json matches SignzyChallanSearchCfg { url, apiKey }.
INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (merchant_id, service_name, config_json, merchant_operating_city_id, created_at, updated_at)
SELECT
  moc.merchant_id,
  'ChallanSearch_Signzy',
  '{"url":"http://localhost:8080/signzy","apiKey":"0.1.0|0|vz507BJYdc0xsVE+S7w3rinEFPEJtMfyDzbAKVGXpW9dIX7lamM9z5WQ7L4PqVBGbSK437ipdy4Ov0ewg6U="}'::json,
  moc.id,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Bangalore'
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
  SET config_json = EXCLUDED.config_json,
      updated_at = now();

-- Add Signzy to the challan provider priority list on the usage config
-- (getPendingChallanCount reads this; without it the call fails with
--  "No challan search provider configured").
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config muc
SET challan_providers_priority_list = '{Signzy}', updated_at = now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Bangalore'
  AND muc.merchant_operating_city_id = moc.id;

-- Verify
SELECT 'merchant_service_config seed' as check, service_name, merchant_operating_city_id
FROM atlas_driver_offer_bpp.merchant_service_config
WHERE service_name = 'ChallanSearch_Signzy';
