INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (merchant_id, service_name, config_json, merchant_operating_city_id, created_at, updated_at)
SELECT
  moc.merchant_id,
  'ChallanSearch_Signzy',
  '{"url":"https://api-preproduction.signzy.app","apiKey":"<ENCRYPTED_SIGNZY_API_KEY>"}'::json,
  moc.id,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
ON CONFLICT (service_name, merchant_operating_city_id) DO NOTHING;
