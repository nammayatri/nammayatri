INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'LiwipStore_LiwipStore',
  json_build_object(
    'url', 'https://dev-api.keystonecommerce.in/api/v1',
    'apiKey', '<PASSETTO_ENCRYPTED_LIWIP_API_KEY>'
  ),
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.state = 'Kerala'
ON CONFLICT (service_name, merchant_operating_city_id) DO NOTHING;
