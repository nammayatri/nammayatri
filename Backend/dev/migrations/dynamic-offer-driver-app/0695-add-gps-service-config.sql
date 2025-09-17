-- Add GPS service configuration for external GPS device providers

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Gps_Hunteyed',
  json_build_object(
    'apiKey', 'default-hunteyed-api-key-change-me'
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m;