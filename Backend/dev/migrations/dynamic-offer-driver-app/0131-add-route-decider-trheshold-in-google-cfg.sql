UPDATE atlas_driver_offer_bpp.merchant_service_config
  SET config_json = config_json::jsonb || '{"routeDurationDeciderThreshold":300}'
  WHERE service_name = 'Maps_Google';