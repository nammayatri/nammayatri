UPDATE atlas_app.merchant_service_config
    SET config_json = config_json :: jsonb || '{"weightedSortCfg": {"arrivalTime": 0.5, "duration": 0.3, "transfers": 0.2}}' :: jsonb
WHERE service_name = 'MultiModal_OTPTransit';