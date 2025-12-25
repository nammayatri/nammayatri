UPDATE atlas_driver_offer_bpp.merchant_service_config SET config_json = (json_build_object(
    'defaultTtl', 600,
    'streamExpirationTime', 86400
    ))
where service_name = 'Notification_GRPC';