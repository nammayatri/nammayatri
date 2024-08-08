UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{googlePlaceNewUrl}',
    ('"https://places.googleapis.com/v1/"')::jsonb
)
WHERE service_name = 'Maps_Google';

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
    config_json::jsonb,
    '{useNewPlaces}',
    json('true')::jsonb
)::json
WHERE service_name = 'Maps_Google';