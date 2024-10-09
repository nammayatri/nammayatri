INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
SELECT m.merchant_id, 'MultiModal_OTPTransit',
 '{"baseUrl": "https://api.sandbox.moving.tech/nandi/otp/gtfs/v1"}'
    , m.id
FROM atlas_app.merchant_operating_city m;

-- ONLY FOR MASTER AND PROD
ALTER TABLE atlas_app.journey ALTER COLUMN modes TYPE text[] USING modes::text[];