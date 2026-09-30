-- Local Kaleyra SMS setup for testing with NAMMA_YATRI merchant
-- Replace only <YOUR_API_KEY> and <YOUR_SID> with real Kaleyra credentials
--
-- Usage:
--   psql -h localhost -p 5434 -U ritankarbose -d atlas_dev -f setup-local-kaleyra.sql

-- ============================================================
-- Rider App (atlas_app) — NAMMA_YATRI / Bangalore
-- ============================================================

-- Step 1: Insert Kaleyra service config
INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'Sms_KaleyraSms',
  json_build_object(
    'apiKey', '<YOUR_API_KEY>',
    'sid', '<YOUR_SID>',
    'url', 'https://api.in.kaleyra.io'
  )::jsonb,
  now(),
  now()
FROM atlas_app.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI'
  AND moc.city = 'Bangalore'
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
SET config_json = EXCLUDED.config_json, updated_at = now();

-- Step 2: Set KaleyraSms as the SMS provider
UPDATE atlas_app.merchant_service_usage_config
SET sms_providers_priority_list = '{KaleyraSms}',
    updated_at = now()
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_app.merchant_operating_city
  WHERE merchant_short_id = 'NAMMA_YATRI' AND city = 'Bangalore'
);

-- Step 3: Insert SEND_OTP message template
INSERT INTO atlas_app.merchant_message
  (merchant_id, merchant_operating_city_id, message_key, message, sender_header, template_id, message_type, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'SEND_OTP',
  '{#otp#} is your OTP for login to SmartRide App. {#hash#} - SmartRide',
  'SMARID',
  NULL,
  'TXN',
  now(),
  now()
FROM atlas_app.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI'
  AND moc.city = 'Bangalore'
ON CONFLICT (merchant_operating_city_id, message_key) DO UPDATE
SET sender_header = EXCLUDED.sender_header,
    message = EXCLUDED.message,
    template_id = EXCLUDED.template_id,
    message_type = EXCLUDED.message_type,
    updated_at = now();

-- ============================================================
-- Driver App (atlas_driver_offer_bpp) — NAMMA_YATRI_PARTNER / Bangalore
-- ============================================================

-- Step 1: Insert Kaleyra service config
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'Sms_KaleyraSms',
  json_build_object(
    'apiKey', '<YOUR_API_KEY>',
    'sid', '<YOUR_SID>',
    'url', 'https://api.in.kaleyra.io'
  )::jsonb,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Bangalore'
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
SET config_json = EXCLUDED.config_json, updated_at = now();

-- Step 2: Set KaleyraSms as the SMS provider
UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET sms_providers_priority_list = '{KaleyraSms}',
    updated_at = now()
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore'
);

-- Step 3: Insert SEND_OTP message template
INSERT INTO atlas_driver_offer_bpp.merchant_message
  (merchant_id, merchant_operating_city_id, message_key, message, sender_header, template_id, message_type, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'SEND_OTP',
  '{#otp#} is your OTP for login to SmartRide App. {#hash#} - SmartRide',
  'SMARID',
  NULL,
  'TXN',
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'NAMMA_YATRI_PARTNER'
  AND moc.city = 'Bangalore'
ON CONFLICT (merchant_operating_city_id, message_key) DO UPDATE
SET sender_header = EXCLUDED.sender_header,
    message = EXCLUDED.message,
    template_id = EXCLUDED.template_id,
    message_type = EXCLUDED.message_type,
    updated_at = now();

-- ============================================================
-- Verify
-- ============================================================

SELECT 'rider: merchant_service_config' as check, service_name, config_json
FROM atlas_app.merchant_service_config
WHERE service_name = 'Sms_KaleyraSms'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_app.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI' AND city = 'Bangalore'
  );

SELECT 'rider: merchant_service_usage_config' as check, sms_providers_priority_list
FROM atlas_app.merchant_service_usage_config
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_app.merchant_operating_city
  WHERE merchant_short_id = 'NAMMA_YATRI' AND city = 'Bangalore'
);

SELECT 'rider: merchant_message' as check, message_key, sender_header, message_type, message
FROM atlas_app.merchant_message
WHERE message_key = 'SEND_OTP'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_app.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI' AND city = 'Bangalore'
  );

SELECT 'driver: merchant_service_config' as check, service_name, config_json
FROM atlas_driver_offer_bpp.merchant_service_config
WHERE service_name = 'Sms_KaleyraSms'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore'
  );

SELECT 'driver: merchant_service_usage_config' as check, sms_providers_priority_list
FROM atlas_driver_offer_bpp.merchant_service_usage_config
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore'
);

SELECT 'driver: merchant_message' as check, message_key, sender_header, message_type, message
FROM atlas_driver_offer_bpp.merchant_message
WHERE message_key = 'SEND_OTP'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'NAMMA_YATRI_PARTNER' AND city = 'Bangalore'
  );
