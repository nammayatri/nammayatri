-- Add Kaleyra SMS service config for MSIL_PARTNER
-- Replace <ENCRYPTED_API_KEY> and <ENCRYPTED_SID> with actual encrypted values

-- ============================================================
-- Rider App (atlas_app)
-- ============================================================

INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'Sms_KaleyraSms',
  json_build_object(
    'apiKey', '<ENCRYPTED_API_KEY>',
    'sid', '<ENCRYPTED_SID>',
    'url', 'https://api.in.kaleyra.io'
  )::jsonb,
  now(),
  now()
FROM atlas_app.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL'
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
SET config_json = EXCLUDED.config_json, updated_at = now();

UPDATE atlas_app.merchant_service_usage_config
SET sms_providers_priority_list = array_prepend('KaleyraSms', array_remove(sms_providers_priority_list, 'KaleyraSms')),
    updated_at = now()
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_app.merchant_operating_city
  WHERE merchant_short_id = 'MSIL'
);

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
WHERE moc.merchant_short_id = 'MSIL'
ON CONFLICT (merchant_operating_city_id, message_key) DO UPDATE
SET sender_header = EXCLUDED.sender_header,
    message = EXCLUDED.message,
    template_id = EXCLUDED.template_id,
    message_type = EXCLUDED.message_type,
    updated_at = now();

-- ============================================================
-- Driver App (atlas_driver_offer_bpp)
-- ============================================================

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  'Sms_KaleyraSms',
  json_build_object(
    'apiKey', '<ENCRYPTED_API_KEY>',
    'sid', '<ENCRYPTED_SID>',
    'url', 'https://api.in.kaleyra.io'
  )::jsonb,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
SET config_json = EXCLUDED.config_json, updated_at = now();

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET sms_providers_priority_list = array_prepend('KaleyraSms', array_remove(sms_providers_priority_list, 'KaleyraSms')),
    updated_at = now()
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

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
WHERE moc.merchant_short_id = 'MSIL_PARTNER'
ON CONFLICT (merchant_operating_city_id, message_key) DO UPDATE
SET sender_header = EXCLUDED.sender_header,
    message = EXCLUDED.message,
    template_id = EXCLUDED.template_id,
    message_type = EXCLUDED.message_type,
    updated_at = now();
