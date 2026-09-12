INSERT INTO atlas_driver_offer_bpp.merchant_message
SELECT (json_populate_record(mm, json_build_object('message_key', new_key.message_key, 'created_at', now(), 'updated_at', now()))).*
FROM atlas_driver_offer_bpp.merchant_message mm
CROSS JOIN (VALUES ('DRIVER_ONBOARDING_DEEPLINK_MESSAGE'), ('FLEET_CONSENT_AND_ONBOARDING_DEEPLINK_MESSAGE')) AS new_key (message_key)
WHERE mm.message_key = 'FLEET_CONSENT_DEEPLINK_MESSAGE'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
