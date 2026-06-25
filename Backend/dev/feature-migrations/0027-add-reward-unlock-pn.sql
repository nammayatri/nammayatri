-- Reward unlock notifications: separate keys for FCM and WhatsApp.
--   FCM:      REWARD_UNLOCK          (merchant_push_notification)
--   WhatsApp: WHATSAPP_REWARD_UNLOCK (merchant_message)
-- Placeholders: rewardTitle, sponsorName, couponCode

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'REWARD_UNLOCK',
    moc.merchant_id,
    moc.id,
    'You''ve unlocked {#rewardTitle#}!',
    'You''ve unlocked {#rewardTitle#} from {#sponsorName#}. Code: {#couponCode#}. Tap to use.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_message (
    merchant_id, merchant_operating_city_id, message_key, message, template_id, contains_url_button, created_at, updated_at
)
SELECT
    moc.merchant_id,
    moc.id,
    'WHATSAPP_REWARD_UNLOCK',
    'Reward unlocked. WhatsApp variables: rewardTitle, sponsorName, couponCode',
    '',
    false,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
