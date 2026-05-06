INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'PASS_RELATED',
    'PASS_EXPIRY_REMINDER',
    moc.merchant_id,
    moc.id,
    'Pass expiring in {#days#} days',
    'Your pass is expiring in {#days#} days. Renew now to continue enjoying benefits.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;
