INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'EKD_LIVE_CALL_FEEDBACK',
    'EKD_LIVE_CALL_FEEDBACK',
    moc.merchant_id,
    moc.id,
    'Over Charging Incident Noted',
    'We promise to take action on the driver and ensure you get better drivers matched for your next trips.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;
