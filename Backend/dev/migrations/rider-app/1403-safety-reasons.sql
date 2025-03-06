-- SAFETY_ALERT_RIDE_STOPPAGE --
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'SAFETY_ALERT_RIDE_STOPPAGE',
    'SAFETY_ALERT_RIDE_STOPPAGE',
    moc.merchant_id,
    moc.id,
    'Everything okay?',
    'We noticed your driver has not moved for a while. Are you feeling safe on your trip?',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;

