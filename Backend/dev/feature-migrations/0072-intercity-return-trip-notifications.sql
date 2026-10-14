INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'DRIVER_HAS_REACHED_DESTINATION',
    'DRIVER_HAS_REACHED_DESTINATION',
    moc.merchant_id,
    moc.id,
    'Driver reached destination!',
    'Your driver has reached the destination.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'DRIVER_STARTED_RETURN_TRIP',
    'DRIVER_STARTED_RETURN_TRIP',
    moc.merchant_id,
    moc.id,
    'Return trip started',
    'Your driver has started the journey back.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;
