INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    title,
    body,
    language,
    should_trigger,
    created_at,
    updated_at
)
SELECT
    'TRIGGER_FCM',
    'DRIVER_REACHING_DESTINATION',
    moc.merchant_id,
    moc.id,
    'Almost there!',
    'Please pay after the driver ends the ride.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
WHERE m.short_id = 'NAMMA_YATRI'
  AND moc.city = 'Bangalore'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'DRIVER_REACHING_DESTINATION'
      AND mpn.merchant_operating_city_id = moc.id
      AND mpn.language = 'ENGLISH'
);
