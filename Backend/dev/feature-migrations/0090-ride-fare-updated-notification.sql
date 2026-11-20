-- Collect payment: push to the rider when the driver asks for the fare mid-ride (on_update RIDE_FARE_UPDATED).
-- {#fare#} is the amount rounded like the rest of the app, without currency (INR: whole rupees).
-- Other placeholders: {#currency#}, {#driverName#}. No row for a city means no push.
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
    'RIDE_FARE_UPDATED',
    moc.merchant_id,
    moc.id,
    'Please pay your driver ₹{#fare#}',
    'You can view more details in the app',
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
    WHERE mpn.key = 'RIDE_FARE_UPDATED'
      AND mpn.merchant_operating_city_id = moc.id
      AND mpn.language = 'ENGLISH'
);
