INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'DRIVER_ROUTE_DEVIATION',
    null,
    moc.merchant_id,
    moc.id,
    'Route Deviation Detected',
    'Your route seems to have deviated. Please follow the correct route.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, trip_category, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_SERVICE',
    'DRIVER_RIDE_STOPPAGE',
    null,
    moc.merchant_id,
    moc.id,
    'Vehicle Stopped',
    'Your vehicle appears to have stopped. Please continue the ride.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;
