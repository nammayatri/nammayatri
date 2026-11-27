INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'SHARED_CAB_DEMAND_NUDGE',
    moc.merchant_id,
    moc.id,
    'Customers waiting',
    'Customers waiting on {#routeName#}: {#waiting#}, near {#stopName#}.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;
