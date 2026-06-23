-- Push-notification template for when the BPP accepts a service-tier change
-- and the booking data is updated (sent from the on_update handler).
-- trip_category is left NULL so it applies to all categories via the
-- findMatchingMerchantPN fallback. {#serviceTierName#} is substituted with the
-- new service tier name at send time.

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIP_UPDATED',
    'SERVICE_TIER_CHANGED',
    moc.merchant_id,
    moc.id,
    'Service tier changed',
    'Your ride is now confirmed as {#serviceTierName#}.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc
ON CONFLICT DO NOTHING;
