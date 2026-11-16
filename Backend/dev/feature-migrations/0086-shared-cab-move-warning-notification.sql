-- Driver push (atlas_driver_offer_bpp): an accepted shared cab still standing away from the board stop is told to start moving
-- before the ride goes to another cab (rider-app awayWarnLeadSec before its AwayTimeout). ENGLISH only; idempotent.
-- TRIGGER_FCM so the driver app shows it as a plain notification. trip_category is left NULL so the findMatchingMerchantPN
-- fallback applies. Params, substituted at send time: {#boardStop#}, {#releaseInMin#}.
INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'SHARED_CAB_MOVE_WARNING',
    moc.merchant_id,
    moc.id,
    'Start moving to {#boardStop#}',
    'Your rider is waiting. Start moving within {#releaseInMin#} min or this ride will go to another cab.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
WHERE EXISTS (
    SELECT 1
    FROM atlas_driver_offer_bpp.merchant_push_notification mpn
    WHERE mpn.key = 'SHARED_CAB_ALLOCATION'
      AND mpn.merchant_operating_city_id = moc.id
)
ON CONFLICT DO NOTHING;
