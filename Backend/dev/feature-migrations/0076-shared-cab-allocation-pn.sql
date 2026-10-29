-- Push-notification template for a shared-cab allocation offer: the driver gets a shared-cab card, NOT the taxi allocation
-- request (dedicated fcm_notification_type SHARED_CAB_ALLOCATION). The shared-kernel enum must be in the deployed build
-- BEFORE this runs, else merchant_push_notification rows fail to parse. trip_category is left NULL so the
-- findMatchingMerchantPN fallback applies. {#seats#} / {#boardingCode#} are substituted at send time. ENGLISH only.

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'SHARED_CAB_ALLOCATION',
    'SHARED_CAB_ALLOCATION',
    moc.merchant_id,
    moc.id,
    'New shared-cab booking',
    '{#seats#} seat(s) booked on your shared cab. Open the route screen for boarding details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc
ON CONFLICT DO NOTHING;
