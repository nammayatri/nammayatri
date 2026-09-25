-- Push-notification template for a shared-cab allocation offer (shared_cab, task 4.6).
-- Fires when rider-app knocks POST /internal/sharedCabAllocationFCM (task 4.4): the driver
-- gets a shared-cab card, NOT the taxi allocation request. Dedicated key + dedicated
-- fcm_notification_type SHARED_CAB_ALLOCATION (added to shared-kernel on
-- feat/shared-cab-taxi-mode) so the client can render/notify per category.
--
-- Requires the shared-kernel SHARED_CAB_ALLOCATION enum to land in the deployed build
-- BEFORE this migration runs, else row reads of merchant_push_notification fail to parse.
-- trip_category is left NULL so the findMatchingMerchantPN fallback applies.
-- {#seats#} / {#boardingCode#} are substituted at send time (Tools/Notifications.hs
-- notifySharedCabAllocation). ENGLISH only; other languages once MeghOne signs them off.

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
