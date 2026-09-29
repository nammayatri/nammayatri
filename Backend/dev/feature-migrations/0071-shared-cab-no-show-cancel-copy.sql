-- R54: a rider's last allowed no-show cancels the booking (SharedLogic.SharedCab.Allocation.cancelForNoShows) and pushes
-- SHARED_CAB_BOOKING_CANCELLED with {#missedCabs#} (SharedLogic.SharedCab.Notify.notifyBookingCancelled).
-- ENGLISH only, like 0067; other languages' copy is theirs to sign off. Same city scope and guards as 0067. Idempotent.

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
    copy.key,
    moc.merchant_id,
    moc.id,
    copy.title,
    copy.body,
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (VALUES
    ('SHARED_CAB_BOOKING_CANCELLED', 'Your booking was cancelled', 'Your booking was cancelled after {#missedCabs#} missed cabs.')
) AS copy (key, title, body)
WHERE EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.agency_key LIKE '%:SHARED_CAB'
)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = copy.key
      AND mpn.merchant_operating_city_id = moc.id
);
