-- Shared-cab rider approach pushes between SHARED_CAB_ASSIGNED and SHARED_CAB_ARRIVING (sent by the allocation tick, once each per
-- allocation). Keys = SharedLogic.SharedCab.Notify.SharedCabNotificationType. ENGLISH only. Idempotent: rows that exist are skipped.
-- Params: {#boardStop#}, {#dropStop#}, {#vehicleNumber#}, {#vehicleLast4#}. Seeded only for cities with a shared-cab feed.
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
    ('SHARED_CAB_ON_THE_WAY', 'Your cab is on the way', 'Cab {#vehicleLast4#} is heading to {#boardStop#}.'),
    ('SHARED_CAB_ARRIVING_SOON', 'Your cab is arriving soon', 'Cab {#vehicleLast4#} is almost at {#boardStop#}. Please be ready to board.'),
    ('SHARED_CAB_PREV_STOP_CROSSED', 'Your cab has crossed the previous stop', 'Cab {#vehicleLast4#} is heading to {#boardStop#}. Please be ready to board.')
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
