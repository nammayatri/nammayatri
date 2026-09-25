-- Shared-cab rider pushes (`07` B10), keys = SharedLogic.SharedCab.Notify.SharedCabNotificationType.
-- Params on every key: {#boardStop#}, {#dropStop#}, {#vehicleNumber#} (only when a cab is known).
--
-- Scope: cities that carry a shared-cab feed (integrated_bpp_config.agency_key `<feed>:SHARED_CAB`).
-- Idempotent: safe to re-run.

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
    ('SHARED_CAB_ASSIGNED', 'Your shared cab is on its way', 'Cab {#vehicleNumber#} will pick you up at {#boardStop#}.'),
    ('SHARED_CAB_ARRIVING', 'Your shared cab is here', 'Cab {#vehicleNumber#} is at {#boardStop#}. Enter the code inside the cab to board.'),
    ('SHARED_CAB_REASSIGNED', 'Finding you another cab', 'Your cab could not take you. We are finding another cab to {#dropStop#}.'),
    ('SHARED_CAB_BOARD_ANY', 'Board any shared cab', 'Board any shared cab to {#dropStop#} at {#boardStop#} and enter the code inside it.'),
    ('SHARED_CAB_ROUTE_CHANGE', 'Your cab is changing route', 'Cab {#vehicleNumber#} will not go on to {#dropStop#}. Get down at the next common stop.'),
    ('SHARED_CAB_DROP_CONFIRM', 'Did you get down?', 'Tap "I got down" once you are off the cab.')
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
