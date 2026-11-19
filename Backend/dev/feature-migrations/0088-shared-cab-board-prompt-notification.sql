-- Shared-cab rider push sent when the board countdown runs out: the rider is asked whether they boarded and has {#answerSec#} seconds
-- to answer before they are checked in. Key = SharedLogic.SharedCab.Notify.SharedCabNotificationType. ENGLISH only. Idempotent:
-- rows that exist are skipped. Params: {#boardStop#}, {#dropStop#}, {#vehicleNumber#}, {#vehicleLast4#}, {#answerSec#}. Seeded only
-- for cities with a shared-cab feed.
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
    'SHARED_CAB_BOARD_PROMPT',
    moc.merchant_id,
    moc.id,
    'Have you boarded your cab?',
    'Open the app and tell us if you are in cab {#vehicleLast4#}. If you do not answer in {#answerSec#} seconds we check you in.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.agency_key LIKE '%:SHARED_CAB'
)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'SHARED_CAB_BOARD_PROMPT'
      AND mpn.merchant_operating_city_id = moc.id
);
