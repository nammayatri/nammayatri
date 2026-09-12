-- Splits the single "BUS_APPROACHING" push-notification key (added in 0051) into two distance-tier
-- keys so the copy itself (not just {#distanceDisplay#}) can differ between the ~1km and ~300m
-- gps-processor thresholds.
--
-- Code side: Domain.Action.UI.FRFSTicketService.approachingNotificationKey picks the key from
-- req.thresholdType, mirroring gps-processor's TripVicinityStatus verbatim (not baking in a
-- distance number, since NotificationDistanceThresholds is configurable and can drift from the
-- current ~1000m/~300m values): "relaxed" -> "BUS_APPROACHING_RELAXED", "nearing" ->
-- "BUS_APPROACHING_NEARING".
--
-- Sound: both tiers keep notificationTypeForSound = Notification.BUS_APPROACHING (unchanged), so
-- they reuse the existing notification_sounds_config row from 0051 — no new sound row needed.
--
-- The old "BUS_APPROACHING" row from 0051 is left in place (now unused by code, but harmless) in
-- case of rollback.
--
-- Scope: ONLY merchant_operating_city_id = c7e3c3eb-cc15-46d4-ba04-5af55ac87874
-- (merchant_id = 4b17bd06-ae7e-48e9-85bf-282fb310209c).
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Push: bus approaching stop, "relaxed" threshold (~1km out)
------------------------------------------------------------------------------------------------------
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
    'BUS_APPROACHING_RELAXED',
    moc.merchant_id,
    moc.id,
    'Your bus {#routeDisplay#} is on the way',
    'It will reach your stop soon',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874'
  AND moc.merchant_id = '4b17bd06-ae7e-48e9-85bf-282fb310209c'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_APPROACHING_RELAXED'
      AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- Push: bus approaching stop, "nearing" threshold (~300m out)
------------------------------------------------------------------------------------------------------
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
    'BUS_APPROACHING_NEARING',
    moc.merchant_id,
    moc.id,
    'Your bus {#routeDisplay#} is almost here',
    'Get ready to board',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874'
  AND moc.merchant_id = '4b17bd06-ae7e-48e9-85bf-282fb310209c'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_APPROACHING_NEARING'
      AND mpn.merchant_operating_city_id = moc.id
);

UPDATE atlas_app.merchant_push_notification
SET
    title = 'Next stop: {#stopName#}',
    body = 'Bus {#routeDisplay#} crossed {#prevStopName#}',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'BUS_PREV_STOP_CROSSED'
  AND merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874'
  AND merchant_id = '4b17bd06-ae7e-48e9-85bf-282fb310209c';