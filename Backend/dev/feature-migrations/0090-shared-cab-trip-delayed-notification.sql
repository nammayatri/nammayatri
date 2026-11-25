-- Shared-cab rider "trip delayed" push (sent by the allocation tick, once per booking, timetabled feeds only): the cab's live ETA at
-- the board stop is tripDelayedThresholdSec or more past the booking's timetabled start_time.
-- Keys = SharedLogic.SharedCab.Notify.SharedCabNotificationType. ENGLISH only. Idempotent: rows that exist are skipped.
-- Params: {#boardStop#}, {#dropStop#}, {#vehicleNumber#}, {#vehicleLast4#}, {#delayMinutes#}, {#newEta#}. Seeded only for cities with a timetabled feed
-- (integrated_bpp_config.config_json contents.sharedKnobs.schedule = TIMETABLED), the only feeds the push is sent for.
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
    ('SHARED_CAB_TRIP_DELAYED', 'Your ride is running late', 'Cab {#vehicleLast4#} is about {#delayMinutes#} min behind schedule for {#boardStop#}, arriving around {#newEta#}.')
) AS copy (key, title, body)
WHERE EXISTS (
    SELECT 1
    FROM atlas_app.integrated_bpp_config ibc
    WHERE ibc.merchant_operating_city_id = moc.id
      AND ibc.config_json -> 'contents' -> 'sharedKnobs' ->> 'schedule' = 'TIMETABLED'
)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = copy.key
      AND mpn.merchant_operating_city_id = moc.id
);
