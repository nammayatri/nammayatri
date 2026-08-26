-- FRFS bus "previous stop crossed" push notification, fired by gps-processor once per (trip, stop)
-- when the bus's stop-sequence watermark advances past a stop, via
-- POST /internal/frfs/trip/{tripId}/stop/{stopCode}/notifyApproaching with thresholdType="crossed"
--
-- {stopCode} in the URL is the NEXT upcoming stop (the passenger's booked fromStationCode, same
-- routing as BUS_APPROACHING); the crossed stop's name is resolved from the request body's
-- crossedStopId and substituted into {#prevStopName#}.
--
-- Tier-gated per city on rider_config.bus_approaching_notification_tiers (same whitelist reused
-- from 0047 — no separate tier gate for this event).
--
-- Sound: notification_sounds_config row for BUS_PREV_STOP_CROSSED was already added in 0047.
--
-- Scope: ONLY merchant_operating_city_id = de93a406-aa99-4db9-8691-2baa1258d4d0
-- (ANNA_APP / Chennai — same city as 0047).
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Push: bus crossed previous stop
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
    'BUS_PREV_STOP_CROSSED',
    moc.merchant_id,
    moc.id,
    'Your bus is on its way!',
    'Bus {#routeDisplay#} on route {#routeName#} just left {#prevStopName#} and is heading to {#stopName#}.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_PREV_STOP_CROSSED'
      AND mpn.merchant_operating_city_id = moc.id
);
