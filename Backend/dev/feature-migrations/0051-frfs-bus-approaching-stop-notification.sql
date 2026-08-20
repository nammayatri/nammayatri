-- FRFS bus "approaching stop" push notifications, fired by gps-processor once per stop per
-- distance-threshold crossing, via
-- POST /internal/frfs/trip/{tripId}/stop/{stopCode}/notifyApproaching (see FRFSInternal +
-- Domain.Action.UI.FRFSTicketService.notifyBusApproachingStopForTrip).
--
-- A single notification key ("BUS_APPROACHING") is used regardless of which threshold fired —
-- the actual (rounded) distance from gps-processor is substituted into {#distanceDisplay#}
-- instead of baking "1 km"/"300 m" into separate per-threshold rows.
--
-- Tier-gated per city on rider_config.bus_approaching_notification_tiers (a dedicated whitelist,
-- separate from BUS_TRIP_STARTED's bus_tracking_notification_tiers).
--
-- Sound: own notification_sounds_config rows (BUS_APPROACHING, BUS_PREV_STOP_CROSSED) — same
-- "default" sound value used by every other row in this table, not a new sound asset.
--
-- Scope: ONLY merchant_operating_city_id = de93a406-aa99-4db9-8691-2baa1258d4d0
-- (ANNA_APP / Chennai — premium bus, not the shuttle city used by 0039/0042/0045).
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Feature flag / tier gate (ONLY this city)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.rider_config
SET
    bus_approaching_notification_tiers = '{PREMIUM}',
    updated_at = CURRENT_TIMESTAMP
WHERE merchant_operating_city_id = 'de93a406-aa99-4db9-8691-2baa1258d4d0';

------------------------------------------------------------------------------------------------------
-- Push: bus approaching stop (distance-agnostic; {#distanceDisplay#} carries the actual distance)
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
    'BUS_APPROACHING',
    moc.merchant_id,
    moc.id,
    'Your bus is on its way!',
    'Bus {#routeDisplay#} on route {#routeName#} is about {#distanceDisplay#} from {#stopName#}.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_APPROACHING'
      AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- Sound: BUS_APPROACHING (own row — "default", same value used by every other row in this table,
-- e.g. REGISTRATION_APPROVED)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.notification_sounds_config (
    blind_sound,
    default_sound,
    merchant_id,
    merchant_operating_city_id,
    notification_type,
    created_at,
    updated_at
)
SELECT
    'default',
    'default',
    moc.merchant_id,
    moc.id,
    'BUS_APPROACHING',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.notification_sounds_config nsc
    WHERE nsc.notification_type = 'BUS_APPROACHING'
      AND nsc.merchant_operating_city_id = moc.id
  );

------------------------------------------------------------------------------------------------------
-- Sound: BUS_PREV_STOP_CROSSED (own row — same "default" sound; category added in shared-kernel
-- for "bus crossed previous stop" flow)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.notification_sounds_config (
    blind_sound,
    default_sound,
    merchant_id,
    merchant_operating_city_id,
    notification_type,
    created_at,
    updated_at
)
SELECT
    'default',
    'default',
    moc.merchant_id,
    moc.id,
    'BUS_PREV_STOP_CROSSED',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = 'de93a406-aa99-4db9-8691-2baa1258d4d0'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.notification_sounds_config nsc
    WHERE nsc.notification_type = 'BUS_PREV_STOP_CROSSED'
      AND nsc.merchant_operating_city_id = moc.id
  );
