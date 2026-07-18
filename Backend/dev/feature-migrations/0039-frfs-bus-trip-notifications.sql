-- FRFS shuttle notifications (push + WhatsApp) for two events, gated per city on
-- rider_config.bus_tracking_notification_tiers.
--
-- Scope: ONLY merchant_operating_city_id =
-- 62f14d5f-8dca-bfa1-a73e-c3b14267e637 (JATRI_SAATHI / Kolkata)
--
--   trip_tracking_enabled (id 8754637): body has NO variables; "Track Shuttle" URL button in template.
--   shuttle_booking_confirmation (id 8754638): var1=name, var2=source, var3=destination, var4=departure.
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Feature flag / tier gate (ONLY this city)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.rider_config
SET
    bus_tracking_notification_tiers = '{SHUTTLE}',
    updated_at = CURRENT_TIMESTAMP
WHERE merchant_operating_city_id = '62f14d5f-8dca-bfa1-a73e-c3b14267e637';

------------------------------------------------------------------------------------------------------
-- Push: trip started
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'BUS_TRIP_STARTED',
    moc.merchant_id,
    moc.id,
    'Your bus has started!',
    'Bus {#vehicleNumber#} on route {#routeName#} has started. Track your ride live in the app.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = '62f14d5f-8dca-bfa1-a73e-c3b14267e637'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_TRIP_STARTED' AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- Push: booking confirmed (shuttle only)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM',
    'SHUTTLE_TRACKING_ON_START',
    moc.merchant_id,
    moc.id,
    'Booking confirmed!',
    'Your shuttle booking for route {#routeName#} is confirmed. Live tracking will appear once your trip starts.',
    'ENGLISH',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = '62f14d5f-8dca-bfa1-a73e-c3b14267e637'
  AND NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'SHUTTLE_TRACKING_ON_START' AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- WhatsApp: trip started (template trip_tracking_enabled, id 8754637; body no vars, has URL button)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_message (
    merchant_id, merchant_operating_city_id, message_key, message, template_id, contains_url_button, created_at, updated_at
)
SELECT
    moc.merchant_id,
    moc.id,
    'WHATSAPP_BUS_TRIP_STARTED',
    'Your shuttle trip has started, and live tracking is now available. Have a safe journey!',
    '8754637',
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = '62f14d5f-8dca-bfa1-a73e-c3b14267e637'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

------------------------------------------------------------------------------------------------------
-- WhatsApp: booking confirmed (template shuttle_booking_confirmation, id 8754638)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_message (
    merchant_id, merchant_operating_city_id, message_key, message, template_id, contains_url_button, created_at, updated_at
)
SELECT
    moc.merchant_id,
    moc.id,
    'WHATSAPP_SHUTTLE_BOOKING_CONFIRMED',
    'Hello.\n\nYour booking for {#var1#} is confirmed.\n\nSource: {#var2#}\nDestination: {#var3#}\nScheduled departure: {#var4#}\n\nLive tracking will be available once your shuttle trip starts. We''ll send you another message as soon as tracking is enabled',
    '8754638',
    false,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE moc.id = '62f14d5f-8dca-bfa1-a73e-c3b14267e637'
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
