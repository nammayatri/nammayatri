-- FRFS shuttle notifications (push + WhatsApp) for two events, gated per city on
-- rider_config.bus_tracking_notification_tiers (the service tiers that get shuttle notifications).
--
--   1) Trip started (driver taps "start trip" on the tablet)
--        Push:     BUS_TRIP_STARTED            vars: vehicleNumber, routeName
--        WhatsApp: WHATSAPP_BUS_TRIP_STARTED   GupShup `trip_tracking_enabled` (id 8754637), no vars
--                  (tracking deep link is static, baked into the template).
--
--   2) Booking confirmed / payment fulfillment success (SHUTTLE ONLY; the generic
--      <service>_FULFILLMENT_SUCCESS push is suppressed for shuttle bookings in code)
--        Push:     SHUTTLE_TRACKING_ON_START            vars: routeName  (carries journeyId in entity)
--        WhatsApp: WHATSAPP_SHUTTLE_BOOKING_CONFIRMED   GupShup `shuttle_booking_confirmation` (id 8754638)
--                  vars: var1=name, var2=origin, var3=destination, var4=departure
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- Feature flag / tier gate: enable for SHUTTLE + PREMIUM in every city. Scope to a city instead with
--   ... WHERE merchant_operating_city_id = '<moc-id>'   (e.g. Kolkata) if you want a narrower rollout.
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.rider_config
SET bus_tracking_notification_tiers = '{SHUTTLE,PREMIUM}', updated_at = CURRENT_TIMESTAMP
WHERE bus_tracking_notification_tiers IS NULL;

------------------------------------------------------------------------------------------------------
-- Push: trip started
-- merchant_push_notification PK is `id` (auto-generated); no unique on (key, city), so guard with NOT EXISTS.
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM', 'BUS_TRIP_STARTED', moc.merchant_id, moc.id,
    'Your bus has started!',
    'Bus {#vehicleNumber#} on route {#routeName#} has started. Track your ride live in the app.',
    'ENGLISH', true, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'BUS_TRIP_STARTED' AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- Push: booking confirmed (shuttle-only reassurance)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'TRIGGER_FCM', 'SHUTTLE_TRACKING_ON_START', moc.merchant_id, moc.id,
    'Booking confirmed!',
    'Your shuttle booking for route {#routeName#} is confirmed. Live tracking will appear once your trip starts.',
    'ENGLISH', true, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
WHERE NOT EXISTS (
    SELECT 1 FROM atlas_app.merchant_push_notification mpn
    WHERE mpn.key = 'SHUTTLE_TRACKING_ON_START' AND mpn.merchant_operating_city_id = moc.id
);

------------------------------------------------------------------------------------------------------
-- WhatsApp: trip started (template `trip_tracking_enabled`, id 8754637)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_message (
    merchant_id, merchant_operating_city_id, message_key, message, template_id, contains_url_button, created_at, updated_at
)
SELECT
    moc.merchant_id, moc.id, 'WHATSAPP_BUS_TRIP_STARTED',
    'Shuttle trip started (template trip_tracking_enabled). Static deep link in template; no variables.',
    '8754637', false, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;

------------------------------------------------------------------------------------------------------
-- WhatsApp: booking confirmed (template `shuttle_booking_confirmation`, id 8754638)
------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.merchant_message (
    merchant_id, merchant_operating_city_id, message_key, message, template_id, contains_url_button, created_at, updated_at
)
SELECT
    moc.merchant_id, moc.id, 'WHATSAPP_SHUTTLE_BOOKING_CONFIRMED',
    'Shuttle booking confirmed (template shuttle_booking_confirmation). Vars: var1=name, var2=origin, var3=destination, var4=departure.',
    '8754638', false, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
