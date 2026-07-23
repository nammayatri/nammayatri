-- FRFS shuttle notification updates (follow-up to 0039-frfs-bus-trip-notifications.sql).
--
-- Points the WhatsApp message rows at the approved YS-account (2000229802) template ids,
-- enables the URL-button flag (both templates carry a "Track"/"Navigate" button, so the
-- backend must send isTemplate=true via contains_url_button), and updates the
-- booking-confirm push body to read source -> destination at pickup time.
--
--   WHATSAPP_SHUTTLE_BOOKING_CONFIRMED -> template 8756354 (fin_shuttle_booking_confirmed):
--       5 vars [name, source, destination, departure, vehicle], URL button.
--   WHATSAPP_BUS_TRIP_STARTED          -> template 8754637 (tracking_live_shuttle):
--       0 vars, URL button.
--
-- Scoped by key / message_key only (no merchant_operating_city_id): the shuttle feature
-- currently runs in a single city and these keys are seeded only there. If other cities
-- adopt shuttle, their rows carry their own GupShup-account template ids and must be set
-- separately (template ids are per-account).
--
-- Idempotent: safe to re-run.

------------------------------------------------------------------------------------------------------
-- WhatsApp: booking confirmed (fin_shuttle_booking_confirmed, 5 vars + button)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.merchant_message
SET template_id = '8756354',
    contains_url_button = true,
    updated_at = CURRENT_TIMESTAMP
WHERE message_key = 'WHATSAPP_SHUTTLE_BOOKING_CONFIRMED';

------------------------------------------------------------------------------------------------------
-- WhatsApp: trip started (tracking_live_shuttle, 0 vars + button)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.merchant_message
SET template_id = '8754637',
    contains_url_button = true,
    updated_at = CURRENT_TIMESTAMP
WHERE message_key = 'WHATSAPP_BUS_TRIP_STARTED';

------------------------------------------------------------------------------------------------------
-- Push: booking confirmed body -> from {#source#} to {#destination#} at {#pickupTime#}
-- (matches the keys passed by notifyShuttleBookingConfirmed: source, destination, pickupTime)
------------------------------------------------------------------------------------------------------
UPDATE atlas_app.merchant_push_notification
SET body = 'Your Yatri Sathi Shuttle booking is confirmed from {#source#} to {#destination#} at {#pickupTime#}. We''ll notify you as soon as the bus starts.',
    updated_at = CURRENT_TIMESTAMP
WHERE key = 'SHUTTLE_TRACKING_ON_START';
